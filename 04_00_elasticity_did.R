## find all salary step increase and uses these for did

library('data.table')
library('stringr')
library('lubridate')

pay_recs<-fread('mkdata/20170803_payworkers_comp/anonymized_data_073117.txt')


pay_recs<-pay_recs[,.SD, .SDcols = colnames(pay_recs)[1:which(colnames(pay_recs)=="PAY_AMOUNT")]]
pay_recs[, dups:=.N>1, by=eval(colnames(pay_recs)[2:which(colnames(pay_recs)=="PAY_AMOUNT")])]
pay_recs[, dups:=NULL]
pay_recs[, V1:=NULL]
pay_recs<-unique(pay_recs)
colnames(pay_recs)<-str_to_lower(colnames(pay_recs))
pay_recs<-pay_recs[job_class_title=='TRAF OFFICER II']
pay_recs[, analysis_workdate:=ymd(work_date)]
## create payperiod dates - do every two weeks after first end date of pay period using pay period variable
pay_recs[,helper_end_date:=max(analysis_workdate), by=c("payroll_year", "pp")]

hold<-as.Date(sapply(0:100, function(x){return(14*x+min(pay_recs$helper_end_date))}))
stopifnot(weekdays(hold)=="Saturday")
hold<-data.table(end_date=hold, num_payperiod=1:length(hold))
hold[, analysis_workdate:=end_date]
pp_xwalk<-data.table()
for (i in 0:13){
  hold[,analysis_workdate:=end_date-i]
  pp_xwalk<-rbind(pp_xwalk,hold )
}
pay_recs<-merge(pay_recs,pp_xwalk, by="analysis_workdate", all.x=TRUE )
stopifnot(!is.na(pay_recs[analysis_workdate>=as.Date('2014-07-01')]$end_date))

## warning: pay periods only well defined for 2014-2015 fiscal eyar onwards.
pay_recs<-pay_recs[analysis_workdate>=as.Date('2014-07-01')]


## reattach years at job on work date
tenure<-fread('mkdata/20170803_payworkers_comp/anonymized_data_073117.txt')
tenure<-unique(tenure[,c("EMPLOYEE_NAME","WORK_DATE", "Years at Job on Workdate","ORIGINAL_HIRE_DATE" )])
stopifnot(uniqueN(tenure[,c("EMPLOYEE_NAME","WORK_DATE")])==nrow(tenure))
setnames(tenure, "WORK_DATE","work_date")
setnames(tenure, "EMPLOYEE_NAME","employee_name")
pay_recs<-merge(pay_recs, tenure, by=c("employee_name","work_date"), all.x=TRUE)

## create one record per pay period with pay rate.
## first pass: only current actual hours
base_rate_codes<-c("CURRENT ACTUAL HOURS WORKED ONLY","100% SICK TIME (CREDIT OR CHARGE)", "HOLIDAY HOURS (CREDIT OR CHARGE)","OVERTIME (1.5) WORKED AND PAID")
##base_rate_codes<-c("CURRENT ACTUAL HOURS WORKED ONLY")

pay_recs<-pay_recs[variation_description %in% base_rate_codes,]
pay_periods<-pay_recs[, .(max_rate=max(unique(var_rate) ), min_rate=(unique(var_rate))), by=c("end_date", "employee_name")]
setorder(pay_periods, "employee_name", "end_date")
pay_periods[, next_min:=shift(min_rate,type="lead"), by="employee_name"]
## all the within period conflicts are resolved by a change in the next period
pay_periods[!(abs(max_rate-min_rate)<=1e-05 | abs(next_min-max_rate)<=1e-05),]
## set base rate to be minimum in period
pay_periods[, base_rate:=max_rate]

## examine all changes
setorder(pay_periods, "employee_name", "end_date")
pay_periods[, delta_base:=base_rate-shift(base_rate), by="employee_name"]
pay_periods[, delta_base_forward:=shift(base_rate,type="lead")-base_rate, by="employee_name"]

pay_periods[, delta_time:=end_date-shift(end_date), by="employee_name"]
## step increases are positive, and current salary steps do not have increment under 0.5 and last period was 14 days prior.
pay_periods[, step_increase:=delta_base>0.5 & delta_time==14]
pay_periods[, num_emp1:=as.integer(gsub("EMPLOYEE ", "",employee_name))]

all_pairs<-readRDS("analysis/data/00_02_estimation_sample.rds")
all_pairs<-merge(all_pairs, pp_xwalk, by=c("analysis_workdate"), all.x=TRUE)
stopifnot(!is.na(all_pairs$end_date))

all_pairs[, avg_wheel:=mean(l_wheel_degree), by="num_emp1"]
all_pairs<-merge(all_pairs,pay_periods[,c("step_increase", "end_date", "num_emp1","delta_base")], by=c("end_date", "num_emp1"), all.x=TRUE )
all_pairs[is.na(step_increase), step_increase:=0]



for_did<-all_pairs[, .(tot_ot=sum(ot_work),step_increase=unique(step_increase),size_increase=unique(delta_base)) , by=c("avg_wheel","end_date","num_payperiod", "num_emp1")]




---
for_did<-all_pairs[, .(tot_ot=sum(ot_work),step_increase=unique(step_increase),size_increase=unique(delta_base)) , by=c("avg_wheel","end_date","num_payperiod", "num_emp1")]
  
### perform difference in difference
library('did')
library('ggplot2')
for_did[, when_treat:=min(ifelse(step_increase,num_payperiod,Inf)), by="num_emp1"]
for_did[is.infinite(when_treat), when_treat:=0]
example_attgt <- att_gt(yname = "tot_ot",
                        tname = "num_payperiod",
                        gname="when_treat",
                        idname = "num_emp1",
                        data = for_did
                        
)

agg.es <- aggte(example_attgt, type = "dynamic", na.rm=TRUE)
summary(agg.es)
ggdid(agg.es)+
  scale_x_continuous(limits = c(-5, 5))

agg.simple <- aggte(example_attgt, type = "simple", na.rm=TRUE)
summary(agg.simple)


example_attgt <- att_gt(yname = "tot_ot",
                        tname = "num_payperiod",
                        gname="when_treat",
                        idname = "num_emp1",
                        data = for_did[avg_wheel<=25]
                        
)

agg.simple <- aggte(example_attgt, type = "simple", na.rm=TRUE)
summary(agg.simple)

agg.es <- aggte(example_attgt, type = "dynamic", na.rm=TRUE)
summary(agg.es)
ggdid(agg.es)+
  scale_x_continuous(limits = c(-5, 5))


example_attgt <- att_gt(yname = "tot_ot",
                        tname = "num_payperiod",
                        gname="when_treat",
                        idname = "num_emp1",
                        data = for_did[avg_wheel>=35]
                        
)

agg.simple <- aggte(example_attgt, type = "simple", na.rm=TRUE)
summary(agg.simple)

agg.es <- aggte(example_attgt, type = "dynamic", na.rm=TRUE)
summary(agg.es)
ggdid(agg.es)+
  scale_x_continuous(limits = c(-5, 5))





---
example_attgt <- att_gt(yname = "tot_ot",
                        tname = "num_payperiod",
                        gname="when_treat",
                        idname = "num_emp1",
                        data = for_did[when_treat%in% c(25,22,30,0) & avg_wheel<30,]
                        
)

# summarize the results
summary(example_attgt)
agg.es <- aggte(example_attgt, type = "dynamic", na.rm=TRUE)
summary(agg.es)