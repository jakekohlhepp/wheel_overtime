library('data.table')
library('lubridate')
library('stringr')
library('almanac')

## subset the data for estimation.
all_pairs<-readRDS('mkdata/data/01_06_panel_working.rds')

## first limit so that after each injury we do not include days until the first day where you work
setkey(all_pairs, "num_emp1", "analysis_workdate")
all_pairs[,inj_stint:= cumsum(matched_injury), by="num_emp1"]
all_pairs[(normal_work==1 | ot_work==1) & matched_injury==0 & inj_stint>0,first_work:= min(analysis_workdate), by=c("num_emp1","inj_stint")]
stopifnot(all_pairs[matched_injury==1,]$ot_work==1 |all_pairs[matched_injury==1,]$normal_work==1 )
all_pairs[inj_stint>0,first_work:= min(first_work, na.rm=TRUE), by=c("num_emp1","inj_stint")]
all_pairs[inj_stint==0,first_work:=min(analysis_workdate), by=c("num_emp1","inj_stint")]
all_pairs<-all_pairs[analysis_workdate>=first_work | matched_injury==1, ]
all_pairs<-all_pairs[ analysis_workdate>=as.Date('2015-01-01') & analysis_workdate<=as.Date('2016-06-30'),]


## attach abilities
all_pairs<-merge(all_pairs,readRDS('analysis/data/00_01_individual_productivity.rds'), by="num_emp1", all.x=TRUE )

## most people missing abilities did not work any overtime
all_pairs[, check_ot:=sum(ot_work), by="num_emp1"]
print(table(unique(all_pairs[is.na(safety_ability), c("num_emp1","check_ot")])$check_ot))
all_pairs[, check_ot:=NULL]
all_pairs<-all_pairs[!is.na(safety_ability),]
## rationalize as voluntary: cannot force people who do not want on the wheel.
### make holiday
holidays<-data.table(cal_events(cal_us_federal(), year = c(2015,2016)))
setnames(holidays, "name", "holiday")
setnames(holidays, "date", "analysis_workdate")
holidays[, is_holiday:=1]
holidays[,analysis_workdate:=ymd(analysis_workdate) ]
all_pairs<-merge(all_pairs, holidays, all.x=TRUE, by="analysis_workdate")
all_pairs[is.na(is_holiday), is_holiday:=0]

## make vars.
all_pairs[,bar_degree:=mean(l_degree, na.rm=TRUE), by="num_emp1"]
all_pairs[, f_dw:=as.factor(dw)]
all_pairs[, f_month:=as.factor(month)]
all_pairs[, ot_work:=as.numeric(ot_work)]
all_pairs[, normal_work:=as.numeric(normal_work)]
all_pairs<-cbind(all_pairs, model.matrix( ~ f_month - 1, data=all_pairs ), model.matrix( ~ f_dw - 1, data=all_pairs ))
all_pairs[, opp_dist:=1-dist_from_med]
all_pairs[, suppliers_interacted:=opp_dist*l_wheel_degree]
all_pairs[, ot_rate:=max_rate*1.5]

## add bereavement and fml flags
## get the raw data - get bereave and fml
raw_data<-fread('mkdata/20170803_payworkers_comp/anonymized_data_073117.txt')
injury<-unique(raw_data[`Med Pd`>0, c("Med Pd", "EMPLOYEE_NAME", "DOI")])
stopifnot(nrow(injury)==uniqueN(injury[,-"Med Pd"]))
injury[, analysis_workdate:=ymd(DOI)]
injury[, injured:=1]
## View(raw_data[, .(count=.N), by="VARIATION_DESCRIPTION"])
## flag 
raw_data[, family_leave:=str_detect(VARIATION_DESCRIPTION, "FML") |str_detect(VARIATION_DESCRIPTION, "FAMILY") ]
raw_data[, bereave:=str_detect(VARIATION_DESCRIPTION, "BEREAVEMENT") ]
raw_data<-raw_data[,.(family_leave=max(family_leave), bereave=max(bereave)), by=c("EMPLOYEE_NAME", "WORK_DATE")]
raw_data[, analysis_workdate:=ymd(WORK_DATE)]
raw_data[, num_emp1:=as.integer(gsub("EMPLOYEE ", "",EMPLOYEE_NAME))]
raw_data<-merge(raw_data, injury[, c("EMPLOYEE_NAME", "analysis_workdate", "injured")], all.x=TRUE, by=c("EMPLOYEE_NAME", "analysis_workdate"))
all_pairs<-merge(all_pairs, raw_data[,c("num_emp1","analysis_workdate","bereave","family_leave", "injured")], by=c("num_emp1","analysis_workdate"), all.x=TRUE)

setorder(all_pairs, "num_emp1", "analysis_workdate")
saveRDS(all_pairs, "analysis/data/00_02_estimation_sample.rds")



