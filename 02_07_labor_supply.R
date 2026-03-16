## get dollar value of shift.


library('data.table')
library('alpaca')
library('lubridate')
set.seed(660062)

load("analysis/data/02_00_estimate.Rdata")

all_pairs<-readRDS("analysis/data/00_02_estimation_sample.rds")

### compute average ot per shift.
avg_ot_hours<-sum(all_pairs$varot_hours)/sum(all_pairs$ot_work)


officer_fe<-data.table(officer_fe=getFEs(mod_mod)$num_emp1, num_emp1=as.numeric(names(getFEs(mod_mod)$num_emp1)))
all_pairs<-merge(all_pairs, officer_fe, by="num_emp1", all.x=TRUE)
date_fe<-data.table(date_fe=getFEs(mod_mod)$analysis_workdate,analysis_workdate=as.Date(names(getFEs(mod_mod)$analysis_workdate)) )
all_pairs<-merge(all_pairs, date_fe, by="analysis_workdate", all.x=TRUE)


## exclude 7 officers without fixed effects
print(uniqueN(all_pairs[is.na(officer_fe)]$num_emp1)/uniqueN(all_pairs$num_emp1))
all_pairs<-all_pairs[!is.na(officer_fe),]

### for each date, compute the total ot hours and total ot instances.
## assign based on number of instances, assume even hours distribution
all_pairs[, all_othours:=sum(varot_hours), by="analysis_workdate"]

all_pairs[, tot_ot_among:=sum(ot_work), by="analysis_workdate"]

## for auctions, we can shut down access costs by making it as if all officers are being granted overtime via the wheel
## essentially setting distance from the wheel median to 0.
### auction 1: bids are deviations from base rate.

## deterministic portion of valuation is seniority rank part plus normal work plus person fe.
all_pairs[, det_val:=((date_fe+officer_fe+seniority_rank*coef(mod_mod)["seniority_rank"]+
            normal_work*coef(mod_mod)["normal_work"])/coef(mod_mod)["ot_rate"]+ot_rate)*avg_ot_hours ]


## has family is based on fml at any point.
all_pairs[, has_family:=max(family_leave, na.rm=TRUE), by="num_emp1"]

# make age on 01012015
all_pairs[, age_on_20150101:= an_age-(analysis_workdate-as.Date('2015-01-01'))/365.25, by="num_emp1"]

stopifnot(all_pairs[,.(check=uniqueN(round(age_on_20150101,digits=8))), by="num_emp1"]$check==1)
all_pairs[,age_on_20150101:=as.numeric(max(age_on_20150101)), by="num_emp1" ]




by_emp<-all_pairs[, .(observed_ot_count=sum(ot_work),valuation=mean(det_val),
                      avg_degree=mean(l_degree),safety_ability=unique(safety_ability),
                      avg_suppliers=mean(l_wheel_degree),barrier_avg=mean(coef(mod_mod)['opp_dist']*opp_dist +coef(mod_mod)['suppliers_interacted']*suppliers_interacted ),
                      avg_seniority=mean(seniority_rank)), by=c("num_emp1","has_family", "age_on_20150101")]


