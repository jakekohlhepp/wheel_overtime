library('fixest')
library('did2s')
library('data.table')
library('ggplot2')

## termination impact on own connectedness
all_pairs<-readRDS("analysis/data/00_02_estimation_sample.rds")
all_pairs[is.na(family_leave), family_leave:=0]
## find family leave
all_pairs[family_leave==1, first_time:=min(analysis_workdate), by="num_emp1"]
all_pairs[family_leave==1, is_leave:= first_time==analysis_workdate ]
all_pairs[family_leave==0, is_leave:= 0 ]
all_pairs[, does_leave:=max(is_leave), by="num_emp1"]

# set treatment to be 1 at most.
all_pairs[, treat:= 0]
all_pairs[is_leave==1, treat:=1]
all_pairs[treat==1, first_treat:= analysis_workdate]
all_pairs[, first_treat := ifelse(max(treat)==1, min(first_treat, na.rm=TRUE),Inf) , by ="num_emp1"]
all_pairs[, rel_time:= analysis_workdate - first_treat]

twfe = feols(wheel_degree  ~  treat| num_emp1 + analysis_workdate, data = all_pairs) 
summary(twfe)


twfe = feols(wheel_degree  ~ i(rel_time, ref=-c(0,Inf)) | num_emp1 + analysis_workdate, data = all_pairs) 
png("analysis/out/figures/01_06b_own_fmla.png", width=900, height=500)
iplot(twfe,main="",xlab="Time to Family Medical Leave (Days)",ylab="Connectedness", drop = "([3-9]\\d{1}|\\d{3})", lab.fit="simple")
dev.off()

# unconditional with treatment starting the day prior
all_pairs[, first_treat:=NULL]
all_pairs[, treat:= 0]
all_pairs[, treat:=rel_time==-1]
all_pairs[treat==1, first_treat:= analysis_workdate]
all_pairs[, first_treat := ifelse(max(treat)==1, min(first_treat, na.rm=TRUE),Inf) , by ="num_emp1"]

twfe = feols(wheel_degree  ~  treat| num_emp1 + analysis_workdate, data = all_pairs) 
summary(twfe)


