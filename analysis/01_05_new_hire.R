library('fixest')
library('did2s')
library('data.table')
library('ggplot2')

## termination impact
all_pairs<-readRDS("analysis/data/00_02_estimation_sample.rds")

## find terminations
all_pairs[, first_time:=min(analysis_workdate), by="num_emp1"]
all_pairs[, is_hire:=first_time==analysis_workdate & first_time>=as.Date('2015-02-01')]
all_pairs[, does_hire:=max(is_hire), by="num_emp1"]


## impacted people are those who work with them on their first day
newhire_list<-copy(all_pairs[is_hire==1,])
col_picks<-colnames(all_pairs)[(which(colnames(all_pairs)=="num_emp1")+1):(which(colnames(all_pairs)=="tot_hours")-1)]
all_pairs[, treat:=0]
for (tt in 1:nrow(newhire_list)){
  mark_list<-as.numeric(newhire_list[tt, .SD, .SDcols=col_picks])
  names(mark_list)<-col_picks
  mark_list<-names(mark_list)[which(mark_list> 0)]
  date_focal<-newhire_list[tt,]$analysis_workdate
  all_pairs[, treat:= treat+((num_emp1 %in% as.numeric(mark_list)) & (analysis_workdate >= 1+date_focal ))]
}
# set treatment to be 1 at most.
all_pairs[treat>1, treat:=1]
all_pairs[treat==1, first_treat:= analysis_workdate]
all_pairs[, first_treat := ifelse(max(treat)==1, min(first_treat, na.rm=TRUE),Inf) , by ="num_emp1"]
all_pairs[, rel_time:= analysis_workdate - first_treat]


twfe = feols(l_degree ~ i(rel_time, ref=-c(1,Inf)) | num_emp1 + analysis_workdate, data = all_pairs[does_hire==0], cluster=~num_emp1) 
png("analysis/out/figures/01_05_new_hire_twfe.png", width=900, height=500)
iplot(twfe,main="",xlab="Time to Treatment (Days)",ylab="Connectedness", drop = "([3-9]\\d{1}|\\d{3})", lab.fit="simple")
dev.off()
