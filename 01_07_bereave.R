library('fixest')
library('did2s')
library('data.table')
library('ggplot2')

## termination impact
all_pairs<-readRDS("analysis/data/00_02_estimation_sample.rds")
all_pairs[is.na(bereave), bereave:=0]
## find family leave
all_pairs[bereave==1, first_time:=min(analysis_workdate), by="num_emp1"]
all_pairs[bereave==1, is_leave:= first_time==analysis_workdate ]
all_pairs[bereave==0, is_leave:= 0 ]
all_pairs[, does_leave:=max(is_leave), by="num_emp1"]

## impacted people - mark as treated day after until end.
term_list<-copy(all_pairs[is_leave==1,])
col_picks<-colnames(all_pairs)[(which(colnames(all_pairs)=="0")):(which(colnames(all_pairs)=="665"))]
all_pairs[, treat:=0]
for (tt in 1:nrow(term_list)){
  mark_list<-as.numeric(term_list[tt, .SD, .SDcols=col_picks])
  names(mark_list)<-col_picks
  mark_list<-names(mark_list)[which(mark_list>=1)]
  date_focal<-term_list[tt,]$analysis_workdate
  all_pairs[, treat:= treat+((num_emp1 %in% as.numeric(mark_list)) & (analysis_workdate >= 1+date_focal ))]
}
# set treatment to be 1 at most.
all_pairs[treat>1, treat:=1]
all_pairs[treat==1, first_treat:= analysis_workdate]
all_pairs[, first_treat := ifelse(max(treat)==1, min(first_treat, na.rm=TRUE),Inf) , by ="num_emp1"]
all_pairs[, rel_time:= analysis_workdate - first_treat]



twfe = feols(l_degree ~ i(rel_time, ref=-c(1,Inf)) | num_emp1 + analysis_workdate, data = all_pairs[does_leave==0]) 
png("analysis/out/figures/01_07_bereave_twfe.png", width=900, height=500)
iplot(twfe,main="",xlab="Time to Treatment (Days)",ylab="Connectedness", drop = "([3-9]\\d{1}|\\d{3})", lab.fit="simple")
dev.off()

