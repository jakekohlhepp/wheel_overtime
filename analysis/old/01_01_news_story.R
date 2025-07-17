## check may 26, 2015

library('data.table')
library('lubridate')


all_pairs<-readRDS("analysis/data/00_02_estimation_sample.rds")
all_pairs[, tm:=floor_date(analysis_workdate, unit="month")]
all_pairs[, tw:=floor_date(analysis_workdate, unit="week")]
all_pairs[, junior:=seniority_rank>=250]


## keep only officers that do not join. recompute seniority among them
all_pairs[, first_date:=min(analysis_workdate), by="num_emp1"]
all_pairs[first_date<=as.Date('2015-05-01'),rank_mod:= frank(seniority_rank, ties.method="dense") , by="analysis_workdate"]
collapsed<-all_pairs[ot_work==1, .(sd_rank=sd(rank_mod),
                         count=.N), by=c("analysis_workdate","tm", "tw")]
plot(collapsed[analysis_workdate<as.Date('2015-02-28'),c("analysis_workdate", "sd_rank")])
plot(collapsed[analysis_workdate>=as.Date('2015-05-13') & analysis_workdate<=as.Date('2015-06-08'),c("analysis_workdate", "sd_rank")])


plot(collapsed[analysis_workdate<as.Date('2015-07-01'),.(avg_sd=mean(sd_rank, na.rm=TRUE)),by=c("tw")])
abline(v=as.Date('2015-06-30'), col="blue")
collapsed<-all_pairs[first_date<=as.Date('2015-06-01'), .(cor_degree=cor(l_degree, ot_work)), by=c("analysis_workdate","tm")]
plot(collapsed[,.(avg_sd=mean(cor_degree)),by=c("tm")])
abline(v=as.Date('2015-06-30'), col="blue")



collapsed<-all_pairs[first_date<=as.Date('2015-05-01'), .(cor_degree=cor(ot_work, l_degree),
                                   count=sum(ot_work)), by=c("analysis_workdate","tm", "tw")]
plot(collapsed[analysis_workdate>=as.Date('2015-05-20') & analysis_workdate<=as.Date('2015-06-01'),c("analysis_workdate", "cor_degree")])
plot(collapsed[,.(avg_sd=mean(cor_degree, na.rm=TRUE)),by=c("tm")])

