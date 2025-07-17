## simulate a savvy manager who can make assignments based on observables only.
source('load_packages.R')

set.seed(486623)
load("analysis/data/02_00_estimate.Rdata")

all_pairs<-readRDS("analysis/data/00_02_estimation_sample.rds")
officer_fe<-data.table(officer_fe=getFEs(mod_mod)$num_emp1, num_emp1=as.numeric(names(getFEs(mod_mod)$num_emp1)))
all_pairs<-merge(all_pairs, officer_fe, by="num_emp1", all.x=TRUE)

## exclude 7 officers without fixed effects
print(uniqueN(all_pairs[is.na(officer_fe)]$num_emp1)/uniqueN(all_pairs$num_emp1))
all_pairs<-all_pairs[!is.na(officer_fe),]

safety_prod<-unique(all_pairs[, c("num_emp1", "safety_ability")])
setkey(safety_prod, "safety_ability", "num_emp1")

### savvy: saviness of manager is defined as the number of partitions of the space
## 0 savy means random assignment.
results<-data.table()
for (savy in 1:nrow(safety_prod)){
  for (sim in 1:10){
    
    manager<-copy(safety_prod)
    if (savy>1) manager[, partitions:=cut(sort(unique(safety_ability) ), breaks = savy, labels = FALSE)]
    if (savy==1) manager[, partitions:=1]
    
    manager<-merge(all_pairs, manager[,-"safety_ability"], by="num_emp1", all.x=TRUE)
    # randomize across officer-days.
    manager[, rand:=runif(.N)]
    setorder(manager, "analysis_workdate", "partitions", "rand")
    # for each date, take the first tot_ot based on available officers, partitions, then rand
    manager[,sim_prod:= (1:.N<=tot_ot)*safety_ability, by="analysis_workdate"]
    results<-rbind(results, data.table(savy_num=savy,sim_num=sim,prod=sum(manager$sim_prod)))
  }
}
saveRDS(results, "analysis/data/03_01_sim_manager.rds")
