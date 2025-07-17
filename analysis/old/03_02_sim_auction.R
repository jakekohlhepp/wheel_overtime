## simulate a k-wage auction, where officers bid for shifts each day.
## simulate a savvy m
library('data.table')
library('alpaca')
library('lubridate')
load("analysis/data/02_00_estimate.Rdata")

all_pairs<-readRDS("analysis/data/00_02_estimation_sample.rds")
officer_fe<-data.table(officer_fe=getFEs(mod_mod)$num_emp1, num_emp1=as.numeric(names(getFEs(mod_mod)$num_emp1)))
all_pairs<-merge(all_pairs, officer_fe, by="num_emp1", all.x=TRUE)

## exclude 7 officers without fixed effects
print(uniqueN(all_pairs[is.na(officer_fe)]$num_emp1)/uniqueN(all_pairs$num_emp1))
all_pairs<-all_pairs[!is.na(officer_fe),]

## deterministic portion of valuation is seniority rank part plus normal work plus person fe.
all_pairs[, det_val:=officer_fe+seniority_rank*coef(mod_mod)["seniority_rank"]+
          normal_work*coef(mod_mod)["normal_work"]]

all_pairs[, tot_ot_among:=sum(ot_work), by="analysis_workdate"]




### auction: draw logit errors.
results<-data.table()
for (iter in 1:2000){
    if (iter %% 50 ==0) print(iter)
    all_pairs[, valuation:=(det_val+rlogis(.N))/coef(mod_mod)["max_rate"]]
  
    setorder(all_pairs, "analysis_workdate", -"valuation")
    # for each date, take the first tot_ot_among based on available officers, partitions, then rand
    all_pairs[,sim_prod:= (1:.N<=tot_ot_among)*safety_ability, by="analysis_workdate"]
    all_pairs[,sim_prod_enforce:= (1:.N<=tot_ot_among)*enforcement_ability, by="analysis_workdate"]
    all_pairs[,sim_work:= (1:.N<=tot_ot_among), by="analysis_workdate"]
    byemp<-all_pairs[, .(ot_tot=sum(sim_work)), by="num_emp1"]
    setorder(byemp, "ot_tot","num_emp1")
    byemp[,position:= (1:.N)/.N ]
    byemp[,cum_ot:= cumsum(ot_tot)/sum(ot_tot)]
    byemp[, is_90th:= position>=0.9 & shift(position)<0.9]
    stopifnot(nrow(byemp[is_90th==1])==1)
    
    results<-rbind(results, data.table(sim_num=iter,safety_prod=sum(all_pairs$sim_prod),
                   enforcement_prod=sum(all_pairs$sim_prod_enforce),
                   share_top10=1-byemp[is_90th==1]$cum_ot[1]))
}
saveRDS(results, "analysis/data/03_02_sim_auction.rds")

## check
hist(results$safety_prod)
abline(v=sum(all_pairs$ot_work*all_pairs$safety_ability),col="blue")
  

