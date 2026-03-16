## randomly assign ot.
library('alpaca')
library('data.table')
library('parallel')
library('lubridate')
library('stats')

set.seed(477812)
load("analysis/data/02_00_estimate.Rdata")

all_pairs<-readRDS("analysis/data/00_02_estimation_sample.rds")
officer_fe<-data.table(officer_fe=getFEs(mod_mod)$num_emp1, num_emp1=as.numeric(names(getFEs(mod_mod)$num_emp1)))
all_pairs<-merge(all_pairs, officer_fe, by="num_emp1", all.x=TRUE)
date_fe<-data.table(date_fe=getFEs(mod_mod)$analysis_workdate,analysis_workdate=as.Date(names(getFEs(mod_mod)$analysis_workdate)) )
all_pairs<-merge(all_pairs, date_fe, by="analysis_workdate", all.x=TRUE)

## exclude 7 officers without fixed effects
print(uniqueN(all_pairs[is.na(officer_fe)]$num_emp1)/uniqueN(all_pairs$num_emp1))
all_pairs<-all_pairs[!is.na(officer_fe),]

all_pairs[, tot_ot_among:=sum(ot_work), by="analysis_workdate"]
## utility is sum of all components including wages and degrees.
all_pairs[, det_util:=opp_dist*coef(mod_mod)["opp_dist"]+suppliers_interacted*coef(mod_mod)["suppliers_interacted"]+date_fe+officer_fe+
            seniority_rank*coef(mod_mod)["seniority_rank"]+
            normal_work*coef(mod_mod)["normal_work"]+ot_rate*coef(mod_mod)["ot_rate"]]

### for each date, compute the total ot hours and total ot instances.
## assign based on number of instances, assume even hours distribution
all_pairs[, all_othours:=sum(varot_hours), by="analysis_workdate"]

## total ot on each date
all_pairs[, tot_ot_among:=sum(ot_work), by="analysis_workdate"]
setkey(all_pairs, "analysis_workdate", "num_emp1")
max_iter<-2500
results<-data.table()
results_byworker<-data.table()
for (iter in 1:max_iter){
  if (iter %% 100 ==0) print(paste0(" iteration: ",iter))
  
  ## those who work are just those randomly with the highest value
  all_pairs[, rand_assign:=runif(.N)]
  all_pairs[,sim_work:= frank(rand_assign, ties.method = "random")<=tot_ot_among, by="analysis_workdate"]
  
  stopifnot(sum(all_pairs$sim_work)==sum(all_pairs$ot_work))
  
  ## draw logit shock
  all_pairs[, true_utility:=det_util+rlogis(.N)]
  all_pairs[, sim_win_wage:= ot_rate]
  
  ## non-wage utility delivered is then true utility but less wages, connections, and with max connectedness piece added.
  all_pairs[, true_valuation:=(true_utility - 
                                 (ot_rate*coef(mod_mod)["ot_rate"] +opp_dist*coef(mod_mod)["opp_dist"]+suppliers_interacted*coef(mod_mod)["suppliers_interacted"] ))/(coef(mod_mod)["ot_rate"]) ]
  
  all_pairs[,sim_prod:= sim_work*safety_ability, by="analysis_workdate"]
  all_pairs[,sim_value:= sim_work*true_valuation, by="analysis_workdate"]
  all_pairs[, sim_payment:=(sim_win_wage)*sim_work*all_othours/tot_ot_among]
  # worker surplus is then true utility (already includes wage)
  all_pairs[, worker_surplus:= sim_work*(true_utility/(coef(mod_mod)["ot_rate"]) ) ]
  
  byemp<-all_pairs[, .(ot_tot=sum(sim_work)), by="num_emp1"]
  setorder(byemp, "ot_tot","num_emp1")
  byemp[,position:= (1:.N)/.N ]
  byemp[,cum_ot:= cumsum(ot_tot)/sum(ot_tot)]
  byemp[, is_90th:= position>=0.9 & shift(position)<0.9]
  stopifnot(nrow(byemp[is_90th==1])==1)
  
  results<-rbind(results, data.table(sim_num=iter,safety_prod=sum(all_pairs$sim_prod),
                                     worker_value=sum(all_pairs$sim_value),
                                     worker_surplus=sum(all_pairs$worker_surplus),
                                     wage_bill=sum(all_pairs$sim_payment),
                                     share_top10=1-byemp[is_90th==1]$cum_ot[1]))
  results_byworker<-rbind(results_byworker,all_pairs[sim_work==1, .(sim_num=iter,total_ot=sum(sim_work),worker_surplus=sum(worker_surplus),total_ot_pay=sum(sim_payment),max_win_wage=max(sim_win_wage), min_win_wage=min(sim_win_wage),
                                                                    avg_win_wage=mean(sim_win_wage) ), by="num_emp1"])
  
  
}
saveRDS(results, "analysis/data/03_00_sim_random.rds")
saveRDS(results_byworker, "analysis/data/03_00_sim_random_byworker.rds")




