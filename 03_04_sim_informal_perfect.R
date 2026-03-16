## sort for exact alignment - most connections for the person who wants most.
## do for different sizes of the access costs.

library('data.table')
library('alpaca')
library('lubridate')
set.seed(455723)

load("analysis/data/02_00_estimate.Rdata")

all_pairs<-readRDS("analysis/data/00_02_estimation_sample.rds")
officer_fe<-data.table(officer_fe=getFEs(mod_mod)$num_emp1, num_emp1=as.numeric(names(getFEs(mod_mod)$num_emp1)))
all_pairs<-merge(all_pairs, officer_fe, by="num_emp1", all.x=TRUE)
date_fe<-data.table(date_fe=getFEs(mod_mod)$analysis_workdate,analysis_workdate=as.Date(names(getFEs(mod_mod)$analysis_workdate)) )
all_pairs<-merge(all_pairs, date_fe, by="analysis_workdate", all.x=TRUE)


## exclude 7 officers without fixed effects
stopifnot(uniqueN(all_pairs[is.na(officer_fe)]$num_emp1)==7)
all_pairs<-all_pairs[!is.na(officer_fe),]

### for each date, compute the total ot hours and total ot instances.
## assign based on number of instances, assume even hours distribution
all_pairs[, all_othours:=sum(varot_hours), by="analysis_workdate"]

all_pairs[, tot_ot_among:=sum(ot_work), by="analysis_workdate"]

## set number of iterations
max_iter<-200


results<-data.table()
results_byworker<-data.table()
for (net in c(seq(from=0, to=20*coef(mod_mod)["suppliers_interacted"], length=20),coef(mod_mod)["suppliers_interacted"])  ){
  ## access cost.
  for (cost in c(seq(from=0, to=20*coef(mod_mod)["opp_dist"], length=20),coef(mod_mod)["opp_dist"])  ){
    
    for (iter in 1:max_iter){
      if (iter %% 50 ==0) print(paste0("Access Cost: ", round(cost,2),"Network Reduction: ", round(net,2), " iteration: ",iter))
      all_pairs[, det_util:=date_fe+officer_fe+seniority_rank*coef(mod_mod)["seniority_rank"]+
                  normal_work*coef(mod_mod)["normal_work"]]
      ## draw logit shocks
      all_pairs[, true_utility:=(det_util+rlogis(.N))]
      
      setorder(all_pairs, "analysis_workdate",-"true_utility", "num_emp1")
      ## reassign the least connections to those who most value overtime
      all_pairs[,mod_l_wheel_degree:=sort(l_wheel_degree, decreasing=TRUE) ,by="analysis_workdate"]
      all_pairs[,mod_suppliers_interacted:= opp_dist*mod_l_wheel_degree]
      
      ## those who work are top based on full index
      all_pairs[, true_utility:= true_utility+ot_rate*coef(mod_mod)["ot_rate"]+opp_dist*cost+mod_suppliers_interacted*net ]
      all_pairs[,sim_work:=frank(-true_utility, ties.method = "random")<=tot_ot_among , by="analysis_workdate"]
      all_pairs[, sim_win_wage:= ot_rate]
      
      ## non-wage utility delivered is then true utility but less wages, connections, and with max connectedness piece added.
      all_pairs[, true_valuation:=(true_utility-
                                     (ot_rate*coef(mod_mod)["ot_rate"] +opp_dist*cost+mod_suppliers_interacted*net  ))/(coef(mod_mod)["ot_rate"]) ]
      all_pairs[,true_utility:=(true_utility)/coef(mod_mod)["ot_rate"] ]
      all_pairs[,sim_prod:= sim_work*safety_ability, by="analysis_workdate"]
      all_pairs[,sim_value:= sim_work*true_valuation, by="analysis_workdate"]
      all_pairs[, sim_payment:=(sim_win_wage)*sim_work*all_othours/tot_ot_among]
      # worker surplus is then true utility plus wages
      all_pairs[, worker_surplus:= sim_work*(true_utility+ot_rate) ]
      
      byemp<-all_pairs[, .(ot_tot=sum(sim_work)), by="num_emp1"]
      setorder(byemp, "ot_tot","num_emp1")
      byemp[,position:= (1:.N)/.N ]
      byemp[,cum_ot:= cumsum(ot_tot)/sum(ot_tot)]
      byemp[, is_90th:= position>=0.9 & shift(position)<0.9]
      stopifnot(nrow(byemp[is_90th==1])==1)
      
      results<-rbind(results, data.table(network_reduction=net,access_cost=cost,sim_num=iter,safety_prod=sum(all_pairs$sim_prod),
                                         worker_value=sum(all_pairs$sim_value),
                                         worker_surplus=sum(all_pairs$worker_surplus),
                                         wage_bill=sum(all_pairs$sim_payment),
                                         share_top10=1-byemp[is_90th==1]$cum_ot[1]))
      results_byworker<-rbind(results_byworker,all_pairs[sim_work==1, .(network_reduction=net,access_cost=cost,sim_num=iter,total_ot=sum(sim_work),worker_surplus=sum(worker_surplus),total_ot_pay=sum(sim_payment),max_win_wage=max(sim_win_wage), min_win_wage=min(sim_win_wage),
                                                                        avg_win_wage=mean(sim_win_wage) ), by="num_emp1"])
      
      
    }
  }
}
saveRDS(results, "analysis/data/03_04_sim_informal_perfect.rds")
saveRDS(results_byworker, "analysis/data/03_04_sim_informal_perfect_byworker.rds")

