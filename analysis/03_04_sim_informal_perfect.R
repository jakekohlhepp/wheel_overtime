## simulate the original dgp, which is reduced form of informal trading.
## sort for exact alignment - most connections for the person who wants most.
## do for different sizes of the access costs.

library('data.table')
library('alpaca')
library('lubridate')
set.seed(387044)

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
max_iter<-2500

## utility is sum of all components including wages and degrees.
all_pairs[, det_util:=date_fe+officer_fe+
            seniority_rank*coef(mod_mod)["seniority_rank"]+
            normal_work*coef(mod_mod)["normal_work"]]

## reassign the least connections to those who most value overtime
setorder(all_pairs, "analysis_workdate",-"officer_fe", "num_emp1")
all_pairs[,mod_l_wheel_degree:=sort(l_wheel_degree, decreasing=TRUE) ,by="analysis_workdate"]
all_pairs[,mod_suppliers_interacted:= opp_dist*mod_l_wheel_degree]

## need to adjust det_util to keep expected number of ot the same.
findit<-function(x,dat,actual){
  return(sum(plogis(dat+x))-sum(actual))
}
results<-data.table()
results_byworker<-data.table()
for (mult in c(1/(2:10),1:10)){
  ## utility is sum of all components including wages and degrees.
  all_pairs[, det_util:=mult*(opp_dist*coef(mod_mod)["opp_dist"]+mod_suppliers_interacted*coef(mod_mod)["suppliers_interacted"])+date_fe+officer_fe+
              seniority_rank*coef(mod_mod)["seniority_rank"]+
              normal_work*coef(mod_mod)["normal_work"]+ot_rate*coef(mod_mod)["ot_rate"]]
  
  #### need to adjust intercept to maintain similar aggregate number of shifts.
  sol_adjust<-uniroot(findit, dat=all_pairs$det_util,actual=all_pairs$ot_work, interval=c(-20,20),tol=1e-12)
  stopifnot(round(sol_adjust$f.root,5)==0)
  sol_adjust<-sol_adjust$root

for (iter in 1:max_iter){
  if (iter %% 100 ==0) print(paste0("Multiplier: ", mult, " iteration: ",iter))
  
  ## those who work are just those with a positive utility
  all_pairs[, true_utility:=(det_util+rlogis(.N))]

  all_pairs[,sim_work:= true_utility+sol_adjust>0]
  all_pairs[, sim_win_wage:= ot_rate]
  
  ## non-wage utility delivered is then true utility but less wages, connections, and with max connectedness piece added.
  all_pairs[, true_valuation:=(true_utility-
              (ot_rate*coef(mod_mod)["ot_rate"] +mult*(opp_dist*coef(mod_mod)["opp_dist"]+mod_suppliers_interacted*coef(mod_mod)["suppliers_interacted"])  ))/(coef(mod_mod)["ot_rate"]) ]
  all_pairs[,true_utility:=(true_utility)/coef(mod_mod)["ot_rate"] ]
  all_pairs[,sim_prod:= sim_work*safety_ability, by="analysis_workdate"]
  all_pairs[,sim_value:= sim_work*true_valuation, by="analysis_workdate"]
  all_pairs[, sim_payment:=(sim_win_wage)*sim_work*all_othours/tot_ot_among]
  # worker surplus is then true utility (already includes wage)
  all_pairs[, worker_surplus:= sim_work*(true_utility+ot_rate) ]
  
  byemp<-all_pairs[, .(ot_tot=sum(sim_work)), by="num_emp1"]
  setorder(byemp, "ot_tot","num_emp1")
  byemp[,position:= (1:.N)/.N ]
  byemp[,cum_ot:= cumsum(ot_tot)/sum(ot_tot)]
  byemp[, is_90th:= position>=0.9 & shift(position)<0.9]
  stopifnot(nrow(byemp[is_90th==1])==1)
  
  results<-rbind(results, data.table(access_cost_mult=mult,sim_num=iter,safety_prod=sum(all_pairs$sim_prod),
                                     worker_value=sum(all_pairs$sim_value),
                                     worker_surplus=sum(all_pairs$worker_surplus),
                                     wage_bill=sum(all_pairs$sim_payment),
                                     share_top10=1-byemp[is_90th==1]$cum_ot[1]))
  results_byworker<-rbind(results_byworker,all_pairs[sim_work==1, .(access_cost_mult=mult,sim_num=iter,total_ot=sum(sim_work),worker_surplus=sum(worker_surplus),total_ot_pay=sum(sim_payment),max_win_wage=max(sim_win_wage), min_win_wage=min(sim_win_wage),
                                                                    avg_win_wage=mean(sim_win_wage) ), by="num_emp1"])
  
  
}
}
saveRDS(results, "analysis/data/03_04_sim_informal_perfect.rds")
saveRDS(results_byworker, "analysis/data/03_04_sim_informal_perfect_byworker.rds")

