## trace out the inequality-allocative efficiency frontier
## do this by starting at random allocation and then allow an artificial manager to make swaps to improve allocations


## create bubble sort function, which takes in vector of values and a number of swaps.
## returns new vector in correct order.
## this bubble sorts towards higher values at the top.
bubble_sort<-function(x, max_swaps){
  tot_swaps<-0
  while (tot_swaps<max_swaps){
    new_swaps<-0
    if (length(x)==1) break
    for (i in 1:(length(x)-1)){
      if (x[i]<x[i+1]){
        new_swaps<-new_swaps+1
        
        x[c(i+1,i)]<-x[c(i,i+1)]
        if (tot_swaps+new_swaps ==max_swaps) break
      }
    }
    tot_swaps<-tot_swaps+new_swaps
    if (new_swaps==0) break
    
    
  }
  return(x)
  
}

bubble_sort_name<-function(x,nms ,max_swaps){
  tot_swaps<-0
  while (tot_swaps<max_swaps){
    new_swaps<-0
    if (length(x)==1) break
    for (i in 1:(length(x)-1)){
      if (x[i]<x[i+1]){
        new_swaps<-new_swaps+1
        
        x[c(i+1,i)]<-x[c(i,i+1)]
        nms[c(i+1,i)]<-nms[c(i,i+1)]
        if (tot_swaps+new_swaps ==max_swaps) break
      }
    }
    tot_swaps<-tot_swaps+new_swaps
    if (new_swaps==0) break
    
    
  }
  return(nms)
  
}


core_count<-50

library('alpaca')
library('data.table')
library('parallel')
library('lubridate')
library('stats')
RNGkind("L'Ecuyer-CMRG")

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
## deterministic portion of valuation is seniority rank part plus normal work plus person fe.
all_pairs[, det_val:=date_fe+officer_fe+seniority_rank*coef(mod_mod)["seniority_rank"]+
            normal_work*coef(mod_mod)["normal_work"]]


setDTthreads(1)

### sequentially give swaps
do_sim<-function(x){
  ## for each sim draw random order.
  ## also draw logistic shocks.
  hold<-copy(all_pairs)
  hold[, rand:=runif(.N)]
  hold[, true_valuation:=(det_val+rlogis(.N))/(coef(mod_mod)["ot_rate"])]
  setorder(hold, "analysis_workdate", "rand", "num_emp1")
  
  results<-data.table()

  ## loop through dates
  savy<-0
  swap_count<-80
  date_count<-uniqueN(hold$analysis_workdate)
  for (looper in 1:6000){
    if (savy==0){
      hold[,sim_value:= (1:.N<=tot_ot_among)*true_valuation, by="analysis_workdate"]
      hold[,sim_work:= (1:.N<=tot_ot_among), by="analysis_workdate"]
      byemp<-hold[, .(ot_tot=sum(sim_work)), by="num_emp1"]
    } else{
      hold[,true_valuation:= bubble_sort(true_valuation,swap_count), by="analysis_workdate"]
      hold[,num_emp1:= bubble_sort_name(true_valuation,num_emp1,swap_count), by="analysis_workdate"]
      hold[,sim_value:= (1:.N<=tot_ot_among)*true_valuation, by="analysis_workdate"]
      hold[,sim_work:= (1:.N<=tot_ot_among), by="analysis_workdate"]
      byemp<-hold[, .(ot_tot=sum(sim_work)), by="num_emp1"]
    }
      
      setorder(byemp, "ot_tot","num_emp1")
      byemp[,position:= (1:.N)/.N ]
      byemp[,cum_ot:= cumsum(ot_tot)/sum(ot_tot)]
      byemp[, is_90th:= position>=0.9 & shift(position)<0.9]
      stopifnot(nrow(byemp[is_90th==1])==1)
      
      results<-rbind(results, data.table(savy_num=savy,sim_num=x,worker_value=sum(hold$sim_value),
                                         share_top10=1-byemp[is_90th==1]$cum_ot[1]))
      savy<-savy+swap_count*date_count

  }
  return(results)
}


#all_res<-data.table()
#for (i in 1:50){
#  temp_res<-data.table(do.call(rbind,mclapply((i*50-49):(i*50),do_sim, mc.cores=core_count)))
#  all_res<-rbind(all_res, temp_res)
#  saveRDS(all_res, "analysis/data/03_00_sim_frontier_cluster.rds")
#  print(paste0("Completed", i, "runs"))
#}

temp_res<-data.table(do.call(rbind,mclapply(1:2500,do_sim, mc.cores=core_count)))
saveRDS(temp_res, "analysis/data/03_00_sim_frontier_cluster.rds")
