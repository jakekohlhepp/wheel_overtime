## simulate a savvy manager who can make assignments based on observables only.
## measure savy using bubble sort idea.


## create bubble sort function, which takes in vector of values and a number of swaps.
## returns new vector in correct order.
bubble_sort<-function(x, max_swaps){
  tot_swaps<-0
  while (tot_swaps<max_swaps){
    new_swaps<-0
    if (length(x)==1) break
    for (i in 1:(length(x)-1)){
      if (x[i]>x[i+1]){
        new_swaps<-new_swaps+1
        
        x[c(i,i+1)]<-x[c(i+1,i)]
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
      if (x[i]>x[i+1]){
        new_swaps<-new_swaps+1
        
        x[c(i,i+1)]<-x[c(i+1,i)]
        nms[c(i,i+1)]<-nms[c(i+1,i)]
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
RNGkind("L'Ecuyer-CMRG")

set.seed(588923)
load("analysis/data/02_00_estimate.Rdata")

all_pairs<-readRDS("analysis/data/00_02_estimation_sample.rds")
officer_fe<-data.table(officer_fe=getFEs(mod_mod)$num_emp1, num_emp1=as.numeric(names(getFEs(mod_mod)$num_emp1)))
all_pairs<-merge(all_pairs, officer_fe, by="num_emp1", all.x=TRUE)

## exclude 7 officers without fixed effects
print(uniqueN(all_pairs[is.na(officer_fe)]$num_emp1)/uniqueN(all_pairs$num_emp1))
all_pairs<-all_pairs[!is.na(officer_fe),]

safety_prod<-unique(all_pairs[, c("num_emp1", "safety_ability")])
setkey(safety_prod, "safety_ability", "num_emp1")

all_pairs[, tot_ot_among:=sum(ot_work), by="analysis_workdate"]
setDTthreads(1)

### sequentially give swaps
do_sim<-function(x){
  ## for each sim draw a random order by date.
  hold<-copy(all_pairs)
  hold[, rand:=runif(.N)]
  setorder(hold, "analysis_workdate", "rand", "num_emp1")
  
  results<-data.table()
  
  ## loop through dates - do 50 swaps per date as max.
  ## purposefully use base r to avoid instability
  savy<-0
  for (looper in 1:80){
    for (d in unique(hold$analysis_workdate)){
      
      hold[analysis_workdate==d,safety_ability:= bubble_sort(safety_ability,5),]
      hold[analysis_workdate==d,num_emp1:= bubble_sort_name(safety_ability,num_emp1,5),]
      hold[,sim_prod:= (1:.N<=tot_ot_among)*safety_ability, by="analysis_workdate"]
      
      hold[,sim_work:= (1:.N<=tot_ot_among), by="analysis_workdate"]
      byemp<-hold[, .(ot_tot=sum(sim_work)), by="num_emp1"]
      setorder(byemp, "ot_tot","num_emp1")
      byemp[,position:= (1:.N)/.N ]
      byemp[,cum_ot:= cumsum(ot_tot)/sum(ot_tot)]
      byemp[, is_90th:= position>=0.9 & shift(position)<0.9]
      stopifnot(nrow(byemp[is_90th==1])==1)
      
      results<-rbind(results, data.table(savy_num=savy,sim_num=x,prod=sum(hold$sim_prod),
                                         share_top10=1-byemp[is_90th==1]$cum_ot[1]))
      savy<-savy+5
    }
    
  }
  return(results)
}

all_res<-data.table()
for (i in 1:50){
  temp_res<-data.table(do.call(rbind,mclapply((i*50-49):(i*50),do_sim, mc.cores=core_count)))
  all_res<-rbind(all_res, temp_res)
  saveRDS(all_res, "analysis/data/03_00_sim_manager_cluster.rds")

}
