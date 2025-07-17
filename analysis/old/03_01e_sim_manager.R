## simulate a savvy manager who can make assignments based on observables only.
## measure savy using bubble sort idea.


## create bubble sort function, which takes in vector of values and a number of swaps.
## returns new vector in correct order.
x<-1:10
bubble_sort<-function(x, max_swaps){
  tot_swaps<-0
  while (tot_swaps<max_swaps){
    new_swaps<-0
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

source('load_packages.R')

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



### savvy: how many swaps allowed.
results<-data.table()
for (sim in 1:200){
  ## for each sim draw a random order by date.
  all_pairs[, rand:=runif(.N)]
  setorder(all_pairs, "analysis_workdate", "rand", "num_emp1")
  ## then allow different number of swaps.
  ## without mods, if we run through n*(n-1)/2 swaps we will sort.
  # can increase efficiency by breaking out.
  for (savy in 1:200){
    
    all_pairs[,new_list:= bubble_sort(safety_ability,savy), by="analysis_workdate"]
    
    all_pairs[,sim_prod:= (1:.N<=tot_ot_among)*new_list, by="analysis_workdate"]
    results<-rbind(results, data.table(savy_num=savy,sim_num=sim,prod=sum(all_pairs$sim_prod)))
  }
}
saveRDS(results, "analysis/data/03_01e_sim_manager.rds")
