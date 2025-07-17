## simulate a k-wage auction, where officers bid for shifts each day.
## add score, where we adjust bids by a score.
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

## attach the ot hours
raw_data<-fread('mkdata/data/01_03_pre_network_90.csv')
raw_data[, analysis_workdate:=dmy(analysis_workdate)]
raw_data[, num_emp1:=as.integer(gsub("EMPLOYEE ", "",employee_name))]

all_pairs<-merge(all_pairs,raw_data[,c("num_emp1", "analysis_workdate", "varot_hours")], by=c("num_emp1", "analysis_workdate"), all.x=TRUE )
rm(raw_data)

### for each date, compute the total ot hours
all_pairs[, all_othours:=sum(varot_hours), by="analysis_workdate"]

## deterministic portion of valuation is seniority rank part plus normal work plus person fe.
all_pairs[, det_val:=officer_fe+seniority_rank*coef(mod_mod)["seniority_rank"]+
            normal_work*coef(mod_mod)["normal_work"]]
## deviations from your base rate.


all_pairs[, tot_ot_among:=sum(ot_work), by="analysis_workdate"]




### auction: draw logit errors.
results<-data.table()
for (phi in seq(from=-0.01, to=0.01,by=0.001 )){
  print(paste0("Starting Score: ",phi))
  for (iter in 1:2500){
    if (iter %% 100 ==0) print(iter)

    ## true valuation does not include scoring.
    all_pairs[, true_valuation:=(det_val+rlogis(.N))/(coef(mod_mod)["max_rate"]/1.5)]
    all_pairs[, valuation:=true_valuation+max_rate*1.5+(seniority_rank-1)*phi]
    
    setorder(all_pairs, "analysis_workdate", -"valuation")
    # for each date, take the first tot_ot_among based on available officers, partitions, then rand
    all_pairs[,sim_prod:= (1:.N<=tot_ot_among)*safety_ability, by="analysis_workdate"]
    all_pairs[,sim_value:= (1:.N<=tot_ot_among)*true_valuation, by="analysis_workdate"]
    all_pairs[,sim_work:= (1:.N<=tot_ot_among), by="analysis_workdate"]
    byemp<-all_pairs[, .(ot_tot=sum(sim_work)), by="num_emp1"]
    setorder(byemp, "ot_tot","num_emp1")
    byemp[,position:= (1:.N)/.N ]
    byemp[,cum_ot:= cumsum(ot_tot)/sum(ot_tot)]
    byemp[, is_90th:= position>=0.9 & shift(position)<0.9]
    stopifnot(nrow(byemp[is_90th==1])==1)
    
    results<-rbind(results, data.table(sim_num=iter,safety_prod=sum(all_pairs$sim_prod),
                                       worker_value=sum(all_pairs$sim_value),
                                       share_top10=1-byemp[is_90th==1]$cum_ot[1],
                                       phi_val=phi))
  }
  saveRDS(results, "analysis/data/03_02_sim_auction_score.rds")
}



