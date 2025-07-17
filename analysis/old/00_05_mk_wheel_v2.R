library('data.table')
library('alpaca')
library('lubridate')
library('circular')
## use the raw data because we do not want to exclude anyone.
all_pairs<-readRDS('mkdata/data/01_06_panel_working.rds')

## first limit so that after each injury we do not include days until the first day where you work
setkey(all_pairs, "num_emp1", "analysis_workdate")
all_pairs[,inj_stint:= cumsum(matched_injury), by="num_emp1"]
all_pairs[(normal_work==1 | ot_work==1) & matched_injury==0 & inj_stint>0,first_work:= min(analysis_workdate), by=c("num_emp1","inj_stint")]
stopifnot(all_pairs[matched_injury==1,]$ot_work==1 |all_pairs[matched_injury==1,]$normal_work==1 )
all_pairs[inj_stint>0,first_work:= min(first_work, na.rm=TRUE), by=c("num_emp1","inj_stint")]
all_pairs[inj_stint==0,first_work:=min(analysis_workdate), by=c("num_emp1","inj_stint")]
all_pairs<-all_pairs[analysis_workdate>=first_work | matched_injury==1, ]
all_pairs<-all_pairs[ analysis_workdate>=as.Date('2015-01-01') & analysis_workdate<=as.Date('2016-06-30'),]


## function that gets the circular mean

circ_mean<-function(x,w){
  ## convert to degrees
  d<-x[w==1]/max(x)*2*pi
  ## return only for those who work
  return(atan2(sum(sin(d)), sum(cos(d))))
}

circ_mean2<-function(x,w){
  ## convert to degrees
  d<-x[w==1]/max(x)*2*pi
  d<-circular(d, units='radians', rotation='clock', 
              zero=0, modulo='2pi')
  ## return only for those who work
  return(mean(d))
}

circ_median2<-function(x,w){
  ## convert to degrees
  d<-x[w==1]/max(x)*2*pi
  d<-circular(d, units='radians', rotation='clock', 
              zero=0, modulo='2pi')
  ## return only for those who work
  return(median(d))
}

## median but exclude yourself.
circ_median2_exc<-function(z,w){
  ## convert to degrees
  return(sapply(1:length(z), function(x){
    d<-z[w==1 & 1:length(z)!=x]/max(z)*2*pi
    d<-circular(d, units='radians', rotation='clock', 
              zero=0, modulo='2pi')
    return(median(d))}))
}

all_pairs[,mean_circ:=circ_mean(seniority_rank, ot_work) , by="analysis_workdate"]
all_pairs[,mean_circ:=max(mean_circ, na.rm=TRUE) , by="analysis_workdate"]

all_pairs[,med_circ:=circ_median2(seniority_rank, ot_work) , by="analysis_workdate"]
all_pairs[,med_circ:=max(med_circ, na.rm=TRUE) , by="analysis_workdate"]

all_pairs[,med_circ_exc:=circ_median2_exc(seniority_rank, ot_work) , by="analysis_workdate"]
all_pairs[,med_circ_exc:=max(med_circ_exc, na.rm=TRUE) , by="analysis_workdate"]


all_pairs[,mean_circ2:=circ_mean2(seniority_rank, ot_work) , by="analysis_workdate"]
all_pairs[,mean_circ2:=max(mean_circ2, na.rm=TRUE) , by="analysis_workdate"]
#stopifnot(nrow(all_pairs[round(mean_circ-mean_circ2,digits=10)==0])==nrow(all_pairs))

## deviation
mrl<-function(x,w){
  ## convert to degrees
  d<-x[w==1]/max(x)*2*pi
  d<-circular(d, units='radians', rotation='clock', 
              zero=0, modulo='2pi')
  ## return only for those who work
  return(rho.circular(d))
}
all_pairs[,mrl:=mrl(seniority_rank, ot_work) , by="analysis_workdate"]
all_pairs[,mrl:=max(mrl, na.rm=TRUE) , by="analysis_workdate"]

check<-unique(all_pairs[, c("analysis_workdate", "mean_circ2", "tot_ot","mrl", "med_circ")])
check[, dow:=weekdays(analysis_workdate)]

## distance from median excluding self each day
## ang distance
ang_dist<-function(x,y){
  return(as.numeric(pmin(abs(x/max(x)-y/(2*pi)),1-x/max(x)+y/(2*pi))))
}
all_pairs[, dist_from_med:=ang_dist(seniority_rank, med_circ_exc)]
all_pairs[, max_rank_date:=max(seniority_rank), by="analysis_workdate"]


saveRDS(all_pairs[, c("num_emp1", "analysis_workdate", "med_circ","mean_circ2","dist_from_med","max_rank_date","mrl","med_circ_exc")],file="analysis/data/00_05_wheel_buyer_seller.rds")
