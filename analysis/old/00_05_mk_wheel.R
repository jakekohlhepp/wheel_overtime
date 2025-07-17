library('data.table')
library('alpaca')
library('lubridate')

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


## construct the likelihood of different wheel start positions

### step 1 : volunteers are those with highest ot probability among active time.
all_pairs[, v_hat:= sum(ot_work)/.N, by="num_emp1"]
## choose number of volunteers.
volunteers<-unique(all_pairs[, c("v_hat", "num_emp1")])
volunteers[, ord:=frank(-v_hat, ties.method="min")]
all_pairs<-merge(all_pairs, volunteers, by="num_emp1", all.x=TRUE)
puzzle<-data.table()
for (vv in seq(from=190, to=270, by=5)){
  
print(paste0("Start: ", vv, "volunteers"))

all_pairs[, volunteer:= ord<=vv ]

## first break ties in seniority. based on num_emp
all_pairs[, seniority_noties:=frank(num_emp1)-1+seniority_rank , by=c("seniority_rank", "analysis_workdate")]
all_pairs[, check:=uniqueN(seniority_rank)==.N, by="analysis_workdate"]

## total ot is 
all_pairs[, total_ot:=sum(ot_work), by="analysis_workdate"]


## fix starting position. then iterate over each day
all_starts<-all_pairs[analysis_workdate==as.Date('2015-01-01') & volunteer==1,]$num_emp1

for (iter in all_starts){

start_pos<-iter
setkey(all_pairs, "analysis_workdate", "seniority_noties")

for (dd in unique(all_pairs$analysis_workdate)){
  ## skip days without ot
  if (nrow(all_pairs[ analysis_workdate==dd & ot_work==1,])==0) next
  
  ## if the start_pos does not exist, go to next person day prior when start_pos did exist.
  check<-nrow(all_pairs[ analysis_workdate==dd &   num_emp1==start_pos,])==0
  ## increment by 1 only if we don't go into loop
  inc_flag<- 1-as.integer(check)
  
  while (check){
    helper<-unique(all_pairs[ analysis_workdate==dd -1 &   num_emp1==start_pos,]$seniority_noties)
    start_pos<-unique(all_pairs[ analysis_workdate==dd -1 &   seniority_noties==check+1,]$num_emp1)
    check<-nrow(all_pairs[ analysis_workdate==dd &   num_emp1==start_pos,])==0
  }
  
  ## those who get wheel assigned are based on first seniority_noties after start_pos, circling back around
  sen_start<- inc_flag + all_pairs[num_emp1==start_pos & analysis_workdate==dd,]$seniority_noties 
  
  ## implement by setting sort order to be 1000+senioity rank for people less than the start.
  all_pairs[volunteer==1 & analysis_workdate==dd,temp_sort:=1000*(seniority_noties<sen_start) +seniority_noties]
  setorder(all_pairs, "analysis_workdate","volunteer" ,"temp_sort")
  all_pairs[volunteer==1 & analysis_workdate==dd,wheel_assign:= 1:.N <=pmin(total_ot,length(all_starts)) ]
  ## assume that the wheel never cycles more than once a day.
  ## assume that if we exceed the number of volunteers on a day we start at position 1 the next day.
  
  
  ## next start_pos is next person on list.
  
  all_pairs[volunteer==1 & analysis_workdate==dd , start_flag:= ifelse(pmin(total_ot,length(all_starts))>=.N,1,pmin(total_ot,length(all_starts))) ==1:.N ]
  
  start_pos<-unique(all_pairs[volunteer==1 & analysis_workdate==dd & start_flag==TRUE,]$num_emp1)
    
  all_pairs[, start_flag:=NULL]
}

## step 2: compute the log-likelihood.
# p will be total worked by volunteers divided by total ot
p_hat<- sum(all_pairs[wheel_assign==1,]$ot_work)/sum(all_pairs$ot_work)

# phat is the probability that the perosn assigned works.
## then the likelihood of each date is p_hat^(num people assigned who work)*(1-p_hat)^(num who do not.)


calc_vect<-all_pairs[, .(work_assigned=sum(ot_work*wheel_assign,na.rm=TRUE)), by=c("total_ot", "analysis_workdate")]
calc_vect[, ll_piece:= work_assigned*log(p_hat)+(total_ot-work_assigned)*log(1-p_hat)]

piece<-data.table(wheel_work=sum(calc_vect$work_assigned),ll=sum(calc_vect$ll_piece), startpos=iter, v_count=vv)
puzzle<-rbind(puzzle, piece)
print(paste0("Completed: ", which(iter==all_starts)/length(all_starts)))
print(paste0("Work-wheel: ", sum(calc_vect$work_assigned)))
}
saveRDS(puzzle,"analysis/data/00_05_wheel_loglik.rds")
}
##
