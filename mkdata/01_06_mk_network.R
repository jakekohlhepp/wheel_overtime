library('data.table')
library('lubridate')
library('circular')
library('qgraph')
library('igraph')
library('ClusTorus')

### visualize the network of officers

## read in data - use 3 month window for now.
data<-fread('mkdata/data/01_03_pre_network_90.csv')
data[, analysis_workdate:=dmy(analysis_workdate)]
data[, num_emp1:=as.integer(gsub("EMPLOYEE ", "",employee_name))]



## exclude people with big gaps.
setkey(data, "num_emp1", "analysis_workdate")
data[, counter:=cumsum(tot_hours>0 | leave_hours>0  ) , by="num_emp1"]
data[, gap_length:= sum(tot_hours==0 & leave_hours==0), by=c("counter", "num_emp1") ]
data[, max_gap:= max(gap_length), by=c( "num_emp1") ]
### drop gaps that are larger than 30 days
data<-data[!(tot_hours==0 & leave_hours==0 & gap_length>=30),  ]
data<-data[, -c("counter", "gap_length", "max_gap")]

## total number of people with ot on each day
data[, ot_work:= varot_hours>0]
data[, normal_work:= varstandard_hours>0]

data[, tot_ot:= sum(ot_work), by="analysis_workdate"]

melt_data<-melt(data, id.vars=c("num_emp1", "analysis_workdate"),
                measure.vars=colnames(data)[grep("roll[0-9]+_exposure",colnames(data))])
melt_data[, num_emp2:=as.integer(gsub("roll[0-9]+_exposure", "",variable))]
all_pairs<-data[,do.call(CJ, list(num_emp1, num_emp1) ),by=c("analysis_workdate")]
setnames(all_pairs, old=c("V1", "V2"), new=c("num_emp1", "num_emp2"))

all_pairs<-merge(all_pairs,melt_data, all.x=TRUE, by=c("num_emp1", "num_emp2","analysis_workdate") )
all_pairs[is.na(value), value:=0]
## zero self-links
all_pairs[num_emp1==num_emp2, value:=0]
setkey(all_pairs,"analysis_workdate","num_emp1", "num_emp2")

all_pairs<-dcast(all_pairs, analysis_workdate+num_emp1~num_emp2, value.var ="value")
all_pairs<-merge(all_pairs, data[,c("num_emp1","analysis_workdate", "tot_hours", "leave_hours","matched_injury","normal_work" ,"tot_ot","ot_work", "seniority_rank", "an_age","max_rate", "medpd", "claimcause", "prcp", "tmax", "tmin", "varot_hours")], by=c("num_emp1","analysis_workdate"), all.x=TRUE)



### for each date starting 01jan2015 compute connectedness.
## in old versions of this program we used other notions of centrality, which required more packages
## connectedness is just the sum of each row (verified this), so it is qucker and more transparent to do it without auxiliary packages.
## a crucial aspect of these functions is that they account for
## who is active on each date.

all_pairs[, day:=weekdays(analysis_workdate)]
all_pairs[, dw:=as.factor(weekdays(analysis_workdate))]
all_pairs[, month:=month(analysis_workdate)]

### check own diagonal is 0
for (x in unique(all_pairs$num_emp1)){
  y<-as.character(x)
  stopifnot(all_pairs[num_emp1==x,.SD, .SDcols = y]==0)
}
check<-all_pairs[,.SD, .SDcols=colnames(all_pairs)[grep("^[0-9]+$",colnames(all_pairs))]]
all_pairs[, degree:=rowSums(check, na.rm=TRUE)]

## lag connectedness vars to day before.
setkey(all_pairs, "num_emp1", "analysis_workdate")
all_pairs[,l_degree:=shift(degree), by="num_emp1"]

setorder(all_pairs, "num_emp1", "analysis_workdate")
all_pairs[, first:=(1:.N)==1, by="num_emp1" ]
## all the na lagged centralities are first days.
stopifnot(all_pairs[is.na(l_degree), ]$first)

##all the nas on first days should be 0
all_pairs[is.na(l_degree) &first==TRUE,l_degree:=0 ]
stopifnot(!is.na(all_pairs$l_degree))


### construct connectedness weighted by wheel position.
## two notes: wheel position is based on day of, potential contact is day before. if not there day before, potential contact is 0.
## step 0: construct leave-one-out median of wheel on each date, and not leave one out, and mean
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
## step 1: construct angular distance from wheel for each person on that date.
# ang distance
#ang_dist<-function(x,y){
#  return(as.numeric(pmin(abs(x/max(x)-y/(2*pi)),1-x/max(x)+y/(2*pi))))
#}
all_pairs[, dist_from_med:=1-ang.dist(seniority_rank/max(seniority_rank)*2*pi, med_circ_exc)/pi, by="analysis_workdate"]
## lead the angular distance. lag the final measure to get timing right.
setkey(all_pairs, "num_emp1", "analysis_workdate")
all_pairs[, lead_dist_from_med:=shift(dist_from_med, n=1, fill=NA, type="lead"), by="num_emp1"]
## step 2: multiply potential contact today by lead_dist_from_med
## look up each potential contact, then multiply by lead_dist_from_med
temp<-dcast(all_pairs, analysis_workdate~num_emp1, value.var ="lead_dist_from_med")
stopifnot(colnames(temp)[-1]==colnames(all_pairs)[grep("^[0-9]+$",colnames(all_pairs))])
colnames(temp)[-1]<-paste0("wheel_", colnames(temp)[-1])
all_pairs<-merge(all_pairs,temp, by="analysis_workdate", all.x=TRUE )
## do multiplication on entire matrix
fold_mult<-function(x){
  ## set all NAs to 0.
  x[is.na(x)]<-0
  x[,1:(ncol(x)/2)]*x[,(ncol(x)/2+1):ncol(x)]
}
## check to see how multiplication is occuring. change the name of variables to see.

to_replace<-colnames(all_pairs)[grep("^[0-9]+$",colnames(all_pairs))]
all_pairs[, (to_replace):=fold_mult(.SD), .SDcols=c(colnames(all_pairs)[grep("^[0-9]+$",colnames(all_pairs))],colnames(temp)[-1])]

## step 3: construct. the weights are now accounting for the wheel distance.
### again use summing of rows methods because we use only direct connections.
for (x in unique(all_pairs$num_emp1)){
  y<-as.character(x)
  stopifnot(all_pairs[num_emp1==x,.SD, .SDcols = y]==0)
}
check<-all_pairs[,.SD, .SDcols=colnames(all_pairs)[grep("^[0-9]+$",colnames(all_pairs))]]
all_pairs[, wheel_degree:=rowSums(check, na.rm=TRUE)]

## lag connectedness vars to day before.
setkey(all_pairs, "num_emp1", "analysis_workdate")
all_pairs[,l_wheel_degree:=shift(wheel_degree), by="num_emp1"]

## all the na lagged wheel degrees are the first days of officers.
stopifnot(all_pairs[is.na(l_wheel_degree), ]$first)

##all the nas on first days should be 0
all_pairs[is.na(l_wheel_degree) &first==TRUE,l_wheel_degree:=0 ]
stopifnot(!is.na(all_pairs$l_wheel_degree))

## compute the maximum rank on each date
all_pairs[, max_rank_date:=max(seniority_rank), by="analysis_workdate"]
## compute the average hours of overtime on each date among those who work overtime.
all_pairs[, avg_othours_conditional:=sum(varot_hours)/sum(ot_work), by="analysis_workdate"]
## expected earnings is base rate times 1.5 times avg_othours_conditional
all_pairs[, expected_earnings:=avg_othours_conditional*max_rate*1.5]

hist(all_pairs$avg_othours_conditional)
saveRDS(all_pairs,"mkdata/data/01_06_panel_working.rds")





