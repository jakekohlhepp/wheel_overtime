### compile raw earnings to comouted earnings
library('data.table')
library('lubridate')
library('ggplot2')


## import raw data

data<-fread('mkdata/data/01_03_pre_network_90.csv')
data[, analysis_workdate:=dmy(analysis_workdate)]
data[, num_emp1:=as.integer(gsub("EMPLOYEE ", "",employee_name))]
summ_data<-data[analysis_workdate>=as.Date('2015-01-01')& analysis_workdate<=as.Date('2016-06-30'), .(total_otpay=sum(ot_pay_amount,na.rm=TRUE)), by="num_emp1"]

all_pairs<-readRDS("analysis/data/00_02_estimation_sample.rds")

check_emps<-merge(all_pairs[,.(est_otpay=sum(varot_hours*ot_rate)), by="num_emp1"],summ_data, by="num_emp1", all.x=TRUE )
check_emps[, gap:=total_otpay-est_otpay]