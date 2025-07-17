library('data.table')
library('lubridate')

raw_panel<-fread('mkdata/data/01_03_pre_network_90.csv')
raw_panel[, analysis_workdate:=dmy(analysis_workdate)]
raw_panel[, num_emp1:=as.integer(gsub("EMPLOYEE ", "",employee_name))]
## only do people who are in one division in a day.
raw_panel<-raw_panel[is.na(div2),]
clean_names<-fread('mkdata/data/01_05_list_complete.csv')
setnames(clean_names, "Div Cd", "div1")

raw_panel<-merge(raw_panel,unique(clean_names[,c("clean_name", "div1")]) , by="div1", all.x=TRUE)
raw_panel<-raw_panel[varstandard_hours>0,]


tickets<-fread('Parking_Citations_20250320.csv')
tickets[, analysis_workdate:=date(mdy_hms(issue_date))]
tickets<-tickets[analysis_workdate>=as.Date("2015-01-01") & analysis_workdate<=as.Date("2016-06-30"), ]
tickets[agency_desc %in% c("52 - DOT - WILSHIRE","HOLLYWOOD","54 - DOT - HOLLYWOOD"),clean_name:="Hollywood-Wilshire"]
tickets[agency_desc %in% c("53 - DOT - VALLEY","VALLEY"),clean_name:="Valley"]
tickets[agency_desc %in% c("51 - DOT - WESTERN","WESTERN"),clean_name:="Western"]
tickets[agency_desc %in% c("55 - DOT - SOUTHERN","SOUTHERN"),clean_name:="Southern"]
tickets[agency_desc %in% c("50 - DOT - HARBOR"),clean_name:="Harbor Traffic Control"]
tickets[agency_desc %in% c("56 - DOT - CENTRAL","CENTRAL"),clean_name:="Central"]
tickets[agency_desc %in% c("57 - HABITUAL VIOLATORS"),clean_name:="Habitual Parking"]
tickets<-tickets[!is.na(clean_name)]
tickets<-tickets[, .(ticket_count=.N), by=c("clean_name", "analysis_workdate")]


raw_panel<-merge(raw_panel,tickets, by=c("clean_name", "analysis_workdate"), all.x=TRUE )
raw_panel[is.na(ticket_count), ticket_count:=0]
raw_panel[, normal_work:=varstandard_hours>0]

#### constructing ticket productivity.
rel_pairs<-copy(raw_panel[!is.na(clean_name),c("num_emp1", "analysis_workdate","clean_name","normal_work","ticket_count","f_dw", "f_month")])
rel_pairs<-dcast(rel_pairs, analysis_workdate+clean_name+ticket_count+f_dw+f_month  ~ num_emp1, value.var = "normal_work")

## first net out day of week, month and division fixed effects.
fe_tickets<-feols(ticket_count~1|f_dw+f_month+as.factor(clean_name), data=rel_pairs)
rel_pairs[, r_ticket_count:=resid(fe_tickets)]

B<-as.matrix(rel_pairs[,-c("clean_name","analysis_workdate","ticket_count")])
B[is.na(B)]<-0
mat_size<-ncol(t(B)%*%ginv(t(B)))
(diag(1,mat_size)-t(B)%*%ginv(t(B)))%*%rep(1,mat_size)->whofind
not_identified<-colnames(B)[which(abs(whofind)>=1e-08)]
## all are identified
stopifnot(length(not_identified)==0)
ticket_ability<-solve(t(B)%*%B)%*%t(B)%*%as.numeric(rel_pairs$ticket_count)
names(ticket_ability)<-colnames(B)

all_pairs<-readRDS( "analysis_network/data/00_01_estimation_sample.rds")
all_pairs[, ticket_ab:=ticket_ability[paste0(num_emp1)]]
ot_fixef<-fixef(feols(ot_work~l_degree+seniority_rank+max_rate+an_age+normal_work|f_dw+f_month+ num_emp1, data=all_pairs))$num_emp1
person_avg<-all_pairs[!is.na(ticket_ab),.(tot_ot=sum(ot_work)), c("num_emp1", "ticket_ab")]
person_avg[, value:=ot_fixef[paste0(num_emp1)]]
person_avg[, ticket_ab:=ecdf(ticket_ab)(ticket_ab)]
ggplot(person_avg, aes(x=ticket_ab,y=value )) +
  stat_summary_bin(fun=match.fun(mean), bins=10,
                   color='black',geom='point', size=2)+
  stat_summary_bin(fun.data='mean_cl_boot', bins=10,
                   color='black',geom='errorbar',position=position_dodge(1), width=.05)+
  ylab("Value")+xlab("Ticket Ability")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title= element_text(size = 30),axis.text = element_text(size = 15))


--
## reconstruct with just those who are identified.
rel_pairs<-copy(all_pairs[ot_work==1,c("num_emp1", "analysis_workdate","ot_work")])
rel_pairs<-dcast(rel_pairs, analysis_workdate  ~ num_emp1, value.var = "ot_work")
setnafill(rel_pairs, fill = 0)

B<-as.matrix(rel_pairs[,-"analysis_workdate"])





library('fixest')


library('MASS')
rel_pairs<-copy(all_pairs[ot_work==1,c("num_emp1", "analysis_workdate","ot_work")])
rel_pairs<-dcast(rel_pairs, analysis_workdate  ~ num_emp1, value.var = "ot_work")
setnafill(rel_pairs, fill = 0)

B<-as.matrix(rel_pairs[,-"analysis_workdate"])
mat_size<-ncol(t(B)%*%ginv(t(B)))
(diag(1,mat_size)-t(B)%*%ginv(t(B)))%*%rep(1,mat_size)->whofind
not_identified<-colnames(B)[which(abs(whofind)>=1e-08)]
all_pairs[, is_notid:= num_emp1 %in% not_identified]
all_pairs[, has_notid:=max(is_notid*ot_work), by="analysis_workdate" ]

## reconstruct with just those who are identified.
rel_pairs<-copy(all_pairs[ot_work==1,c("num_emp1", "analysis_workdate","ot_work")])
rel_pairs<-dcast(rel_pairs, analysis_workdate  ~ num_emp1, value.var = "ot_work")
setnafill(rel_pairs, fill = 0)

B<-as.matrix(rel_pairs[,-"analysis_workdate"])
