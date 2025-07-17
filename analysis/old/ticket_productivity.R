#### constructing ticket productivity for regular shifts.
tickets<-readRDS("mkdata/data/00_02_tickets.rds")
# only standard hours,remove special event.
ticket_panel<-merge(raw_panel[varstandard_hours>0,],tickets[clean_name!="Special Events",], by=c("clean_name", "analysis_workdate"), all.x=TRUE)
## there is at least one ticket on every day and every division that has standard hours.
stopifnot(!is.na(ticket_panel$ticket_count))
ticket_panel[, tot_ot:=.N, by="analysis_workdate"]
ticket_panel[, bin5_num_officer:=floor(tot_ot/5)*5]


not_identified<-as.numeric()
all_not_id<-as.numeric()
stopit<-TRUE
while (stopit){
  
  ticket_panel[, has_notid:=max(num_emp1 %in% all_not_id), by=c("analysis_workdate", "clean_name")]
  rel_pairs<-copy(ticket_panel[has_notid==0 & !(num_emp1 %in% all_not_id),c("num_emp1", "analysis_workdate","clean_name","ticket_count","bin5_num_officer")])
  rel_pairs[, normal_work:=1]
  
  rel_pairs<-dcast(rel_pairs, analysis_workdate+clean_name+ticket_count+bin5_num_officer  ~ num_emp1, value.var = "normal_work")
  fe_ticket<-feols(ticket_count~1|bin5_num_officer+as.factor(day(analysis_workdate))+as.factor(month(analysis_workdate))+as.factor(clean_name), data=rel_pairs)
  rel_pairs[, r_ticket_count:=resid(fe_ticket)]
  
  B<-as.matrix(rel_pairs[,-c("analysis_workdate","ticket_count","r_ticket_count","clean_name")])
  B[is.na(B)]<-0
  mat_size<-ncol(t(B)%*%ginv(t(B)))
  
  helper_mat<-(diag(1,mat_size)-t(B)%*%ginv(t(B)))
  not_identified<-as.numeric()
  for (i in colnames(B)){
    helper_vect<-rep(0, mat_size)
    names(helper_vect)<-colnames(B)
    helper_vect[which(colnames(B)==i)]<-1
    if (any(helper_mat%*%helper_vect>=1e-08)) not_identified<-c(not_identified,as.numeric(i))
  }
  
  all_not_id<-c(all_not_id,not_identified)
  if (length(not_identified)==0) stopit<-FALSE
}
stopifnot(length(all_not_id)==0)
ticket_ability<-solve(t(B)%*%B)%*%t(B)%*%as.numeric(rel_pairs$r_ticket_count)
names(ticket_ability)<-colnames(B)
