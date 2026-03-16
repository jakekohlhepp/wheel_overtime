### obtain individual productivity using bonhomme from the group data.
library('fixest')
library('data.table')
library('lubridate')
library('stringr')
library('MASS')
## go back to the raw data because we need division allocation.
raw_panel<-fread('mkdata/data/01_03_pre_network_90.csv')
raw_panel[, analysis_workdate:=dmy(analysis_workdate)]
raw_panel[, num_emp1:=as.integer(gsub("EMPLOYEE ", "",employee_name))]
## only do people who are in one division in a day.
raw_panel<-raw_panel[is.na(div2),]
clean_names<-fread('mkdata/data/01_05_list_complete.csv')
setnames(clean_names, "Div Cd", "div1")
raw_panel<-merge(raw_panel,unique(clean_names[,c("clean_name", "div1")]) , by="div1", all.x=TRUE)

#### constructing collision productivity
collide<-readRDS("mkdata/data/00_01_collisions_with_mindist.rds")

## for each date we limit to just collisions that are within 400 meters of a special event.
collide_panel<-collide[, .(collide_400=sum(min_dist<=400)),by="analysis_workdate"]


# only ot hours.
collide_panel<-merge(raw_panel[varot_hours>0,],collide_panel, by=c("analysis_workdate"), all.x=TRUE)
# when no collision set to 0
collide_panel[is.na(collide_400), collide_400:=0]
collide_panel[, tot_ot:=.N, by="analysis_workdate"]


## iterate until identified set.

not_identified<-as.numeric()
all_not_id<-as.numeric()
stopit<-TRUE
while (stopit){

  collide_panel[, has_notid:=max(num_emp1 %in% all_not_id), by="analysis_workdate"]
  rel_pairs<-copy(collide_panel[has_notid==0 & !(num_emp1 %in% all_not_id),c("num_emp1", "analysis_workdate","collide_400", "tot_ot")])
  rel_pairs[, ot_work:=1]
  rel_pairs<-dcast(rel_pairs, analysis_workdate+collide_400+tot_ot  ~ num_emp1, value.var = "ot_work")

  
  B<-as.matrix(rel_pairs[,-c("analysis_workdate","collide_400","r_collide_400","tot_ot")])
  B[is.na(B)]<-0
  mat_size<-ncol(t(B)%*%ginv(t(B)))
  helper_mat<-(diag(1,mat_size)-t(B)%*%ginv(t(B)))
  not_identified<-as.numeric()
  for (i in colnames(B)){
    helper_vect<-rep(0, mat_size)
    names(helper_vect)<-colnames(B)
    helper_vect[which(colnames(B)==i)]<-1
    if (any(helper_mat%*%helper_vect>= 1e-08)) not_identified<-c(not_identified,as.numeric(i))
    #print(any(helper_mat%*%helper_vect>=1e-08))
  }
  
  all_not_id<-unique(c(all_not_id,not_identified))
  print(length(all_not_id))
  if (length(not_identified)==0) stopit<-FALSE
}

## we removed 33 not identified
stopifnot(length(all_not_id)==33)

## we bucket number of officers its buckets of 5.
rel_pairs[, bin5_num_officer:=floor(tot_ot/5)*5]

### net out time and size effects
fe_collide<-feols(collide_400~1|bin5_num_officer+as.factor(day(analysis_workdate))+as.factor(month(analysis_workdate)), data=rel_pairs)
rel_pairs[, r_collide_400:=resid(fe_collide)]

safety_ability<-solve(t(B)%*%B)%*%t(B)%*%as.numeric(rel_pairs$r_collide_400)
names(safety_ability)<-colnames(B)


officer_abilities<-data.table(num_emp1=unique(raw_panel$num_emp1), 
           safety_ability=safety_ability[as.character(unique(raw_panel$num_emp1))])
saveRDS(officer_abilities, "analysis/data/00_01_individual_productivity.rds")







