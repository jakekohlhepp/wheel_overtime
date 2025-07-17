### estimate dynamic specifications with lags of OT and normal work.
library('ggplot2')
library('data.table')
library('alpaca')
library('kableExtra')
library('lubridate')






all_pairs<-readRDS("analysis/data/00_02_estimation_sample.rds")
## generate lagged vars
setorder(all_pairs, "num_emp1", "analysis_workdate")
all_pairs[, delta:=analysis_workdate-shift(analysis_workdate), by="num_emp1"]

## lagged ot
all_pairs[,l1_otwork:=ifelse(delta==1,shift(ot_work),NA) , by="num_emp1"]
all_pairs[,l2_otwork:=ifelse(delta==1,shift(ot_work,2),NA) , by="num_emp1"]
all_pairs[,l3_otwork:=ifelse(delta==1,shift(ot_work,3),NA) , by="num_emp1"]
all_pairs[,l4_otwork:=ifelse(delta==1,shift(ot_work,4),NA) , by="num_emp1"]
all_pairs[,l5_otwork:=ifelse(delta==1,shift(ot_work,5),NA) , by="num_emp1"]
all_pairs[,l6_otwork:=ifelse(delta==1,shift(ot_work,6),NA) , by="num_emp1"]
all_pairs[,l7_otwork:=ifelse(delta==1,shift(ot_work,7),NA) , by="num_emp1"]

all_pairs[,l1_normal_work:=ifelse(delta==1,shift(normal_work),NA) , by="num_emp1"]
all_pairs[,l2_normal_work:=ifelse(delta==1,shift(normal_work,2),NA) , by="num_emp1"]
all_pairs[,l3_normal_work:=ifelse(delta==1,shift(normal_work,3),NA) , by="num_emp1"]
all_pairs[,l4_normal_work:=ifelse(delta==1,shift(normal_work,4),NA) , by="num_emp1"]
all_pairs[,l5_normal_work:=ifelse(delta==1,shift(normal_work,5),NA) , by="num_emp1"]
all_pairs[,l6_normal_work:=ifelse(delta==1,shift(normal_work,6),NA) , by="num_emp1"]
all_pairs[,l7_normal_work:=ifelse(delta==1,shift(normal_work,7),NA) , by="num_emp1"]

mod <- alpaca::feglm(ot_work ~ opp_dist+suppliers_interacted+ot_rate+seniority_rank+normal_work+
                              l1_otwork+l2_otwork+l3_otwork+l4_otwork+l5_otwork+l6_otwork+l7_otwork+
                              l1_normal_work+l2_normal_work+l3_normal_work+l4_normal_work+l5_normal_work+l6_normal_work+l7_normal_work| num_emp1+analysis_workdate, 
                            all_pairs,family = binomial("logit"))
mod_mod<-alpaca::biasCorr(mod)
summary(mod_mod,type="clustered", cluster=~num_emp1)


## lagged ot + lagged work

mod_probit <- alpaca::feglm(ot_work ~ opp_dist+suppliers_interacted+ot_rate+seniority_rank+normal_work+
                              l1_otwork+l2_otwork+l3_otwork+l4_otwork+l5_otwork+l6_otwork+l7_otwork+
                              l1_normal_work+l2_normal_work+l3_normal_work+l4_normal_work+l5_normal_work+l6_normal_work+l7_normal_work| num_emp1+analysis_workdate, 
                            all_pairs,family = binomial("probit"))
mod_probit_mod<-alpaca::biasCorr(mod_probit)
summary(mod_probit_mod,type="clustered", cluster=~num_emp1)



## compute average ot hours.
avg_othours<-mean(all_pairs[ot_work==1]$varot_hours)

### displaying the estimates - both the valuation distribution and the coefficients.
## get apes
ape_logit<-summary(getAPEs(mod_mod),type="clustered", cluster=~num_emp1)$cm[,1:2]
ape_probit<-summary(getAPEs(mod_probit_mod),type="clustered", cluster=~num_emp1)$cm[,1:2]
output<-data.table(summary(mod_mod,type="clustered", cluster=~num_emp1)$cm[,1:2])
output<-cbind(output,ape_logit,data.table(summary(mod_probit_mod,type="clustered", cluster=~num_emp1)$cm[,1:2],ape_probit))
output<-rbind(output[,c(1,3,5,7)],output[,c(2,4,6,8)], use.names=FALSE)
output<-cbind(rep(rownames(ape_logit),2),c(rep("coef",19),rep("se",19)),output)

colnames(output)<-c("Variable","type","Estimate (Logit)","APE (Logit)","Estimate (Probit)","APE (Probit)" )
output[Variable=="suppliers_interacted", Variable:="Supplier Count x Distance from Wheel"]
output[Variable=="opp_dist", Variable:="Distance from Wheel"]
output[Variable=="ot_rate", Variable:="Overtime Wage"]
output[Variable=="seniority_rank", Variable:="Seniority Rank"]
output[Variable=="normal_work", Variable:="Normal Work"]
output[ str_detect(Variable, "l[0-9]{1}"), temp:= paste0(str_extract(Variable,"[0-9]{1}" ))]
output[ str_detect(Variable, "_otwork"), temp:= paste0("OT - Lag ", temp)]
output[ str_detect(Variable, "_normal_work"), temp:= paste0("Normal - Lag ", temp)]
output[!is.na(temp), Variable:=temp][, temp:=NULL]

setorder(output, "Variable", "type")
output[,`Estimate (Logit)`:=paste0(as.character(sprintf("%.3f",round(`Estimate (Logit)`,4))))]
output[type=="se",`Estimate (Logit)`:=paste0("(",`Estimate (Logit)`,")")]
output[,`APE (Logit)`:=paste0(as.character(sprintf("%.3f",round(`APE (Logit)`,4))))]
output[type=="se",`APE (Logit)`:=paste0("(",`APE (Logit)`,")")]
output[,`Estimate (Probit)`:=paste0(as.character(sprintf("%.3f",round(`Estimate (Probit)`,4))))]
output[type=="se",`Estimate (Probit)`:=paste0("(",`Estimate (Probit)`,")")]

output[,`APE (Probit)`:=paste0(as.character(sprintf("%.3f",round(`APE (Probit)`,4))))]
output[type=="se",`APE (Probit)`:=paste0("(",`APE (Probit)`,")")]
output[type=="se", Variable:=""]
kable(output[,-"type"], "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = NA, label=NA) %>%
  cat(., file = "analysis/out/tables/02_05_dynamic_est.tex")
