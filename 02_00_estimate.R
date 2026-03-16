library('data.table')
library('alpaca')


set.seed(633491)
all_pairs<-readRDS("analysis/data/00_02_estimation_sample.rds")

## estimate logit with two-way fixed effects
mod <- alpaca::feglm(ot_work ~ opp_dist+suppliers_interacted+ot_rate+seniority_rank+normal_work | num_emp1+analysis_workdate, 
                     all_pairs,family = binomial("logit"))
mod_mod<-alpaca::biasCorr(mod)
summary(mod_mod,type="clustered", cluster=~num_emp1)
save(mod_mod,file="analysis/data/02_00_estimate.Rdata")


## to be comprehensive we also include probit
mod_probit <- alpaca::feglm(ot_work ~ opp_dist+suppliers_interacted+ot_rate+seniority_rank+normal_work | num_emp1+analysis_workdate, 
                     all_pairs,family = binomial("probit"))
mod_probit_mod<-alpaca::biasCorr(mod_probit)
summary(mod_probit_mod,type="clustered", cluster=~num_emp1)

save(mod_probit_mod,file="analysis/data/02_00_estimate_probit.Rdata")







