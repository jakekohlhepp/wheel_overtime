### estimate the klein and spady estimator
## need packages because this is going to the cluster.
library('data.table')
library('lubridate')
library('np')
library('stats')
library('bife')
library('alpaca')
set.seed(633491)
all_pairs<-readRDS("analysis/data/00_02_estimation_sample.rds")


## create holida-person fixed effects
all_pairs[, hol_person:=as.factor(paste0(num_emp1," - ", is_holiday))]


stat <- bife(
  ot_work ~ l_degree+max_rate | num_emp1,
  data  = all_pairs,
  model = "probit"
)
summary(stat)



mod <- alpaca::feglm(ot_work ~ l_degree+max_rate+seniority_rank | hol_person+analysis_workdate|num_emp1, 
                     all_pairs)
mod_mod<-alpaca::biasCorr(mod)
fes_both<-alpaca::getFEs(mod_mod)
summary(mod_mod,type="clustered", cluster=~num_emp1)


mod <- alpaca::feglm(ot_work ~ l_degree+max_rate+seniority_rank | num_emp1+analysis_workdate, 
                     all_pairs)
mod_mod<-alpaca::biasCorr(mod)
fes_both<-alpaca::getFEs(mod_mod)
summary(mod_mod,type="clustered", cluster=~num_emp1)


all_pairs[,is_min:= min(analysis_workdate)==analysis_workdate, by="num_emp1"]
all_pairs[is_min==1,age_20150101:= time_length(an_age-(analysis_workdate-as.Date('2015-01-01')), unit="years")]
all_pairs[,age_20150101:= max(age_20150101,na.rm=TRUE), by="num_emp1" ]
all_pairs[,valuation:=fes_both$num_emp1[as.character(num_emp1)] ]
cor(all_pairs[!is.na(enforcement_ability) & !is.na(valuation), c("enforcement_ability", "l_degree", "safety_ability", "valuation","seniority_rank")])

officers<-unique(all_pairs[, c("num_emp1", "enforcement_ability", "safety_ability", "bar_degree","age_20150101","valuation")])
cor(officers[!is.na(enforcement_ability) & !is.na(valuation), c("enforcement_ability", "bar_degree", "safety_ability", "valuation","age_20150101")])

select_it<-function(x){
  which(quantile(x,0.99)>x & quantile(x,0.01)<x)
}
ggplot(all_pairs[!is.na(enforcement_ability) & !is.na(l_degree),], aes(x=valuation,y=safety_ability )) +
  stat_summary_bin(fun=match.fun(mean), bins=20,
                   color='black',geom='point', size=2)+
  stat_summary_bin(fun.data='mean_cl_boot', bins=20,
                   color='black',geom='errorbar',position=position_dodge(1), width=.05)+
  theme_bw()
  ylab("Fraction Officer-Days with Overtime")+xlab("Potential Contact Centrality")+
  +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title= element_text(size = 30),axis.text = element_text(size = 15))

  
all_pairs[, groups:=floor(safety_ability/(diff(range(safety_ability))/10))]
ggplot(all_pairs[!is.na(enforcement_ability) & !is.na(l_degree),], aes(x=as.factor(groups),y=valuation )) +
    geom_boxplot()+
    theme_bw()

ggplot(all_pairs[!is.na(enforcement_ability) & !is.na(l_degree),], aes(x=l_degree ,y=valuation,color=safety_ability )) +
  geom_point()+
  scale_fill_gradient(low = "red", high = "yellow") +
  theme_bw()

officers[!is.na(valuation), q_degree:=ecdf(bar_degree)(bar_degree)]
officers[!is.na(valuation), q_valuation:=ecdf(valuation)(valuation)]
ggplot(officers[!is.na(enforcement_ability),], aes(y=bar_degree ,x=safety_ability )) +
  geom_point(size=4)+
  theme_bw()


## sample from logit errors.
library('stats')

sim_safety<-c()
for (i in 1:3000){
valuations<-fes_both$num_emp1+rlogis(length(fes_both$num_emp1))

date_get<-as.Date('2015-07-04')
num<-unique(all_pairs[analysis_workdate==date_get]$tot_ot)
raw_safety<-sum(all_pairs[analysis_workdate==date_get & ot_work==1]$safety_ability)

sim_safety<-c(sim_safety,sum(officers[num_emp1 %in% as.numeric(names(sort(valuations, decreasing=TRUE)[1:num])),]$safety_ability))

}
hist(sim_safety)
abline(v=raw_safety,col="red")
---
test<-all_pairs[runif(.N)<=0.005]
klein_spady<-npindex(ot_work ~ l_degree+max_rate+seniority_rank+an_age+
                       safety_ability+enforcement_ability+bar_degree+
                       is_holiday,
                       method="kleinspady", gradients=TRUE, data=test)

ksmooth(as.numeric(klein_spady$index), as.numeric(test$ot_work), kernel = c("box", "normal"), bandwidth = 0.5)

klein_spady<-npindex(ot_work ~ l_degree+max_rate+seniority_rank+an_age+
                       safety_ability+enforcement_ability+bar_degree+
                       is_holiday+f_dwTuesday+f_dwWednesday+f_dwThursday+
                       f_dwFriday+f_dwSaturday+f_dwSunday+
                       f_month2+f_month3+f_month4+f_month5+f_month6+f_month7+
                       f_month8+f_month9+f_month10+f_month11+f_month12,
                     method="kleinspady", gradients=TRUE, data=all_pairs)
save(klein_spady, file="analysis/data/02_00_kleinspady.RData")


