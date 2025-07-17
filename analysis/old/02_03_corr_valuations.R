all_pairs<-readRDS("analysis/data/00_02_estimation_sample.rds")


load("analysis/data/02_00_estimate.Rdata")
officer_fe<-data.table(officer_fe=getFEs(mod_mod)$num_emp1, num_emp1=as.numeric(names(getFEs(mod_mod)$num_emp1)))
all_pairs<-merge(all_pairs, officer_fe, by="num_emp1", all.x=TRUE)


cor(all_pairs[!is.na(enforcement_ability) & !is.na(officer_fe), c("enforcement_ability", "l_degree", "safety_ability", "officer_fe","seniority_rank")])



all_pairs[,is_min:= min(analysis_workdate)==analysis_workdate, by="num_emp1"]
all_pairs[is_min==1,age_20150101:= time_length(an_age-(analysis_workdate-as.Date('2015-01-01')), unit="years")]
all_pairs[,age_20150101:= max(age_20150101,na.rm=TRUE), by="num_emp1" ]
cor(all_pairs[!is.na(enforcement_ability) & !is.na(officer_fe), c("enforcement_ability", "l_degree", "safety_ability", "officer_fe","seniority_rank")])

officers<-unique(all_pairs[, c("num_emp1", "ticket_ability", "safety_ability", "bar_degree","age_20150101","officer_fe")])
cor(officers[!is.na(ticket_ability) & !is.na(officer_fe), c("ticket_ability", "bar_degree", "safety_ability", "officer_fe","age_20150101")])

select_it<-function(x){
  which(quantile(x,0.99)>x & quantile(x,0.01)<x)
}
ggplot(officers[!is.na(enforcement_ability),], aes(x=safety_ability,y=officer_fe )) +
  stat_summary_bin(fun=match.fun(mean), bins=10,
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


