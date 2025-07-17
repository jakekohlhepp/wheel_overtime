### fact 2: ot among those ever injured.
all_pairs[matched_injury==1, first_injury:=min(analysis_workdate), by="num_emp1"]
all_pairs[, first_injury:=min(first_injury, na.rm=TRUE), by="num_emp1"]
all_pairs[, ever_injured:= is.finite(first_injury)]
for_fact<-all_pairs[analysis_workdate<=first_injury,.(tot_ot=sum(ot_work), tot_days=.N), by=c("ever_injured", "num_emp1")]
for_fact[, ot_frac:=tot_ot/tot_days]

ggplot(for_fact, aes(x=ot_frac,y= ..density.., fill=ever_injured))+
  geom_histogram(position="identity", alpha=0.75)+xlab("Fraction of Days Working Overtime")+ylab("Density of Officers")+
  theme_bw()+  guides(fill=guide_legend(title="Ever Injured"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title= element_text(size = 30),axis.text = element_text(size = 15), legend.title = element_text(size = 15), legend.text = element_text(size = 15))
ggsave("analysis/out/figures/01_00_injury_ot_hist.png", width=12, height=8, units="in")
for_fact[, .(mean(tot_ot/tot_days),.N), by="ever_injured"]

### 2sls fixed effects

res0<-feols(matched_injury~ 1|ot_work~opp_dist+suppliers_interacted+ot_rate, data=all_pairs, cluster="num_emp1")
res1<-feols(matched_injury~ 1|num_emp1|ot_work~opp_dist+suppliers_interacted+ot_rate, data=all_pairs, cluster="num_emp1")
res2<-feols(matched_injury~ normal_work|num_emp1|ot_work~opp_dist+suppliers_interacted, data=all_pairs, cluster="num_emp1")
res3<-feols(matched_injury~ normal_work+an_age+an_age^2+seniority_rank+normal_work |num_emp1|ot_work~opp_dist+suppliers_interacted+ot_rate, data=all_pairs, cluster="num_emp1")
res4<-feols(matched_injury~ normal_work+an_age+an_age^2+seniority_rank+normal_work |num_emp1+f_dw+f_month|ot_work~opp_dist+suppliers_interacted+ot_rate, data=all_pairs, cluster="num_emp1")
res5<-feols(matched_injury~ normal_work+an_age+an_age^2+seniority_rank+normal_work|num_emp1+analysis_workdate|ot_work~opp_dist+suppliers_interacted+ot_rate, data=all_pairs, cluster="num_emp1")
etable(summary(res0,stage=1), summary(res1,stage=1), summary(res2,stage=1), summary(res3,stage=1), summary(res4,stage=1), summary(res5,stage=1), fitstat=~r2+ ivfall,keep="!Constant",dict=c(s_degree = "Standardized Centrality", normal_workTRUE="Normal Work",ot_workTRUE="Overtime", an_age="Age" ,seniority_rank="Seniority Rank", emps="Firm Size",
                                                                                                                                                                                             max_rate="Wage",analysis_workdate="Date", num_emp1="Officer",f_month="Month", f_dw="Day of Week"),
       file="analysis/out/tables/01_00_relevance_regs.tex", replace=TRUE,signifCode=c(`***`=0.001,`**`=0.01, `*`=0.05))

etable(res0, res1,res2,res3,res4,res5, fitstat=~r2,keep="!Constant",dict=c(s_degree = "Standardized Centrality", normal_workTRUE="Normal Work", an_age="Age" ,seniority_rank="Seniority Rank",ot_workTRUE="Overtime", emps="Firm Size",
                                                                           max_rate="Wage",analysis_workdate="Date", num_emp1="Officer",f_month="Month", f_dw="Day of Week"),
       file="analysis/out/tables/01_00_2sls_regs.tex", replace=TRUE,signifCode=c(`***`=0.001,`**`=0.01, `*`=0.05))

## naive regression
res0<-feols(matched_injury~ ot_work, data=all_pairs, cluster="num_emp1")
res1<-feols(matched_injury~ ot_work+normal_work, data=all_pairs, cluster="num_emp1")
res2<-feols(matched_injury~ ot_work+normal_work|num_emp1, data=all_pairs, cluster="num_emp1")
res2<-feols(matched_injury~ ot_work+normal_work+an_age+seniority_rank+max_rate|num_emp1, data=all_pairs, cluster="num_emp1")
res4<-feols(matched_injury~ ot_work+normal_work+an_age+seniority_rank+max_rate|num_emp1+f_dw+f_month, data=all_pairs, cluster="num_emp1")
res5<-feols(matched_injury~ ot_work+normal_work+seniority_rank+max_rate|num_emp1+analysis_workdate, data=all_pairs, cluster="num_emp1")
etable(res0, res1,res2,res3,res4,res5, fitstat=~r2,keep="!Constant",dict=c(s_degree = "Standardized Centrality", normal_workTRUE="Normal Work", an_age="Age" ,seniority_rank="Seniority Rank",ot_workTRUE="Overtime", emps="Firm Size",
                                                                           max_rate="Wage",analysis_workdate="Date", num_emp1="Officer",f_month="Month", f_dw="Day of Week"),
       file="analysis/out/tables/01_00_naive_regs.tex", replace=TRUE,signifCode=c(`***`=0.001,`**`=0.01, `*`=0.05))
