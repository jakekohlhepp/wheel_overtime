
## check it
data<-fread('analysis_network/data/00_01_estimation_data_formatlab.csv')
colnames(data)[1:3]<-c("d","y", "z")
x_mat<-data[,.SD, .SDcols=setdiff(4:27,c(9,21))]

y_formula<-as.formula(paste0("y~", paste0(colnames(x_mat), collapse="", sep="+"),"d"))
d_formula<-as.formula(paste0("d~", paste0(colnames(x_mat), collapse="", sep="+"),"z"))

res<-biprobit(y_formula, d_formula, data=data)

res_new<-biprobit(ot_work~l_degree+bar_degree+bar_rate+bar_age+bar_seniority+an_age+seniority_rank+max_rate+normal_work+f_month2+f_month3+f_month4+f_month5+f_month6+
                    f_month7+f_month8+f_month9+f_month10+f_month11+f_month12+f_dwMonday+f_dwSaturday+f_dwSunday+f_dwThursday+f_dwTuesday+f_dwWednesday,
                  matched_injury~ot_work+bar_degree+bar_rate+bar_age+bar_seniority+an_age+seniority_rank+max_rate+normal_work+f_month2+f_month3+f_month5+f_month6+
                    f_month7+f_month8+f_month9+f_month10+f_month11+f_month12+f_dwMonday+f_dwSaturday+f_dwSunday+f_dwThursday+f_dwTuesday+f_dwWednesday, data=all_pairs)

res_new<-biprobit(ot_work~l_degree+bar_degree+f_month2+f_month3+f_month4+f_month5+f_month6+
                    f_month7+f_month8+f_month9+f_month10+f_month11+f_month12+f_dwMonday+f_dwSaturday+f_dwSunday+f_dwThursday+f_dwTuesday+f_dwWednesday,
                  matched_injury~ot_work+bar_degree+f_month2+f_month3+f_month5+f_month6+
                    f_month7+f_month8+f_month9+f_month10+f_month11+f_month12+f_dwMonday+f_dwSaturday+f_dwSunday+f_dwThursday+f_dwTuesday+f_dwWednesday, data=all_pairs)

res_new<-biprobit(ot_work~l_degree+bar_degree+an_age+seniority_rank+max_rate+normal_work+f_month2+f_month3+f_month4+f_month5+f_month6+
                    f_month7+f_month8+f_month9+f_month10+f_month11+f_month12+f_dwMonday+f_dwSaturday+f_dwSunday+f_dwThursday+f_dwTuesday+f_dwWednesday,
                  matched_injury~ot_work+bar_degree+an_age+seniority_rank+max_rate+normal_work+f_month2+f_month3+f_month5+f_month6+
                    f_month7+f_month8+f_month9+f_month10+f_month11+f_month12+f_dwMonday+f_dwSaturday+f_dwSunday+f_dwThursday+f_dwTuesday+f_dwWednesday, data=all_pairs)

all_pairs[, work:=normal_work>0 | ot_work>0,]

res<-biprobit(work~l_degree+bar_degree+an_age+seniority_rank+max_rate+f_month2+f_month3+f_month4+f_month5+f_month6+
                f_month7+f_month8+f_month9+f_month10+f_month11+f_month12+f_dwMonday+f_dwSaturday+f_dwSunday+f_dwThursday+f_dwTuesday+f_dwWednesday,
              matched_injury~work+bar_degree+an_age+seniority_rank+max_rate+f_month2+f_month3+f_month5+f_month6+
                f_month7+f_month8+f_month9+f_month10+f_month11+f_month12+f_dwMonday+f_dwSaturday+f_dwSunday+f_dwThursday+f_dwTuesday+f_dwWednesday, data=all_pairs)

