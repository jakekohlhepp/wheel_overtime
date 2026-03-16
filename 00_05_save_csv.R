## save to csv
library('data.table')
library('flexmix')

all_pairs<-readRDS("analysis/data/00_02_estimation_sample.rds")

write.csv(all_pairs[, c("ot_work", "opp_dist","l_degree" ,"l_wheel_nothave_degree","l_wheel_degree","num_emp1","tot_ot","analysis_workdate", "ot_rate")], file="analysis/data/00_05_csv_forstata.csv")

Model4 <- FLXMRglmfix(family = "gaussian", fixed = ~ ot_rate)
ff_4 <- flexmix(ot_work ~ l_degree, data = all_pairs, k=2,
                 concomitant = FLXPmultinom(~ opp_dist),model=Model4 )
