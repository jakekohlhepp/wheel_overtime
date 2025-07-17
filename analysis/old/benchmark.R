

## benchmark each system against manager.
stopifnot(uniqueN(managers$sim_num)==2500)
stopifnot(uniqueN(managers_val$sim_num)==2500)

auctions_dev_sum[, type:="Uniform-Markdown Auction"]
auctions_sum[, type:="Uniform-Wage Auction"]
status_quo[, type:="Informal Trading"]

setnames(status_quo, old=c("wage_bill", "surplus", "ineq", "prod"),
         new=paste0("mean_",c("wage_bill", "surplus", "ineq", "prod")))

total_res<-rbind(status_quo,auctions_sum,auctions_dev_sum)

mbench<-as.numeric(managers_sum$mean_prod)
names(mbench)<-as.numeric(managers_sum$savy_num )
find_m<-Vectorize(function(x){
  as.numeric(names(mbench)[min(which(x>mbench))])
})

total_res[, mbench_prod:=find_m(mean_prod)]


mbench<-as.numeric(managers_val_sum$mean_surplus)
names(mbench)<-as.numeric(managers_val_sum$savy_num )
find_m<-Vectorize(function(x){
  as.numeric(names(mbench)[max(which(x>mbench))])
})

total_res[, mbench_surplus:=find_m(mean_surplus)]
total_res[, mean_ineq:=round(mean_ineq, 3)]
total_res[, mean_prod:=round(mean_prod, 2)]
total_res[, mean_wage_bill:=formatC(round(mean_wage_bill), format="d", big.mark=",")]
total_res[, mbench_prod:=formatC(mbench_prod, format="d", big.mark=",")]
total_res[, mbench_surplus:=formatC(round(mbench_surplus), format="d", big.mark=",")]

### table: mean and manager equivalent for safety and worker surplus the two shift auctions and informal trading
total_res<-total_res[,c("type", "mean_wage_bill", "mean_ineq",
                        "mean_prod", "mbench_prod",
                        "mean_surplus", "mbench_surplus")]
colnames(total_res)<-c("System", "Wage Bill", "Inequality", "Value", "Benchmark", "Value", "Benchmark")

kable(total_res, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = NA, label=NA) %>%
  add_header_above(c(" " = 3, "Safety Productivity" = 2, "Non-Wage Utility" = 2)) %>% 
  cat(., file = "analysis/out/tables/03_99_system_summary.tex")