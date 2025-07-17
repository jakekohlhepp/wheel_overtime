
## connectedness and preferences counteract
## eliminating either one generates more inequality.
## eliminating both also increases inequality.

## old simulate outcomes by redrawing shocks 10,000 times.
results<-data.table()

for (iter in 1:10000){
  all_pairs[, eps_sim:=rnorm(.N)]
  all_pairs[, work_sim_0degree:= xb_0degree+sol_0degree$root>eps_sim]
  all_pairs[, work_sim_nopref:= xb_nopref+sol_nopref$root>eps_sim]
  
  byemp<-all_pairs[, .(ot_tot=sum(work_sim_0degree)), by="num_emp1"]
  setorder(byemp, "ot_tot","num_emp1")
  byemp[,position:= (1:.N)/.N ]
  byemp[,cum_ot:= cumsum(ot_tot)/sum(ot_tot)]
  byemp[, is_90th:= position>=0.9 & shift(position)<0.9]
  stopifnot(nrow(byemp[is_90th==1])==1)
  
  results<-rbind(results, data.table(sim_num=iter,type="no network",
                                     share_top10=1-byemp[is_90th==1]$cum_ot[1]))
  
  byemp<-all_pairs[, .(ot_tot=sum(work_sim_nopref)), by="num_emp1"]
  setorder(byemp, "ot_tot","num_emp1")
  byemp[,position:= (1:.N)/.N ]
  byemp[,cum_ot:= cumsum(ot_tot)/sum(ot_tot)]
  byemp[, is_90th:= position>=0.9 & shift(position)<0.9]
  stopifnot(nrow(byemp[is_90th==1])==1)
  
  results<-rbind(results, data.table(sim_num=iter,type="no preferences",
                                     share_top10=1-byemp[is_90th==1]$cum_ot[1]))
  
  
}
hist(results$share_top10)

ggplot(results, aes(x=share_top10, fill=type))+
  geom_histogram(position="identity", alpha=0.75)+xlab("Fraction of Days Working Overtime")+ylab("Number of Simulations")+
  theme_bw()+  guides(fill=guide_legend(title="Type"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title= element_text(size = 30),axis.text = element_text(size = 15), legend.title = element_text(size = 15), legend.text = element_text(size = 15))
ggsave("analysis/out/figures/01_03_decomp.png", width=12, height=8, units="in")

