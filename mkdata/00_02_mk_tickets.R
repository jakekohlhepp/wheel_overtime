## create ticket-based productivity.

tickets<-fread('mkdata/20250320_citations/Parking_Citations_20250320.csv')
tickets[, analysis_workdate:=date(mdy_hms(issue_date))]
tickets<-tickets[analysis_workdate>=as.Date("2015-01-01") & analysis_workdate<=as.Date("2016-06-30"), ]
tickets[agency_desc %in% c("52 - DOT - WILSHIRE","HOLLYWOOD","54 - DOT - HOLLYWOOD"),clean_name:="Hollywood-Wilshire"]
tickets[agency_desc %in% c("53 - DOT - VALLEY","VALLEY"),clean_name:="Valley"]
tickets[agency_desc %in% c("51 - DOT - WESTERN","WESTERN"),clean_name:="Western"]
tickets[agency_desc %in% c("55 - DOT - SOUTHERN","SOUTHERN"),clean_name:="Southern"]
tickets[agency_desc %in% c("50 - DOT - HARBOR"),clean_name:="Harbor Traffic Control"]
tickets[agency_desc %in% c("56 - DOT - CENTRAL","CENTRAL"),clean_name:="Central"]
tickets[agency_desc %in% c("57 - HABITUAL VIOLATORS"),clean_name:="Habitual Parking"]

### we also store those that are from special events.
tickets[agency_desc %in% c("58 - SPECIAL EVENTS"),clean_name:="Special Events"]

##99% of tickets are associated with ladot
print(nrow(tickets[!is.na(clean_name)])/nrow(tickets))
tickets<-tickets[!is.na(clean_name)]
tickets<-tickets[, .(ticket_count=.N, total_fines=sum(fine_amount, na.rm=TRUE)), by=c("clean_name", "analysis_workdate")]
saveRDS(tickets,"mkdata/data/00_02_tickets.rds")

