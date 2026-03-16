


install.packages("gender")
library(gender)
library(stringr)
library(predictrace)
# Predict for multiple names

readRDS("mkdata/data/final_work_10years.rds")->hold
hold[, name_clean:=str_replace(`EMPLOYEE NAME`, " .*$", "")]
hold[, last_name_clean:=str_trim(str_extract(str_replace(`EMPLOYEE NAME`, "-[0-9]+$", ""), " [A-Za-z'-]*$"))]
add_gender<-unique(hold[!is.na(name_clean),c("name_clean")])
add_gender<-data.table(gender(names = add_gender$name_clean, years = 1990))
setnames(add_gender, old="name", new="name_clean")
hold<-merge(hold,add_gender[,c("gender", "name_clean")], by="name_clean", all.x=TRUE)

add_race<-unique(hold[!is.na(last_name_clean),c("last_name_clean")])
add_race<-predict_race(add_race$last_name_clean)
setnames(add_race, old="name", new="last_name_clean")
hold<-merge(hold,add_race[,c("likely_race", "last_name_clean")], by="last_name_clean", all.x=TRUE)

check<-hold[, .(sum(HOURS, na.rm=TRUE)), by=c("EMPLOYEE NAME", "gender","likely_race")]


check[,.(avg=mean(V1, na.rm=TRUE), count=.N),by=c("gender")]
check[,.(avg=mean(V1, na.rm=TRUE), count=.N),by=c("likely_race")]
