clear all
cap log close
set type double
log using log/process_holidays.log, replace
set more off 


*** Process weather data for merge
* source: https://data.world/sudipta/us-federal-holidays-2011-2020
import delimited  using "data/us-federal-holidays-2011-2020.csv", clear varnames(1)
rename date _date
gen date = date(_date, "MDY")
format date %td
drop _date

compress
save data/holidays, replace 

log close

