clear all
cap log close
set type double
log using log/process_weather.log, replace
set more off 


*** Process weather data for merge
* source: NOAA National Centers for Environmental Information, accessed 2019-08-11.
import delimited  using data/1834210.csv, clear
keep if name=="LOS ANGELES DOWNTOWN USC, CA US"
rename date _date
gen date = date(_date, "YMD")
format date %td
drop _date


* no missing temps or precips
assert !missing(tmax)
assert !missing(tmin)
assert !missing(prcp)
confirm numeric variable tmin tmax prcp

keep tmin tmax prcp date
isid date

compress
save data/weather_daily, replace 

log close

