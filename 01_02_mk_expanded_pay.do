clear
cap log close
set type double
log using log/01_02_mk_expanded_pay.log, replace
set more off 
cap ssc install unique
local print_pic ="yes"
set scheme cleanplots 
//Purpose: Expand the pay data to have an ob for every day for each person and to do cumulative sums of each var. 

*** create some charts with various statistics of the two data sources.
use data/pay_data, clear
gen year = year(dofc(work_date))
* histograms of pay codes. 
gen month=mofd(dofc(work_date))
format month %tmCCYY
label variable month "Month of Work Date"
gen outlier = 1 if year(dofc(work_date))<=2013
drop if outlier==1
hist month, discrete density
if "`print_pic'"=="yes" graph export out/freq_work.pdf, replace
tostring year, replace
bys employee_name year: gen unique_emps=1 if _n==1
bys variation_desc year: gen unique_codes=1 if _n==1
bys employee_name work_date: gen unique_persondays=1 if _n==1
tempfile all
save `all'
replace year = "Overall"
replace unique_emps = .
drop unique_codes
bys variation_desc: gen unique_codes=1 if _n==1
bys employee_name: replace unique_emps=1 if _n==1
append using `all'
gen recs=1
collapse (count) recs unique_persondays unique_emps unique_codes , by(year)
outsheet using out/01_02_work_data_counts.csv, comma replace

use data/workers_comp, clear
tempfile forgrand
save `forgrand'
preserve
replace natureofinjury = "All Types"
append using `forgrand'
gen emp_count =1 
replace natureofinjury = subinstr(natureofinjury, "Multiple", "Mult",.) if strpos(natureofinjury, "Mult ")>0
replace natureofinjury = subinstr(natureofinjury, "Incl", "Include",.) if strpos(natureofinjury, "Incl ")>0
replace natureofinjury = subinstr(natureofinjury, " (e.g.,", "",.) if strpos(natureofinjury, " (e.g., ")>0
replace natureofinjury = subinstr(natureofinjury, ",", "",.)
collapse (count) emp_count, by(natureofinjury)
gen last = natureofinjury == "All Types"
sort last natureofinjury
gen percent = emp_count/emp_count[_N]
gen ovrl = natureofinjury=="All Types"
gsort ovrl -emp_count 
drop ovrl
outsheet using out/01_02_nature.csv, comma replace
restore
preserve
replace claimcausegroup = "All Types"
append using `forgrand'
gen emp_count =1 
collapse (count) emp_count, by(claimcausegroup)
gen last = claimcausegroup == "All Types"
sort last claimcausegroup
gen percent = emp_count/emp_count[_N]
gen ovrl = claimcausegroup=="All Types"
gsort ovrl -emp_count 
outsheet using out/01_02_cause.csv, comma replace
restore
gen month=mofd(doi)
label variable month "Month of Date of Injury"
format month %tmCCYY
gen outlier = 1 if year(doi)<=2013
hist month, discrete density  subtitle("Outliers Included")
if "`print_pic'"=="yes" graph export out/01_02_freq_compclaims.pdf, replace
hist month if outlier!=1, discrete density subtitle("Outliers Excluded")
if "`print_pic'"=="yes" graph export out/01_02_freq_compclaims_nooutliers.pdf, replace
****


*** save job_status terms
use 20170803_payworkers_comp/data/anonymized_data_073117, clear
* job_end_date is always blank
assert  job_end_date==.
gen analysis_workdate = dofc(job_status_date)
format analysis_workdate %td
keep if inlist(job_status, "Transferred Out", "Terminated")
keep analysis_workdate employee_name
duplicates drop
gen status_term = 1
tempfile status_terms
save `status_terms'
****



*** now begin working on pay data.
use data/pay_data, clear

* save out just termination paycodes
preserve
keep if strpos(variation_description, "TERM")>0
gen analysis_workdate = dofc(work_date)
format analysis_workdate %td
keep analysis_workdate variation_description employee_name
duplicates drop

* keep first observed termination code in streak
collapse (min) analysis_workdate, by(employee_name)
append using `status_terms'
duplicates drop analysis_workdate employee_name, force

* if the termination is before fiscal year 2014-2015, drop
drop if analysis_workdate<d(01jul2014) 

* verify that all conflicts are within a year.
bys employee_name (analysis_workdate): gen diff = analysis_workdate[_N]-analysis_workdate[1]
assert diff<=365
bys employee_name (analysis_workdate): gen count = _n

* use first termination record.
drop if count>1
drop count

* unique by employee
isid employee_name
keep employee_name analysis_workdate
rename analysis_workdate term_date
tempfile terms
save `terms'
restore

* only work pay codes.
keep if work==1 | strpos(variation_description, " IOD ")>0 | out_type==1 
* examine the effective rate.
gen test_rate = pay_amount/hours

* remove 0 test rates from hours worked.
drop if test_rate==0 & work==1

gen iod_flag = 1 if strpos(variation_description, "IOD")>0
* are the following unique within person-day?
bys employee_name work_date: assert dept==dept[1]

* collapse to day. we allow corrections (negatives to cancel out hours)
gen varot_hours = hours if strpos(lower(variation_description), "overtime")>0 & work==1
replace varot_hours= 0 if missing(varot_hours)
gen varstandard_hours = hours if strpos(lower(variation_description), "overtime")==0
replace varstandard_hours=0 if missing(varstandard_hours)

gen types = "not leave" if work==1 | iod_flag==1 
replace types = cleaned_variation_desc if out_type==1
assert !missing(types)

* by person, choose the var_rate that is highest. note that all regular base rates are less than 40.
* so any rates over 40 are clearly not work pay rates.
assert var_rate<34 if work==1
tab variation_description if var_rate>34
by employee_name work_date: egen max_rate = max(var_rate)
assert max_rate>=0
replace max_rate = -99 if max_rate>34


gen ot_pay_amount = pay_amount*(strpos(lower(variation_description), "overtime")>0)
gen work_pay_amount = pay_amount*work

rename div geo_div

collapse (sum) tot_hours = hours varstandard_hours varot_hours work_pay_amount ot_pay_amount (firstnm) iod_flag, by(employee_name work_date dept yearsoldonworkdate geo_div types sick_subset maximum_gap_2015 max_rate)


* for the categories of leave time, zero out negatives so things don't cancel across categories
replace tot_hours=0 if tot_hours<0 & types!="not leave"

* now collapse to just leave vs not leave.
replace types = "leave" if types!="not leave"

collapse (sum) tot_hours varstandard_hours varot_hours work_pay_amount ot_pay_amount (firstnm) iod_flag, by(employee_name work_date dept yearsoldonworkdate geo_div types sick_subset maximum_gap_2015 max_rate)


* now create separate var for leave time and zero out tot_hours for leave time and sum it all.
gen leave_hours = tot_hours if types=="leave"
replace leave_hours =0 if types=="not leave"
assert !missing(leave_hours)
gen sick_hours = tot_hours if sick_subset==1
replace sick_hours=0 if sick_subset!=1
replace tot_hours=0 if types=="leave"
collapse (sum) tot_hours leave_hours sick_hours varstandard_hours varot_hours work_pay_amount ot_pay_amount (firstnm) iod_flag, by(employee_name work_date dept yearsoldonworkdate geo_div maximum_gap_2015 max_rate)

*** save out the data at the div-officer-day level for network creation.
isid employee_name work_date geo_div,m
preserve
keep if varstandard_hours>0 | varot_hours>0
keep if !missing(geo_div)
gen analysis_workdate = dofc(work_date)
keep geo_div employee_name analysis_workdate
format analysis_workdate %td
save data/01_02_fornetwork, replace
restore



bys employee_name work_date (tot_hours geo_div): assert _N<=2
bys employee_name work_date (tot_hours geo_div): gen div1 = geo_div[1]
bys employee_name work_date (tot_hours geo_div): gen div2 = geo_div[2]
bys employee_name work_date (tot_hours geo_div): gen lhours_1 = leave_hours[1]
bys employee_name work_date (tot_hours geo_div): gen lhours_2 = leave_hours[2]
bys employee_name work_date (tot_hours geo_div): gen shours_1 = sick_hours[1]
bys employee_name work_date (tot_hours geo_div): gen shours_2 = sick_hours[2]
collapse (sum) leave_hours tot_hours sick_hours varstandard_hours varot_hours work_pay_amount ot_pay_amount (firstnm) iod_flag, by(employee_name work_date dept yearsoldonworkdate div1 div2 maximum_gap_2015 max_rate lhours_1 lhours_2 shours_1 shours_2)

isid employee_name work_date

*** tabulate rates
summ max_rate, d
count if max_rate==-99
assert max_rate<=34
* allow fill down for these rates
assert !missing(max_rate)
replace max_rate=. if max_rate==-99
bys employee_name (work_date): replace max_rate=max_rate[_n-1] if missing(max_rate)
assert !missing(max_rate)


**** NOTE: We zero one situation with negative total hours (36 total but only 1 after 01jan2015)
gen flag_hours_zeroed = tot_hours<0
replace tot_hours=0 if tot_hours<0
gen analysis_workdate=dofc(work_date)
format analysis_workdate %td

************** merge on injuries.
gen doi = analysis_workdate
merge m:1 employee_name doi using data/workers_comp, keepusing(timeofinj natureofinjury bodypart claimcause claimcausegroup contribcause medpd)
format doi %td

* check merge
bys employee_name (_m analysis_workdate ): gen flag=1 if _m[1]==_m[_N] & _m[1]==2
assert flag!=1
drop flag 
replace analysis_workdate = doi if missing(analysis_workdate)
gen matched_injury =_m==3
drop _m

* fix variables
replace tot_hours = 0 if missing(tot_hours)
replace varstandard_hours=0 if missing(varstandard_hours)
replace varot_hours = 0 if missing(varot_hours)
replace leave_hours = 0 if missing(leave_hours)
replace sick_hours=0 if missing(sick_hours)

sort employee_name analysis_workdate
tempfile data
save `data'

** expand the data to include the the fiscal years 2014-2015 and 2015-2016
use data/employee_data, clear
gen date_start = dofc(original_hire_date) if dofc(original_hire_date)>d(01jul2014)
replace date_start =d(01jul2014) if dofc(original_hire_date)<=d(01jul2014)
assert !missing(date_start)
keep employee_name date_start
duplicates drop
format date_start %td
merge 1:1 employee_name using `terms'
assert _m!=2
drop _m
gen date_end = d(30jun2016) if term_date>d(30jun2016)
replace date_end = term_date if term_date<=d(30jun2016)
format date_end %td
isid employee_name
drop term_date
reshape long date, i(employee_name) j(type) string
isid employee_name date
drop type
encode employee_name, gen(empid)
xtset empid date
tsfill
rename date analysis_workdate

bys empid (employee_name analysis_workdate): replace employee_name = employee_name[_N]
assert !missing(employee_name)

merge 1:1 employee_name analysis_workdate using `data'
drop if _m==2


replace tot_hours = 0 if missing(tot_hours) // this is crucial.
replace leave_hours = 0 if missing(leave_hours)
replace sick_hours =0 if missing(sick_hours)
replace matched_injury = 0 if matched_injury==.
isid employee_name analysis_workdate
sort employee_name analysis_workdate
by employee_name: replace dept=dept[_n-1] if missing(dept)
by employee_name: replace max_rate=max_rate[_n-1] if missing(max_rate)

assert tot_hours!=.
gen not_worked = tot_hours==0 //important flag.
label variable not_worked "Created to denote observations created to fill in time gaps."
foreach var of varlist tot_hours varstandard_hours varot_hours {
	replace `var' = 0 if missing(`var')
	by employee_name: gen cum_`var'=`var' if _n==1
	by employee_name: replace cum_`var'=cum_`var'[_n-1]+`var' if _n!=1

}

label variable cum_varot_hours "Cum. Sum. of all OT Hours Based on Var Desc."

drop work_date
assert !missing(analysis_workdate)



************** re-attach identifying information like hire date, etc. 
rename _m _mold
merge m:1 employee_name using data/employee_data
assert _merge==3
drop _merge
assert !missing(original_hire_date) & !missing(job_status)

***** 44 officers have no observed leave or work hours 
***** spot checks show that they are due to residual paycodes or work that is outside the window
***** remove these.
bys employee_name (_mold): gen all_miss = _mold[_N]==1
unique employee_name if all_miss==1
assert r(sum)==44
drop if all_miss==1
drop all_miss _mold

*****

gen an_week = wofd(analysis_workdate)
gen an_month= mofd(analysis_workdate)
format an_week %tw 


** add on weather
gen date = analysis_workdate

merge m:1 date using 20190811_weather/data/weather_daily
assert _m!=1 if analysis_workdate>=d(01jan2015)
drop if _m==2
drop _m

** add holidays
merge m:1 date using 20190814_fed_holidays/data/holidays
drop if _m==2
gen is_holiday = _m==3
drop _m
drop date

gen rain = prcp>0


label variable is_holiday "HOLIDAY"
label variable tmax "MAX. TEMP."
label variable rain "RAIN"
label variable work_pay_amount "Pay from Work Pay Codes"
label variable ot_pay_amount "Pay from OT Pay Codes"
label variable medpd "Medical Expenses Paid"

confirm variable maximum_gap_2015
compress
save data/working_expanded, replace



log close






