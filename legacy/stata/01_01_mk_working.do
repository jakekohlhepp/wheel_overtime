clear
cap log close
set type double
log using log/01_01_mk_working.log, replace


//reshape and structure data for analyses
* separate the injury and the pay data.

* first we get all the unique claims
use 20170803_payworkers_comp/data/anonymized_data_073117, clear
isid v1
* check that the following vars are constant within person:
sort employee_name v1
foreach var of varlist job_status totalnumberofinjuries original_hire_date civilian_entry_date  job_end_date job_class_title job_class jobclassdescription {
	by employee_name : assert `var'==`var'[1]
	di "`var' consistent within employee"
}

* they are constant. save out as time employee specific data.
preserve
bys employee_name (v1): keep if _n==1
keep job_status totalnumberofinjuries original_hire_date civilian_entry_date  job_end_date employee_name job_class_title job_class jobclassdescription
compress
save data/employee_data, replace
restore
* note that last_update_date and by are also almost constant within person.

keep employee_name timeofinj  - totalnumberofinjuries doi
drop dayofweek yearsoldonworkdate yearsatjobonworkdate totalnumberofinjuries quarter jobclass jobclassdescription
duplicates drop

assert !missing(doi) if !missing(timeofinj)
assert !missing(timeofinj) if !missing(doi)
assert !missing(natureofinjury) if !missing(doi)
assert !missing(doi) if !missing(natureofinjury)
drop if missing(doi)

* format expenses.
destring medpd, replace ignore(",")

compress
save data/workers_comp, replace

*** investigate pay data.
use 20170803_payworkers_comp/data/anonymized_data_073117, clear
* remove quotes from variation_desc
gen cleaned_variation_desc= subinstr(variation_desc, `"""',"", .)
sort employee_name work_date variation_desc v1
drop timeofinj  - last_update_date yearsatjobonworkdate totalnumberofinjuries doi v1 raword job_class_title job_class jobclassdescription
duplicates drop 

* flag work pay codes based on description - use inputted data
preserve
insheet using out/list_var_desc.csv, clear comma
drop if missing(variation_desc)
rename variation_desc cleaned_variation_desc
tempfile info
save `info'
restore

merge m:1 cleaned_variation_desc using `info'
assert _m==3
drop _m 

*** generate time buckets - assume normal week is a work week 
gen cal_week = wofd(dofc(work_date))
format cal_week %tw 

gen cal_month = mofd(dofc(work_date))
format cal_month %tm

gen cal_year = year(dofc(work_date))


*** identify large time gaps where there are no records in the time window after 01jan2015.
gen _date = dofc(work_date)
gen _period = _date>=d(01jan2015)
bys employee_name _period (_date): gen _gap = _date-_date[_n-1]
replace _gap=0 if _gap==.
replace _gap = 0 if _period==0
assert _gap!=.
bys employee_name (_date): egen maximum_gap_2015 = max(_gap) if _date>=d(01jan2015)
replace maximum_gap_2015=0 if _date<d(01jan2015)
gsort employee_name -maximum_gap_2015 _date
by employee_name: replace maximum_gap_2015 = maximum_gap_2015[1]
assert !missing(maximum_gap_2015)
label variable maximum_gap_2015 "Maximum # days between raw pay obs for gaps ending after 01012015"
*** group all gap periods greater than 31 days.
gen gap_end = _date if _gap>=31
bys employee_name work_date (gap_end): assert gap_end==gap_end[1] | gap_end==.
bys employee_name work_date (gap_end): replace gap_end=gap_end[1]
drop _period _date _gap


compress
save data/pay_data, replace

log close






