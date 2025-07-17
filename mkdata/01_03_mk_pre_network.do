clear
cap log close
set type double
log using log/01_03_mk_prenetwork.log, replace
set more off 
cap ssc install unique
cap ssc install rangestat
local print_pic ="yes"
set scheme cleanplots 

*** Purpose: create dataset storing potential contact between workers

*** check that traffic officer I never work ot
use data/working_expanded, clear

bys employee_name (analysis_workdate): assert job_class_title[1]==job_class_title
gen is_ot = varot_hours >0
tab is_ot if job_class_title =="TRAF OFFICER II"
count if is_ot==1 & job_class_title =="TRAF OFFICER I"
assert r(N)==1
** there is only one documented day where a traffic officer i works overtime

*** store age from very raw data.
use data/pay_data, clear
assert !missing(yearsoldonworkdate )
gen analysis_workdate=dofc(work_date)
* age as of 20150101 - check that all ages agree
bys employee_name (yearsoldonworkdate analysis_workdate): gen age_20150101 = yearsoldonworkdate-(analysis_workdate-d(01jan2015))/365.25
assert !missing(age_20150101)
* age is correct to within a week
bys employee_name (age_20150101 analysis_workdate): assert round((age_20150101-age_20150101[1])*52)==0
bys employee_name (analysis_workdate): replace age_20150101 = age_20150101[1]
keep employee_name age_2015010
duplicates drop
isid employee_name
tempfile age
save `age'



use data/01_02_fornetwork, clear
isid employee_name geo_div analysis_workdate
* add on employee details
merge m:1 employee_name using data/employee_data, keepusing(job_class_title)
assert _m!=1
drop if _m==2
drop _m


*** limit to only traffic officer IIs, because they are the only ones who can have OT assigned in their name
bys employee_name (analysis_workdate): assert job_class_title[1]==job_class_title
keep if job_class_title =="TRAF OFFICER II"
drop job_class_title


preserve 
*** step 1: compute exposure
bys geo_div analysis_workdate (employee_name): gen exposure = 1/(_N-1)

** Step 2: cast to div-day
destring employee_name, ignore("EMPLOYEE ") gen(emp_num)
drop employee_name
reshape wide exposure, i(geo_div analysis_workdate) j(emp_num)

* set exposure variable to 0 when missing
foreach var of varlist exposure* {
    replace `var' = 0 if missing(`var')
}

tempfile widediv
save `widediv'

*** Step 3: reattach and collapse to officer-date
restore
merge m:1 geo_div analysis_workdate using `widediv'
assert _m==3
drop _m

keep exposure* employee_name analysis_workdate
compress

** this is just because of memory issues on some computers.
confirm variable exposure300
confirm variable exposure0
confirm variable exposure301
confirm variable exposure691

preserve 
keep employee_name analysis_workdate  exposure301 - exposure691
collapse (sum) exposure*, by(employee_name analysis_workdate)
tempfile half
save `half'
restore

keep employee_name analysis_workdate  exposure0 - exposure300 
collapse (sum) exposure*, by(employee_name analysis_workdate)
merge 1:1 employee_name analysis_workdate using `half'
assert _m==3
drop _m

*** Step 4: reattach to the fully expanded data that includes non-work days.
merge 1:1 employee_name analysis_workdate using data/working_expanded
keep if job_class_title =="TRAF OFFICER II"

assert _m!=1
drop _m
compress

sort employee_name analysis_workdate
foreach var of varlist exposure* {
    replace `var' = 0 if missing(`var')                              

}

*  old code which does just all time cum. sum
*sort employee_name analysis_workdate
  * foreach var of varlist exposure* {
   *     by employee_name: replace `var' = sum(`var')
    *}

* 1000 encompasses the entire period.

foreach p in 30 90 180 1000 {
    preserve

    * sum of exposure in p-day rolling window.
    * looping over vars is because of memory/var constraints in some stata implementations.
    foreach var of varlist exposure* {
        rangestat (sum) roll`p'_`var' = `var', interval(analysis_workdate -`p' 0) by(employee_name)
        drop `var'
    }
    

    *** remove those without observed work between january 1, 2015 and june 30, 2016
    gen has_work= tot_hours>0 & analysis_workdate>=d(01jan2015) & analysis_workdate<=d(30jun2016)
    bys employee_name (analysis_workdate): egen max_flag = max(has_work)
    unique employee_name if max_flag==0
    assert r(sum)==36
    drop if max_flag==0
    drop max_flag has_work
    
    *** when there is an injury, either the person worked normal hours or overtime
    replace varstandard_hours =0 if missing(varstandard_hours )
    replace varot_hours  =0 if missing(varot_hours )
    assert varstandard_hours >0 | varot_hours >0    if matched_injury==1
    
    ** construct tenure from original hire date and age from age on 20150101
    merge m:1 employee_name using data/employee_data, keepusing(original_hire_date)
    assert _m!=1
    drop if _m==2
    drop _m
    assert !missing(original_hire_date)
    gen tenure = (analysis_workdate - dofc(original_hire_date))/365.25
    label variable tenure "Tenure (years)"
    assert tenure>=0
    
    * for age: we have age on 20150101.
    merge m:1 employee_name using `age'
    assert _m!=1
    drop if _m==2
    drop _m
    assert !missing(age_20150101 )
    gen an_age = age_20150101 + (analysis_workdate-d(01jan2015))/365.25
    
    * rank by seniority many different ways
    bys analysis_workdate (tenure employee_name): egen seniority_rank = rank(tenure), field
    
    
    label variable max_rate "Wage"
    label variable an_age "Age"
    label variable seniority_rank "Seniority Rank in Org"
    
    
    * create value holding date of first observed injury
    bys employee_name matched_injury (analysis_workdate): gen first_inj_date = analysis_workdate if matched_injury==1
    bys employee_name (first_inj_date analysis_workdate): replace first_inj_date = first_inj_date[1]
    
    sort employee_name analysis_workdate
    isid employee_name analysis_workdate
    
    ** note that currently exposure is as of the end of the listed date.
    ** in general we want exposure as of the end of the previous date
    
    *** CAUTION: WHEN USING THIS DATA TO PLOT A NETWORK, WE MUST FIRST PURGE OUT EMPLOYEES THAT ARE NOT YET HIRED OR WHO WERE FIRED.
    *** ROWS ARE ALREADY PURGED THIS WAY. SO AN EMPLOYEE IS ONLY IN THIS DATA ON A DATE IF THEY ARE EMPLOYED
    *** BUT COLUMNS ARE NOT! SO TO CORRECTLY GET A NETWORK ONE NEEDS TO DROP ALL EMPLOYEE COLUMNS THAT ARE NOT ROWS ON THAT DATE.
    
    drop empid
    
    export delimited data/01_03_pre_network_`p'.csv, replace
    restore
}


    

log close

