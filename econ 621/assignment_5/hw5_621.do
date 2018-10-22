

use "C:/Users/Nmath_000/Documents/MI_school/Second Year/621 Labor/Assignments/Assignment_5/macurdy_lee.dta", clear


***************
* question 2 *
***************

* subset sample 
keep if lee_macurdy_sample == 1

* create IV variables 
gen educ_sq = educ^2
gen ageXeduc = age*educ
gen ageXeduc_sq = age*educ_sq
gen smsa_large = smsa == 1
gen smsa_mid = smsa == 2
gen moth_hs = mothed == 2
gen moth_col = mothed == 3
gen fath_hs = fathed == 2 
gen fath_col = fathed == 3
gen econst_ave = econgrow == 2
gen econst_well = econgrow == 3

* create a local for these variables 
local iv14 "age educ educ_sq ageXeduc ageXeduc_sq white smsa_large smsa_mid moth_hs moth_col fath_hs fath_col econst_ave econst_well"

* create loacl for 5 instromental variables 
local iv5 = "age educ educ_sq ageXeduc ageXeduc_sq"

* create log hours and log wage variables 
gen wages = earnings/hours
gen lnh = ln(hours)
gen lnw = ln(wages)

* create variable for change in hours and wages between years 
bysort respid (year) : gen ch_lnh = lnh - lnh[_n-1] 
bysort respid (year) : gen ch_lnw = lnw - lnw[_n-1] 

* use survey weights 
svyset [pweight = wtvar]

* now do regression with these weights 
svy: regress lnh lnw, robust 








