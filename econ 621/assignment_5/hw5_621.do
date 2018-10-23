

use "C:/Users/Nmath_000/Documents/MI_school/Second Year/621 Labor/Assignments/Assignment_5/macurdy_lee.dta", clear
cd "C:\Users\Nmath_000\Documents\Code\courses\econ 621\assignment_5\"

***************
* question 2 *
***************

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
local iv14 age educ educ_sq ageXeduc ageXeduc_sq white smsa_large smsa_mid moth_hs moth_col fath_hs fath_col econst_ave econst_well

* create loacl for 5 instromental variables 
local iv5 age educ educ_sq ageXeduc ageXeduc_sq

* create log hours and log wage variables 
gen wages = earnings/hours
gen lnh = ln(hours)
gen lnw = ln(wages)

* create variable for change in hours and wages between years 
*bysort respid (year) : gen ch_lnh = lnh - lnh[_n-1] 
*bysort respid (year) : gen ch_lnw = lnw - lnw[_n-1] 

xtset respid year

// Create variables
gen ch_lnh  = lnh - L.lnh 
gen ch_lnw = lnw - L.lnw


* preserve data 
preserve 

* subset sample 
keep if lee_macurdy_sample == 1


* PART a
* use survey weights and cluster standard errors. 
regress ch_lnh ch_lnw [pweight = wtvar] , vce(cluster respid) 

* Part b
* do iv with 14 variables 
ivregress 2sls ch_lnh (ch_lnw = `iv14') [pw = wtvar] , vce(cluster respid) 

* PART c
* Do first stage regression 
regress ch_lnw `iv14' [pweight = wtvar] , vce(cluster respid) 
* test significance 
test `iv14'


* part D 
* do liml estimate with 14 vars 
ivregress liml ch_lnh (ch_lnw = `iv14') [pw = wtvar] , vce(cluster respid) 

*Part E 
* do 2sls with correct CI
ivregress 2sls ch_lnh (ch_lnw = `iv14') [pw = wtvar] , vce(cluster respid) 
rivtest, ci usegrid grid(-2.5(0.01)2.5)


* PART g
* Do first stage regression 
regress ch_lnw `iv5' [pweight = wtvar] , vce(cluster respid) 
* test significance 
test `iv5'


* Part h
* do iv with 14 variables 
ivregress 2sls ch_lnh (ch_lnw = `iv5') [pweight = wtvar] , vce(cluster respid) 

* Part i
* do iv with 14 variables 
ivregress liml ch_lnh (ch_lnw = `iv5') [pweight = wtvar] , vce(cluster respid) 

* Part j
ivregress 2sls ch_lnh (ch_lnw = `iv5') [pweight = wtvar] , vce(cluster respid) 
rivtest, ci usegrid grid(-2.5(0.01)2.5)


***************************
* K-P use entire data set *
***************************

restore 

* part k
* use survey weights and cluster standard errors. 
regress ch_lnh ch_lnw [pweight = wtvar] , vce(cluster respid) 

* part l 
* Do first stage regression 
regress ch_lnw `iv14' [pweight = wtvar] , vce(cluster respid) 
* test significance 
test `iv14'

* part m, n
* do iv with 14 variables 
ivregress 2sls ch_lnh (ch_lnw = `iv14') [pweight = wtvar] , vce(cluster respid) 
rivtest, ci usegrid grid(-2.5(0.01)2.5)

* do liml estimate with 14 vars 
ivregress liml ch_lnh (ch_lnw = `iv14') [pweight = wtvar] , vce(cluster respid) 


*part o, p
* do iv with 14 variables 
ivregress 2sls ch_lnh (ch_lnw = `iv5') [pweight = wtvar] , vce(cluster respid) 
rivtest, ci usegrid grid(-2.5(0.01)2.5)

* do iv with 14 variables 
ivregress liml ch_lnh (ch_lnw = `iv5') [pweight = wtvar] , vce(cluster respid) 










