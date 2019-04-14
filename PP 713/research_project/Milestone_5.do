


***************
* set up data *
***************

* output directory 
global outdir "C:\Users\Nmath_000\Documents\Code\courses\PP 713\research_project"

* load in xwalk
use "C:\Users\Nmath_000\Documents\data\pp713\fips_to_census.dta" , clear

rename V1 census_region

rename V2 state_name

rename V3 bpl


* save as temp file for merge 
tempfile temp_region
 save "`temp_region'"
 
use "C:\Users\Nmath_000\Documents\data\pp713\acs_small.dta", clear


*  merge onto region temp file 
merge m:1 bpl using `temp_region'

drop _merge 

*save as temp file 
tempfile temp_data
 save "`temp_data'"

* load in comp school age 
import delim "C:\Users\Nmath_000\Documents\data\pp713\Dropout_Age_for_Testing.csv", clear

local year = 2000

foreach var of varlist v3-v20 {
	rename `var' y_`year'
	local year=`year'+1
}



*  merge onto region temp file 
merge 1:m bpl using `temp_data'

* make a dummy for if a particular student observation faced a minium school leaving age > 16
gen year_16 = birthyr + 16
tab year_16

* initialize variable for this dummy 
gen dropage_gr_16 = .


* loop over years 2000 - 2017 and get rule for each observation 
forvalues i = 2000/2017{

* create dummy for people 16 in year i

replace dropage_gr_16 = 1 if year_16 == `i' & y_`i' == 1
replace dropage_gr_16 = 0 if year_16 == `i' & y_`i' == 0

* do a check to make sure it worked 
tab dropage_gr_16 y_`i' if year_16 == `i'
}

numlabel, add
tab age
tab school age
tab educ

* create variable for in school or have hs degree 
gen educ_dum = 0
replace educ_dum = 1 if school == 2 | educ > 5

******************
* do regressions *
******************


* set up a matrix for the results 
matrix coefs = J(1,7,.)
matrix SE = J(1,7,.)

forvalues i = 14/20{


* get matrix position 
local mat_post = `i' - 13

* run regression 
* limit to those above 16 since those are who we expect a change for 
reg educ_dum dropage_gr_16 i.census_region if age == `i'

* store result in matrix 
matrix coefs[1,`mat_post'] = _b[dropage_gr_16]


* store cutoff in matrix 
matrix SE[1, `mat_post'] =  _se[dropage_gr_16]

}

mat li coefs
mat li SE
mat colnames coefs = 14 15 16 17 18 19 20

numlabel, remove 

coefplot matrix(coefs), se(SE)  vertical 

cd 
graph export "$outdir\coef_plot.png" , replace


graph bar educ_dum, over(age) label


graph export "$outdir\bar_plot.png" , replace





