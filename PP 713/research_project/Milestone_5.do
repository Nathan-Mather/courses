
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

