


***************
* set up data *
***************



* load in comp school age 
import delim "C:\Users\Nmath_000\Documents\data\pp713\Dropout_Age_final.csv", clear

local year = 2000

* start for loops to change variable names 
foreach var of varlist v3-v20 {
	
	
	
	rename `var' y_`year'
	local year=`year'+1
	
}

*generate policy change variable to fill in
gen policy_change = .


* loop over year variables 
forvalues i = 2000/2017 {
	
* If its the first year with a one for leaving age > 16, fill in that year for policy_change
replace policy_change = `i' if y_`i' == 1 & policy_change ==.

	
}


