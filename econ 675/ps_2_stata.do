* NOTES:
* My code fricking CRAAAWLS. It is extremely slow. I guess trying to jerry rig how 
* I did this in R into the weird world of stata was not a great idea. 


clear all
set more off, perm

global dir "c:\Users\Nmath_000\Documents\Code\courses\econ 675\PS_2_tex\"

cap log close
log using $pset2_stata_log.smcl, replace
************************************
************ Question 1 ************
************************************

 global hvalues .5 .6 .7 .8 0.8199 .9 1 1.1 1.2 1.3 1.4 1.5
* global hvalues .5 
 	local h = .5 
 	local i = 1 
global n = 1000

* I need to only do 10 simulations because of how slow this thing is 
global m = 10
set obs $n

* replace with for loop eventually 
forvalues i = 1/10{
di `i'
* start loop 
clear 
set obs $n
* generate random data 
gen z_o = uniform() 
gen xi = rnormal(-1.5, sqrt(1.5)) if z_o < .5
replace xi = rnormal(1,1) if z_o >= .5

* drop zero one var
drop z_o

* gen constaant for merge 
gen const = 1

 * rename variable 
 rename xi x
 
* try merging this with teacher level enr_staff file 
joinby const using `rand_i'

* now loop over h values 
foreach h in $hvalues {

	di `h'
	* make h for file names 
	local h_n: subinstr local h "." "", all
	
	*preserve data before I mess with is
	preserve

	* gnerate u 
	gen u =  (xi-x )/`h'
	
	* calculate kernal for pairs 
	gen kern = (.75*(1-u^2)*(abs(u)<=1))/`h'

	
	* get means 
	bys x: egen fhats = mean(kern)
	
	egen tag = tag(x)
	keep if tag == 1
	drop xi const u kern
	
	* add in f_x
	gen f_x = .5*normalden(x, -1.5, sqrt(1.5)) + .5*normalden(x, 1, 1)

	* find sq error 
	gen sq_er = (fhats-f_x)^2
	
	* now get  imse_li
	egen imse_li = mean(sq_er)
	egen tag2 = tag(x)
	keep if tag2 == 1
	

	* fill in sum info 
	gen sim = `i'
	gen h = `h'
	
	
	* save temp data 
	tempfile imseli_`h_n'_`i'
	 quietly save "imseli_`h_n'_`i'", replace
	 
	* restore data, preserve it for next thing 
	restore
	
	preserve 
	* now do the leave on out, drop columns with the same x xi
	* this is bad coding but STATA is terrible so this is what it deserves 
	keep if x != xi

	* gnerate u 
	gen u =  (xi-x )/`h'
	
	* calculate kernal for pairs 
	gen kern = (.75*(1-u^2)*(abs(u)<=1))/`h'

	* collaps data to get means \* collapse data 
    collapse (mean) fhats = kern , by(x)
	
	* add in f_x
	gen f_x = .5*normalden(x, -1.5, sqrt(1.5)) + .5*normalden(x, 1, 1)

	* find sq error 
	gen sq_er = (fhats-f_x)^2
	
	* now get  imse_li 
	 collapse (mean) imse_lo = sq_er 
	
	* fill in sum info 
	gen sim = `i'
	gen h = `h'
	
	
	* save temp data 
	tempfile imselo_`h_n'_`i'
	quietly save "imselo_`h_n'_`i'" , replace
	
	* restore data for next h
	restore
	 
}
}
* now, because I dont think stata has lists we just load all that back in and stack it
* clear out data 
clear 

forvalues i = 1/$m{
foreach h in $hvalues {

	* make h for file names 
	local h_n: subinstr local h "." "", all
	
	append using "imseli_`h_n'_`i'.dta"
	append using "imselo_`h_n'_`i'.dta"

}
}

* Now collapse data to get mean leave on in and out across iteratiosn by h 
collapse (mean) imse_li = imse_li (mean) imse_lo = imse_lo , by(h)


* graph this stuff 
line imse_li imse_lo h

graph export "$dir\stata_plot_1_3_b.png", replace



*********************************************************************
******************* question 2 **************************************
*********************************************************************
* clear stuff and change any globals I need to change 
clear

* for loop over iterations 












