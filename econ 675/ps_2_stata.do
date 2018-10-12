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
* 	local h = .5 // for line by line 
* 	local i = 1 // for line by line 
global n = 1000
global m = 10
set obs $n

* replace with for loop eventually 
forvalues i = 1/10{

* start loop 
clear 
set obs $n
* generate random data 
gen z_o = uniform() 
gen xi = 1.5*runiform()-1.5 if z_o < .5
replace xi = 1+runiform() if z_o >= .5

* drop zero one var
drop z_o

* gen constaant for merge 
gen const = 1

* save as temp file for merge 
tempfile rand_i
 save "`rand_i'"
 
 * rename variable 
 rename xi x
 
* try merging this with teacher level enr_staff file 
joinby const using `rand_i'

* now loop over h values 
foreach h in $hvalues {

	* make h for file names 
	local h_n: subinstr local h "." "", all
	
	*preserve data before I mess with is
	preserve

	* calculate kernal for pairs 
	gen kern = .75 * ( 1- ( ( xi-x )/`h' )^2 )*(abs(((xi-x)/`h'))<=1)
	
	* collaps data to get means \* collapse data 
    collapse (mean) fhats = kern , by(x)
	
	* add in f_x
	gen f_x = .5*normalden(x, -1.5, 1.5) + .5*normalden(x, 1, 1)

	* find sq error 
	gen sq_er = (fhats-f_x)^2
	
	* now get  imse_li 
	 collapse (mean) imse_li = sq_er 
	
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
	
	*do kernal 
	gen kern = .75 * ( 1- ( ( xi-x )/`h' )^2 )*(abs(((xi-x)/`h'))<=1)
	
	* collaps data to get means \* collapse data 
    collapse (mean) fhats = kern , by(x)
	
	* add in f_x
	gen f_x = .5*normalden(x, -1.5, 1.5) + .5*normalden(x, 1, 1)

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

/*



  # now do the imse calculations for each h in h_v
  for(i in 1:length(h_v)){

  
    # now get the f_hats for the leave one out by deleating the observation where x= xi. This will be rows 
    # 1, M+2, 2M+3, 3M+4 ... so eq(1, M*M, M+1) should take care of those 
    paired_dt_lo <- paired_dt[-c(seq(1, n*n, n+1)), ]
    
    # now get the mean of the f_hats leacing out the x 
    f_hats_lo <- paired_dt_lo[, list(f_hat_x = mean(k_x)/h), by = x_vx]
    
    # now add in f_x for each 
    f_hats[, f_x := f_x(get(x_vx))]
    f_hats_lo[, f_x := f_x(get(x_vx))]
    
    # now do squared error 
    f_hats[, sq_er := (f_hat_x - f_x)^2]
    f_hats_lo[, sq_er := (f_hat_x - f_x)^2]
    
    # now get imse 
    imse_li <- f_hats[, mean(sq_er)]
    imse_lo <- f_hats_lo[, mean(sq_er)]
    
    # now put into a data.table and put in list 
    ouput_list[[i]] <- data.table(imse_li = imse_li, imse_lo= imse_lo, h = h)
  }
  
  output <- rbindlist(ouput_list)
  
  return(output[])
  
}
