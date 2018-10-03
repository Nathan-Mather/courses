cap log close
global dir "C:\Users\Nmath_000\Documents\MI_school\Second Year\621 Labor\Assignments\Assignment_3"
log using "C:\Users\Nmath_000\Documents\MI_school\Second Year\621 Labor\Assignments\Assignment_3\solutions\hw3_621_log.log", replace

***************
* set up data *
***************
clear
clear programs
use "C:\Users\Nmath_000\Documents\MI_school\Second Year\621 Labor\Assignments\Assignment_3\MROZ.DTA"

gen age_sq = age^2
gen age_cu = age^3
gen educ_sq = educ^2
gen educ_cu = educ^3
gen age_educ = age*educ
gen age_sq_educ = age_sq*educ
gen age_educ_sq = age*educ_sq
gen nonlab_i = (faminc - wage*hours - huswage*hushrs)/1000

gen working = hours > 0

*ivregress 2sls hours nwifeinc kidslt6 kidsge6 age educ //
 *(lwage = age_sq age_cu educ_sq educ_cu age_educ age_sq_educ age_educ_sq unem city motheduc fatheduc), robust

* working ~nwifeinc + kidslt6 + kidsge6 + age + educ +
* age_sq + age_cu + educ_sq + educ_cu + 
*   age_educ + age_sq_educ + age_educ_sq + 
*   unem + city + motheduc + fatheduc
 
 
 ********
 * Q1 A *
 ********
  
  
 local w_vars =  "kidslt6 kidsge6 age educ age_sq age_cu educ_sq educ_cu" 
local w_vars "`w_vars' age_educ age_sq_educ age_educ_sq unem city motheduc fatheduc "
 
 
 program myprobit
version 14
args lnf theta1
quietly replace `lnf' = ln(normal(`theta1')) if $ML_y1==1
quietly replace `lnf' = ln(normal(-`theta1')) if $ML_y1==0
end

ml model lf myprobit (beta: working = `w_vars')
ml search
ml maximize


********
* Q1 B *
********


program myolsnorm
version 14
args lnf theta1 lnsigma
tempvar sigma
gen double `sigma' = exp(`lnsigma')
quietly replace `lnf' = ln(normalden($ML_y1, `theta1', `sigma'))
end


ml model lf myolsnorm (beta: hours = `w_vars') (lnsigma:)
ml search
ml maximize


********
* Q1 C *
********


program mytunc_olsnorm
version 14
args lnf theta1 lnsigma
tempvar sigma
gen double `sigma' = exp(`lnsigma')
quietly replace `lnf' = ln(normalden($ML_y1, `theta1', `sigma'))/(1-normal(0))
end


ml model lf mytunc_olsnorm (beta: hours = `w_vars') (lnsigma:)
ml search
ml maximize

********
* Q1 D *
*********

* CHECK not use if it sohuld be `sigma'^2 or just `sigma' down there 

program mytobit
version 14
args lnf theta1 lnsigma
tempvar sigma
gen double `sigma' = exp(`lnsigma')
quietly replace `lnf' = ln(normalden($ML_y1, `theta1', `sigma')) if $ML_y1>0
quietly replace `lnf' = ln(normal(-`theta1'/`sigma'^2)) if $ML_y1==0
end


ml model lf mytobit (beta: hours = `w_vars') (lnsigma:)
ml search
ml maximize


********
* Q1 e *
********

* replace missing hours with something else i guess 
replace lwage = -99 if missing(lwage)

* define wage vars 
local z1i_xvars =  "age educ age_sq age_cu educ_sq educ_cu" 
local z1i_xvars "`w_vars' age_educ age_sq_educ age_educ_sq unem city motheduc fatheduc "
 

program my_heckman
version 14
args lnf theta1 theta2 lnsigma atanhp
tempvar rho
tempvar sigma
gen double `rho' = tanh(`atanhp')
gen double `sigma' = exp(`lnsigma')


quietly replace `lnf' = ln(normalden($ML_y1, `theta1',`sigma'))+ ln(normal((`theta2'+ (`rho'/`sigma')*($ML_y1 -`theta1'))/sqrt(1-`rho'^2))) if $ML_y2 ==1
quietly replace `lnf' = ln(normal(-`theta2')) if $ML_y2 == 0 


end

ml model lf my_heckman (beta: lwage = `z1i_xvars') (pi: working = `w_vars') (lnsigma:) (atanhp:)

ml search
ml maximize



* check answer
heckman lwage `z1i_xvars', select(working = `w_vars')


*******
*Q2 b *
*******

* load data 
use "C:\Users\Nmath_000\Documents\MI_school\Second Year\621 Labor\Assignments\Assignment_3\friedberg_cps.DTA", clear

clear programs

* create age control 
gen agemin65=age-65

*create budget threshhold 
gen threshold_hours=threshold/hrlywage

* generate categorical variable for shich part of budget they are at
gen budget_segment=0
replace budget_segment = 1 if belowkink==1
replace budget_segment = 2 if middlesegment==1
replace budget_segment = 3 if uppersegment==1

 

* Generate marginal tax rate using info from Friedman paper
gen     etau = 0.5 if inrange(age, 62, 64)
replace etau = 0.33 if inrange(age, 65, 69)
replace etau = 0 if age>69


foreach x of numlist 1/3 {
	gen netwage`x'=hrlywage
	gen virtual_inc`x'=non_labor_inc 
}

replace netwage2=etau*netwage2 

replace virtual_inc2=non_labor_inc+etau*threshold
replace virtual_inc3=non_labor_inc-incss


program nonlinbudget
	version 14
	args lnf thetax thetaw thetay lnsigma
	tempvar theta1 theta2 theta3 sigma
	quietly gen double `sigma' = exp(`lnsigma')
	quietly gen double `theta1' = `thetax' + `thetaw'*$ML_y2 + `thetay'*$ML_y5 
	quietly gen double `theta2' = `thetax' + `thetaw'*$ML_y3 + `thetay'*$ML_y6 
	quietly gen double `theta3' = `thetax' + `thetaw'*$ML_y4 + `thetay'*$ML_y7

	quietly replace `lnf'= log((1/`sigma')* ///
			normalden(($ML_y1-`theta1')/`sigma')) if $ML_y9==1 //bottom
			
	quietly replace `lnf'= log((1/`sigma')* ///
			normalden(($ML_y1-`theta2')/`sigma')) if $ML_y9==2 // mid
			
	quietly replace `lnf'= log((1/`sigma')* ///
			normalden(($ML_y1-`theta3')/`sigma')) if $ML_y9==3 // top
			
	quietly replace `lnf'= ///
	 log(normal(($ML_y8-`theta2')/`sigma')-normal(($ML_y8-`theta1')/`sigma')) ///
	 if (normal(($ML_y8-`theta2')/`sigma')-normal(($ML_y8-`theta1')/`sigma'))>0 ///
	 & $ML_y9==0 // kink, non negative value
	 
	quietly replace `lnf'= -99999 ///
	 if normal(($ML_y8-`theta2')/`sigma')-normal(($ML_y8-`theta1')/`sigma')<=0 ///
	 & $ML_y9==0 // kink negative value, make ml avoid this case
	 
	quietly replace `lnf' = `lnf' - log(normal(`theta1'/`sigma')) //truncation 
end


mat all= [321.3,59.3,263.62,-131.96,937.33,38.8,-.03814,7.099]

ml model lf nonlinbudget (betax: annhours netwage1 netwage2 netwage3 ///
		virtual_inc1 virtual_inc2 virtual_inc3 threshold_hours budget_segment = ///
		hsgrad nonwhite married agemin65) (betaw:) (betay:) (lnsigma:)
ml init all, copy
ml maximize

* now find the elasticity 
ereturn list
matrix list e(b)
mat b =e(b)
* grab wage coef 
scalar wagecoef= b[1,6]

* create wage var 
gen wage=hrlywage
* replace it for those who are coming off SS 
replace wage=hrlywage*.5 if budget_segment==2 
* get mean wage 
sum wage
scalar wagemean = r(mean)

* get mean hours 
sum annhours
scalar hoursmean = r(mean)


 /*
 Uncompensated wage elasticity is the derivative of hours w.r.t. to net wage
 times the average net wage over the average hours
 */
di "Uncompensated wage elasticity:" wagecoef*wagemean/hoursmean

********
* Q2 C *
********

 foreach x of numlist 1/3 {
     gen lnetwage`x'=log(netwage`x') 
  }

  mat all= [321.3,59.3,263.62,-131.96,937.33,log(38.8),-.03814,7.099]

 ml model lf nonlinbudget (betax: annhours lnetwage1 lnetwage2 lnetwage3 ///
                virtual_inc1 virtual_inc2 virtual_inc3 threshold_hours budget_segment = ///
                hsgrad nonwhite married agemin65) (betaw:) (betay:) (lnsigma:)

. ml init all, copy

. ml maximize

 * get elasticity 
  mat b =e(b)

. scalar lwagecoef= b[1,6]

 /*
 Uncompensated log wage elasticity is the derivative of hours w.r.t. to net wage
 times 1 over the average hours
 */
 di "Uncompensated log wage elasticity:" lwagecoef/hoursmean


 *************
 **** Q 3 ****
 *************
* load data 
use "C:\Users\Nmath_000\Documents\MI_school\Second Year\621 Labor\Assignments\Assignment_3\cps_ssa_1978.dta", clear

clear programs


 
 gen earn_minus_threshold = earn - threshold

 * loop over values of delta
 foreach deltar of numlist 1500 1000 {
local delta = `deltar'*cpi/100
 
 preserve 
 * Check for bunching
  hist earn_* if inrange(earn_minus_threshold, -2000, 2000), width(50) xline(0)

 
 gen Hstar = ((-`delta' < earn_minus_threshold) & (earn_minus_threshold <= `delta'))

 gen Hstarm = ((-2*`delta' < earn_minus_threshold) & (earn_minus_threshold <= -`delta'))

 gen Hstarp = ((`delta' < earn_minus_threshold) & (earn_minus_threshold <= 2*`delta'))

 
 qui summ Hstar

 local Hs = r(mean)*100

 qui summ Hstarm

 local Hsm = r(mean)*100

 qui summ Hstarp

 local Hsp = r(mean)*100
 di "HS using Delta = `deltar':   " `Hs' 
  di "Hsm using Delta = `deltar':   "`Hsm' 
   di "Hsp using Delta = `deltar':   "`Hsp' 
 
 local B = `Hs'-(`Hsm'+`Hsp')

 di "B using Delta = `deltar':   "`B'


 
 local hzm = `Hsm'/`delta'

 local hzp = `Hsp'/`delta'

 
 di "hzm using Delta = `deltar':   "`hzm' 


 di "hzp using Delta = `deltar':   "`hzp'

local hbar = (`hzm'+`hzp')/2

local z = 5440
local t0 = 0
local  t1 = .5
local e = (`B'/`hbar')/(`z'*(log(1-`t0')-log(1-`t1')))


di "e using Delta = `deltar':   " `e'

restore

}



log close
translate "C:\Users\Nmath_000\Documents\MI_school\Second Year\621 Labor\Assignments\Assignment_3\solutions\hw3_621_log.log" "C:\Users\Nmath_000\Documents\MI_school\Second Year\621 Labor\Assignments\Assignment_3\solutions\hw3_621_log.pdf", translator(log2pdf)









