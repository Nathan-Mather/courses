
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
  
  
 local x_vars =  "kidslt6 kidsge6 age educ age_sq age_cu educ_sq educ_cu" 
local x_vars "`x_vars' age_educ age_sq_educ age_educ_sq unem city motheduc fatheduc "
 
 
 program myprobit
version 14
args lnf theta1
quietly replace `lnf' = ln(normal(`theta1')) if $ML_y1==1
quietly replace `lnf' = ln(normal(-`theta1')) if $ML_y1==0
end

ml model lf myprobit (beta: working = `x_vars')
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


ml model lf myolsnorm (beta: hours = `x_vars') (lnsigma:)
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


ml model lf mytunc_olsnorm (beta: hours = `x_vars') (lnsigma:)
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


ml model lf mytobit (beta: hours = `x_vars') (lnsigma:)
ml search
ml maximize


********
* Q1 e *
********

* replace missing hours with something else i guess 
replace lwage = -99 if missing(lwage)

* define wage vars 
local lwage_xv =  "age educ age_sq age_cu educ_sq educ_cu" 
local lwage_xv "`x_vars' age_educ age_sq_educ age_educ_sq unem city motheduc fatheduc "
 

program my_heckman
version 14
args lnf theta1 theta2 lnsigma atanhp
tempvar rho
gen double `rho' = tanh(`atanhp')
gen double `sigma' = exp(`lnsigma')

quietly replace `lnf' = ln(normal(-`theta1')) if $ML_y2 <= 0 & $ML_y1 == -99




ml model lf myheckman (beta: y = x1 x2 x3 ... xn) (pi: s = w1 w2 w3 ... wn) (lnsigma:) (atanhp:)


