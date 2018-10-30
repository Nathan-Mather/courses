******************************************
* PS 3 675 metrics 
*Nate Mather 
*Stata section 
******************************************

clear
**************
* Question 1 *
**************
* load data 
use "C:\Users\Nmath_000\Documents\MI_school\Second Year\675 Applied Econometrics\hw\hw3\pisofirme.dta",clear

* set wd 
cd "C:\Users\Nmath_000\Documents\Code\courses\econ 675\PS_3_tex"

gen s = 1-dmissing
gen log_inc = ln(S_incomepc + 1)

*******
*part1*
*******

logit s S_age S_HHpeople log_inc, vce(robust)

* output for LaTeX
outreg2 using stata_tab_q1_9_a.tex, side stats(coef se tstat pval ci) ///
 noaster noparen nor2 noobs dec(3) replace tex(frag)
 
 *******
*part 2*
********

* nonparametric bootstrap
logit s S_age S_HHpeople log_inc, vce(bootstrap, reps(999))

* output for LaTeX
outreg2 using stata_table_q1_9_b.tex, side stats(coef se tstat pval ci) ///
 noaster noparen nor2 noobs dec(3) replace  tex(frag)

* Q1.9c - propensity scores
* logit regression, robust standard errors
logit s S_age S_HHpeople log_inc, vce(robust)

* predict propensity score
predict p

* plot histogram, overlay kernel density
twoway histogram p || kdensity p, k(gaussian)

	 
* save
graph export stata_plot_q1_9_c.png, replace



*************************************************
**** Question 2

*************************************************


* gmm, four moment conditions
local vars = "dpisofirme S_age S_HHpeople log_inc"
gmm ((danemia - invlogit((dpisofirme*{theta}+S_age*{gamma1}+S_HHpeople*{gamma2}+log_inc*{gamma3})))*dpisofirme) ///
((danemia - invlogit((dpisofirme*{theta}+S_age*{gamma1}+S_HHpeople*{gamma2}+log_inc*{gamma3})))*S_age) ///
((danemia - invlogit((dpisofirme*{theta}+S_age*{gamma1}+S_HHpeople*{gamma2}+log_inc*{gamma3})))*S_HHpeople) ///
((danemia - invlogit((dpisofirme*{theta}+S_age*{gamma1}+S_HHpeople*{gamma2}+log_inc*{gamma3})))*log_inc), ///
instruments(`vars') winitial(identity) vce(boot)
 
* output for LaTeX
 mata: 
	coef = st_matrix("e(b)")'
	se = st_matrix("e(se)")'
	
	tstat = coef:/se
		
	CI_low = coef - 1.96:*se
	CI_high = coef + 1.96:*se
	 
	stats = round((coef,se,tstat,CI_low,CI_high),.001)
		
	st_matrix("stats",stats)
end
mat rownames stats = `vars'
mat colnames stats = coef se tstat CI_low CI_high
outtable using stata_table_q2_2_b, mat(stats) replace nobox

* Q2.3c (MAR)- feasible estimator
* we predicted p before, but did not use t, so do that now:
* logit regression, robust standard errors
logit s dpisofirme S_age S_HHpeople log_inc, vce(robust)

* predict propensity score
predict p_witht

* now run gmm adding in new term s/p
local vars = "dpisofirme S_age S_HHpeople log_inc"
gmm ((s/p_witht)*(danemia - invlogit((dpisofirme*{theta}+S_age*{gamma1}+S_HHpeople*{gamma2}+log_inc*{gamma3})))*dpisofirme) ///
((s/p_witht)*(danemia - invlogit((dpisofirme*{theta}+S_age*{gamma1}+S_HHpeople*{gamma2}+log_inc*{gamma3})))*S_age) ///
((s/p_witht)*(danemia - invlogit((dpisofirme*{theta}+S_age*{gamma1}+S_HHpeople*{gamma2}+log_inc*{gamma3})))*S_HHpeople) ///
((s/p_witht)*(danemia - invlogit((dpisofirme*{theta}+S_age*{gamma1}+S_HHpeople*{gamma2}+log_inc*{gamma3})))*log_inc), ///
instruments(`vars') winitial(identity) vce(boot)

* output for LaTeX
 mata: 
	coef = st_matrix("e(b)")'
	se = st_matrix("e(se)")'
	
	tstat = coef:/se
		
	CI_low = coef - 1.96:*se
	CI_high = coef + 1.96:*se
	 
	stats = round((coef,se,tstat,CI_low,CI_high),.001)
		
	st_matrix("stats",stats)
end
mat rownames stats = `vars'
mat colnames stats = coef se tstat CI_low CI_high
outtable using stata_table_q2_3_c, mat(stats) replace nobox

* Q2.3d (MAR)- feasible estimator, trimmed
* we predicted p before, and have s, so add that before the moment conditions
local vars = "dpisofirme S_age S_HHpeople log_inc"
gmm ((s/p_witht)*(danemia - invlogit((dpisofirme*{theta}+S_age*{gamma1}+S_HHpeople*{gamma2}+log_inc*{gamma3})))*dpisofirme) ///
((s/p_witht)*(danemia - invlogit((dpisofirme*{theta}+S_age*{gamma1}+S_HHpeople*{gamma2}+log_inc*{gamma3})))*S_age) ///
((s/p_witht)*(danemia - invlogit((dpisofirme*{theta}+S_age*{gamma1}+S_HHpeople*{gamma2}+log_inc*{gamma3})))*S_HHpeople) ///
((s/p_witht)*(danemia - invlogit((dpisofirme*{theta}+S_age*{gamma1}+S_HHpeople*{gamma2}+log_inc*{gamma3})))*log_inc) ///
if p_witht >= 0.1, instruments(`vars') winitial(identity) vce(boot)

* output for LaTeX
 mata: 
	coef = st_matrix("e(b)")'
	se = st_matrix("e(se)")'
	
	tstat = coef:/se
		
	CI_low = coef - 1.96:*se
	CI_high = coef + 1.96:*se
	 
	stats = round((coef,se,tstat,CI_low,CI_high),.001)
		
	st_matrix("stats",stats)
end
mat rownames stats = `vars'
mat colnames stats = coef se tstat CI_low CI_high
outtable using stata_table_q2_3_d, mat(stats) replace nobox






********************************
*** Question 3 
********************************

* Q3.1 - nonparametric bootstrap
clear all

* generate sample
set seed 123
set obs 1000
gen X = runiform()

* save actual max
sum X
local maxX=r(max)

* run nonparametric bootstrap of max
bootstrap stat=r(max), reps(599) saving(nonpar_results, replace): summarize X

* load results
use nonpar_results, clear

* generate statistic
gen nonpar_stat = 1000*(`maxX'-stat)

* plot
hist nonpar_stat, ///
 plot(function exponential = 1-exponential(1,x), range(0 5) color(red))
graph export stata_plot_q3_1.png, replace

********************************************************************************
* Q3.2 - parametric bootstrap
clear all

tempname memhold
tempfile para_results

* generate sample
set seed 123
set obs 1000
gen X = runiform()

* save actual max
sum X
local maxX=r(max)

* parametric bootstrap
postfile `memhold' max using `para_results'
forvalues i = 1/599{
	capture drop sample
	gen sample = runiform(0,`maxX')
	sum sample
	post `memhold' (r(max))
}
postclose `memhold'

* load results
use `para_results', clear

* generate statistic
gen para_stat = 1000*(`maxX'-max)

* plot
hist para_stat, ///
 plot(function exponential = 1-exponential(1,x), range(0 5) color(red))
graph export stata_plot_q3_2.png, replace
