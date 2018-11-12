

********************************************************************************
* Preliminaries
********************************************************************************
clear all
set more off


********************************************************************************
* Import data, create additional covariates
********************************************************************************

* Import LaLonde data
import delimited using "C:\\Users\Nmath_000\Documents\MI_school\Second Year\675 Applied Econometrics\hw\hw4\LaLonde_all.csv"


* set directory 
cd "C:\Users\Nmath_000\Documents\Code\courses\econ 675\PS_4_tex\"
* Generate additional covariates 
gen log_re74 = log(re74+1)
gen log_re75 = log(re75+1)
gen age_sq   = age^2
gen age_cu   = age^3
gen educ_sq  = educ^2
gen black_u74 = black*u74
gen educ_log_re74 = educ*log_re74
gen treat2    = treat if treat==1|treat==2
replace treat2=0 if treat2==2

********************************************************************************
* [1] Difference in means
********************************************************************************

* Lalonde control
reg re78 treat if treat==1|treat==0 , hc2

* PSID control
reg re78 treat if treat==1|treat==2 , hc2

********************************************************************************
* [2] OLS
********************************************************************************

* Covariates A, Lalonde control
reg re78 treat age educ black hisp married nodegr log_re74 log_re75 if treat==1|treat==0 , hc2

* Covariates B, Lalonde control
reg re78 treat age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 if treat==1|treat==0 , hc2

* Covariates C, Lalonde control
reg re78 treat age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74 if treat==1|treat==0 , hc2

* Covariates A, PSID
reg re78 treat age educ black hisp married nodegr log_re74 log_re75 if treat==1|treat==2 , hc2

* Covariates B, PSID
reg re78 treat age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 if treat==1|treat==2 , hc2

* Covariates C, PSID
reg re78 treat age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74 if treat==1|treat==2 , hc2


********************************************************************************
* [3] Regression Imputation 
********************************************************************************

* Covariates A, Lalonde control
teffects ra (re78 age educ black hisp married nodegr log_re74 log_re75) (treat) if treat==1|treat==0 , ate 
teffects ra (re78 age educ black hisp married nodegr log_re74 log_re75) (treat) if treat==1|treat==0 , atet

* Covariates B, Lalonde control
teffects ra (re78 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75) (treat) if treat==1|treat==0 , ate 
teffects ra (re78 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75) (treat) if treat==1|treat==0 , atet 

* Covariates C, Lalonde control
teffects ra (re78 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74) (treat) if treat==1|treat==0 , ate 
teffects ra (re78 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74) (treat) if treat==1|treat==0 , atet 


* Covariates A, PSID control
eststo ri1: teffects ra (re78 age educ black hisp married nodegr log_re74 log_re75) (treat2) if treat2==1|treat2==0 , ate 
eststo ri2: teffects ra (re78 age educ black hisp married nodegr log_re74 log_re75) (treat2) if treat2==1|treat2==0 , atet

* Covariates B, PSID control
teffects ra (re78 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75) (treat2) if treat2==1|treat2==0 , ate 
eststo ri3: teffects ra (re78 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75) (treat2) if treat2==1|treat2==0 , atet 

* Covariates C, PSID control
eststo ri4: teffects ra (re78 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74) (treat2) if treat2==1|treat2==0 , ate 
eststo ri5: teffects ra (re78 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74) (treat2) if treat2==1|treat2==0 , atet 

esttab ri1 using Q2_atematch.csv, se nostar keep(r1vs0.treat2) wide noparentheses nonumber noobs plain nomtitles replace
esttab ri2 ri3 ri4 using Q2_att.csv, se nostar keep(r1vs0.treat2) wide noparentheses nonumber noobs plain nomtitles replace

********************************************************************************
* [4] IPW
********************************************************************************

* Covariates A, Lalonde control
teffects ipw (re78) (treat age educ black hisp married nodegr log_re74 log_re75, logit) if treat==1|treat==0 , ate 
teffects ipw (re78) (treat age educ black hisp married nodegr log_re74 log_re75, logit) if treat==1|treat==0 , atet 

* Covariates B, Lalonde control
teffects ipw (re78) (treat age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75, logit) if treat==1|treat==0 , ate 
teffects ipw (re78) (treat age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75, logit) if treat==1|treat==0 , atet

* Covariates C, Lalonde control
teffects ipw (re78) (treat age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74, logit) if treat==1|treat==0 , ate 
teffects ipw (re78) (treat age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74, logit) if treat==1|treat==0 , atet 

* Covariates A, PSID control [doesn't converge, so set maxiter = 50!!!]
eststo i1: teffects ipw (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75, logit) if treat2==1|treat2==0 , ate iterate(25)
eststo i2: teffects ipw (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75, logit) if treat2==1|treat2==0 , atet iterate(25)

* Covariates B, PSID control [first need to drop obs with very low prop scores]
teffects ipw (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75, logit) if treat2==1|treat2==0 , ate osample(viol)
teffects ipw (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75, logit) if treat2==1|treat2==0 & viol==0 , ate iter(25)
eststo i3: teffects ipw (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75, logit) if treat2==1|treat2==0 & viol==0 , atet iter(25)

* Covariates C, PSID control [need to drop people]
teffects ipw (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74, logit) if treat2==1|treat2==0 , ate osample(violl)
teffects ipw (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74, logit) if treat2==1|treat2==0 & violl==0, ate iter(25)
eststo i4: teffects ipw (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74, logit) if treat2==1|treat2==0 , atet iter(25) 

esttab i1 using Q2_atematch.csv, se nostar keep(r1vs0.treat2) wide noparentheses nonumber noobs plain nomtitles append
esttab i2 i3 i4 using Q2_att.csv, se nostar keep(r1vs0.treat2) wide noparentheses nonumber noobs plain nomtitles append

********************************************************************************
* [5] Doubly Robust
********************************************************************************

* Covariates A, Lalonde control
teffects ipwra (re78) (treat age educ black hisp married nodegr log_re74 log_re75, logit) if treat==1|treat==0 , ate 
teffects ipwra (re78) (treat age educ black hisp married nodegr log_re74 log_re75, logit) if treat==1|treat==0 , atet 

* Covariates B, Lalonde control
teffects ipwra (re78) (treat age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75, logit) if treat==1|treat==0 , ate 
teffects ipwra (re78) (treat age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75, logit) if treat==1|treat==0 , atet

* Covariates C, Lalonde control
teffects ipwra (re78) (treat age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74, logit) if treat==1|treat==0 , ate 
teffects ipwra (re78) (treat age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74, logit) if treat==1|treat==0 , atet 

* Covariates A, PSID control
eststo d1: teffects ipwra (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75, logit) if treat2==1|treat2==0 , ate iter(25)
eststo d2: teffects ipwra (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75, logit) if treat2==1|treat2==0 , atet iter(25)

* Covariates B, PSID control
teffects ipwra (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75, logit) if treat2==1|treat2==0 , ate iter(25)
eststo d3: teffects ipwra (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75, logit) if treat2==1|treat2==0 , atet iter(25)

* Covariates C, PSID control
teffects ipwra (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74, logit) if treat2==1|treat2==0 , ate 
eststo d4: teffects ipwra (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74, logit) if treat2==1|treat2==0 , atet iter(25)

esttab d1 using Q2_atematch.csv, se nostar keep(r1vs0.treat2) wide noparentheses nonumber noobs plain nomtitles append
esttab d2 d3 d4 using Q2_att.csv, se nostar keep(r1vs0.treat2) wide noparentheses nonumber noobs plain nomtitles append

********************************************************************************
* [6] Nearest Neighbour Matching
********************************************************************************

* Covariates A, Lalonde control
eststo n1: teffects nnmatch (re78 age educ black hisp married nodegr log_re74 log_re75) (treat) if treat==1|treat==0 , ate nneighbor(1) metric(maha)
eststo n2: teffects nnmatch (re78 age educ black hisp married nodegr log_re74 log_re75) (treat) if treat==1|treat==0 , atet nneighbor(1) metric(maha)

* Covariates B, Lalonde control
eststo n3: teffects nnmatch (re78 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75) (treat) if treat==1|treat==0 , ate nneighbor(1) metric(maha)
eststo n4: teffects nnmatch (re78 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75) (treat) if treat==1|treat==0 , atet nneighbor(1) metric(maha)

* Covariates C, Lalonde control
eststo n5: teffects nnmatch (re78 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74) (treat) if treat==1|treat==0 , ate nneighbor(1) metric(maha)
eststo n6: teffects nnmatch (re78 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74) (treat) if treat==1|treat==0 , atet nneighbor(1) metric(maha)

* Covariates A, PSID control
eststo n7: teffects nnmatch (re78 age educ black hisp married nodegr log_re74 log_re75) (treat2) if treat2==1|treat2==0 , ate nneighbor(1) metric(maha)
eststo n8:teffects nnmatch (re78 age educ black hisp married nodegr log_re74 log_re75) (treat2) if treat2==1|treat2==0 , atet nneighbor(1) metric(maha)

* Covariates B, PSID control
eststo n9:teffects nnmatch (re78 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75) (treat2) if treat2==1|treat2==0 , ate nneighbor(1) metric(maha)
eststo n10:teffects nnmatch (re78 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75) (treat2) if treat2==1|treat2==0 , atet nneighbor(1) metric(maha)

* Covariates C, PSID control
eststo n11:teffects nnmatch (re78 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74) (treat2) if treat2==1|treat2==0 , ate nneighbor(1) metric(maha)
eststo n12:teffects nnmatch (re78 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74) (treat2) if treat2==1|treat2==0 , atet nneighbor(1) metric(maha)

esttab n7 using Q2_atematch.csv, se nostar keep(r1vs0.treat2) wide noparentheses nonumber noobs plain nomtitles append
esttab n8 n10 n12 using Q2_att.csv, se nostar keep(r1vs0.treat2) wide noparentheses nonumber noobs plain nomtitles append

********************************************************************************
* [7] PS matching
********************************************************************************

* Covariates A, Lalonde control
eststo p1: teffects psmatch (re78) (treat age educ black hisp married nodegr log_re74 log_re75, logit) if treat==1|treat==0 , ate 
eststo p2: teffects psmatch (re78) (treat age educ black hisp married nodegr log_re74 log_re75, logit) if treat==1|treat==0 , atet 

* Covariates B, Lalonde control
eststo p3: teffects psmatch (re78) (treat age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75, logit) if treat==1|treat==0 , ate 
eststo p4: teffects psmatch (re78) (treat age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75, logit) if treat==1|treat==0 , atet

* Covariates C, Lalonde control
eststo p5: teffects psmatch (re78) (treat age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74, logit) if treat==1|treat==0 , ate 
eststo p6: teffects psmatch (re78) (treat age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74, logit) if treat==1|treat==0 , atet 

* Covariates A, PSID control
eststo p7:teffects psmatch (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75, logit) if treat2==1|treat2==0 , ate 
eststo p8:teffects psmatch (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75, logit) if treat2==1|treat2==0 , atet 

* For the PSID samples below there are some prop scores too close to 1.
* First I need to run the treat2ment models, identify the respondents w/ problematic prop scores -- this will cause the code to break
* Then I drop the violators and estimate the treat2ment effects
teffects psmatch (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75, logit) if treat2==1|treat2==0 , ate osample(viol2) 
teffects psmatch (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74, logit) if treat2==1|treat2==0, ate osample(viol3)


* Covariates B, PSID control
eststo p9:teffects psmatch (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75, logit) if treat2==1|treat2==0 & viol2==0 , ate
eststo p10:teffects psmatch (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75, logit) if treat2==1|treat2==0 & viol2==0, atet 

* Covariates C, PSID control
eststo p11: teffects psmatch (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74, logit) if treat2==1|treat2==0 & viol3==0 , ate 
eststo p12: teffects psmatch (re78) (treat2 age educ black hisp married nodegr log_re74 log_re75 age_sq educ_sq u74 u75 age_cu black_u74 educ_log_re74, logit) if treat2==1|treat2==0 & viol3==0 , atet 

esttab p1 p3 p5 p7 p9 n11 using Q2_atematch.csv, se nostar keep(r1vs0.treat r1vs0.treat2) wide noparentheses nonumber noobs plain nomtitles append
esttab p8 p10 p12 using Q2_att.csv, se nostar keep(r1vs0.treat2) wide noparentheses nonumber noobs plain nomtitles append





********************************************************************************
* Preliminaries
********************************************************************************
clear all
set more off

* Set working directory 
global dir "/Users/Anirudh/Desktop/GitHub"


set seed 22
set obs 50

********************************************************************************
* [1] Summary stats and density plots
********************************************************************************

* number of replications
local M = 1000
set matsize 11000

* empty matrices to store estimates and indicator of coverage
matrix est = J(`M',3,.)
matrix cov = J(`M',3,.)

* initial values we will replace during replication
gen x = rnormal(0,1) 
gen z = .85*x + sqrt(1-.85)*rnormal(0,1)
gen eps = rnormal(0,1)
gen y = 1 + .5*x + z + eps

* loop for M replications
forvalues i = 1/`M'{
	qui replace x = rnormal(0,1) 
	qui replace z = .85*x + sqrt(1-.85)*rnormal(0,1)
	qui replace eps = rnormal(0,1)
	qui replace y = 1 + .5*x + z + eps
	
	* long regression
	qui reg y x z, r
	
	* extract first estimate
	local beta_hat = _b["x"]
	matrix est[`i',1] = `beta_hat'
	
	* get SE and calculate coverage of true beta_0 = .5
	local se_hat = _se["x"]
	local lb_hat = `beta_hat' - 1.96 * `se_hat'
	local ub_hat = `beta_hat' + 1.96 * `se_hat'
	local cov_hat = (.5 >= `lb_hat') & (.5 <= `ub_hat')
	matrix cov[`i',1] = `cov_hat'
	
	* save gamma over se gamma
	local gamma_hat = _b["z"]
	local gamma_se  = _se["z"]
	local tstat = `gamma_hat'/`gamma_se'
		
	* short regression
	qui reg y x, r
	local beta_tilde = _b["x"]
	matrix est[`i',2] = `beta_tilde'
	
	* get SE and calculate coverage of true beta_0 = .5
	local se_tilde = _se["x"]
	local lb_tilde = `beta_tilde' - 1.96 * `se_tilde'
	local ub_tilde = `beta_tilde' + 1.96 * `se_tilde'
	local cov_tilde = (.5 >= `lb_tilde') & (.5 <= `ub_tilde')
	matrix cov[`i',2] = `cov_tilde'
	
	* third estimate
	local beta_check = cond(`tstat' >= 1.96, `beta_hat', `beta_tilde')	
	matrix est[`i',3] = cond(`tstat' >= 1.96, `beta_hat', `beta_tilde')	
	matrix cov[`i',3] = cond(`tstat' >= 1.96, `cov_hat', `cov_tilde') 
}

* turn results into variables
svmat est
svmat cov

* drop old data
drop x
drop z
drop eps
drop y

* rename variables
rename est1 beta_hat
rename est2 beta_tilde
rename est3 beta_check
rename cov1 cov_hat
rename cov2 cov_tilde
rename cov3 cov_check

* write summary statistics to latex
outreg2 using q3.tex, replace sum(log) ///
	keep(beta_hat beta_tilde beta_check) ///
	eqkeep(min mean median max) ///
	dec(2)

* kernel densities
twoway kdensity beta_hat, k(epanechnikov) || ///
 kdensity beta_tilde, k(epanechnikov) || ///
 kdensity beta_check, k(epanechnikov) ///
 leg(lab(1 "beta_hat") lab(2 "beta_tilde") lab(3 "beta_check")) ///
 ytitle("Density") xtitle("")
	 

********************************************************************************
* [2] Coverage rates
********************************************************************************

* calculate these here, report them in LaTeX
sum(cov_hat)
sum(cov_tilde)
sum(cov_check)
