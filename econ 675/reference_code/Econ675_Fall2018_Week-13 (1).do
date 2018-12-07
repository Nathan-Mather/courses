********************************************************************************
** Econ-675: Applied Microeconometrics, Fall 2018
** Week 13: Regression Discontinuity Designs
** Author: Matias D. Cattaneo
** Last update: 20-Nov-2018
********************************************************************************
** RDROBUST: net install rdrobust, from(https://sites.google.com/site/rdpackages/rdrobust/stata) replace
** RDLOCRAND: net install rdlocrand, from(https://sites.google.com/site/rdpackages/rdlocrand/stata) replace
** RDDENSITY: net install rddensity, from(https://sites.google.com/site/rdpackages/rddensity/stata) replace
** RDMULTI: net install rdmulti, from(https://sites.google.com/site/rdpackages/rdmulti/stata) replace
** RDPOWER: net install rdpower, from(https://sites.google.com/site/rdpackages/rdpower/stata) replace
********************************************************************************
clear all
set more off
set linesize 200

** Load Data: U.S. Senate
use "C:\Users\Nmath_000\Documents\MI_school\Second Year\675 Applied Econometrics\hw\hw6\senate.dta", clear
gl x demmv
gl y demvoteshfor2

********************************************************************************
** Summary Stats & Diff-in-means
********************************************************************************
sum $y $x demvoteshlag1 demvoteshlag2 dopen
gen T= ($x>=0)
ttest $y, by(T)

********************************************************************************
** RD Plots
********************************************************************************
** Mimicking variance
rdplot $y $x

** IMSE-optimal
rdplot $y $x, binselect(es)

** Undersmoothed & graphs options
local tmp = 300
rdplot $y $x, ///
    numbinl(`tmp') numbinr(`tmp') p(4) ///
    graph_options(title(RD Plot - Senate Elections Data) ///
                  ytitle(Vote Share in Election at time t+1) ///
                  xtitle(Vote Share in Election at time t))

********************************************************************************
** Fasification, Validation
********************************************************************************
** Density Test
twoway (histogram $x if $x < 0, bcolor(red)) ///
       (histogram $x if $x >=0, bcolor(ltblue))

** Binomial Test
tab T if abs($x) <= 1
bitesti 46 28 1/2

** Using rdwinselect
rdwinselect $x 
rdwinselect $x, wmin(0.5) wstep(0.25) nwindows(20)

** Manipulation test
rddensity $x
rddensity $x, all

** Covariates
** z = demvoteshlag1 | demvoteshlag2 | dopen
gl z demvoteshlag1
rdplot $z $x if abs($x) <= 100, ///
    numbinl(100) numbinr(100) p(1)  ///
    graph_options(title(RD Plot - Senate Elections Data) ///
                  ytitle(Vote Share in Election at time t-1) ///
                  xtitle(Vote Share in Election at time t))

ttest $z if abs($x) <= 1, by(T)

** RDWINSELECT command
rdwinselect $x $z, nwindows(20) approx

global z demvoteshlag1 demvoteshlag2 dopen
rdwinselect $x $z

********************************************************************************
** Global polynomial approach
********************************************************************************
** Partially linear
*drop x2-x5
gen x2 = $x^2
gen x3 = $x^3
gen x4 = $x^4
gen x5 = $x^5

reg $y T $x x2 x3 x4 x5 

** Full interaction
*drop Tx1-Tx5
gen Tx1 = T*$x
gen Tx2 = T*$x^2
gen Tx3 = T*$x^3
gen Tx4 = T*$x^4
gen Tx5 = T*$x^5

*drop Cx1-Cx5
gen Cx1 = (1-T)*$x
gen Cx2 = (1-T)*$x^2
gen Cx3 = (1-T)*$x^3
gen Cx4 = (1-T)*$x^4
gen Cx5 = (1-T)*$x^5

reg $y T $x Tx1 Tx2 Tx3 Tx4 Tx5 ///
           Cx1 Cx2 Cx3 Cx4 Cx5

rdplot $y $x, ///
    numbinl(100) numbinr(100) p(5) ///
    graph_options(title(RD Plot - Senate Elections Data) ///
                  ytitle(Vote Share in Election at time t+1) ///
                  xtitle(Vote Share in Election at time t))

*** Covariate Balance
global z demvoteshlag1
reg $z T $x Tx1 Tx2 Tx3 Tx4 Tx5 ///
            Cx1 Cx2 Cx3 Cx4 Cx5

rdrobust $z $x, h(10) kernel(uni)

********************************************************************************
** Local Polynomial approach
********************************************************************************
** Using linear models
reg $y T $x Tx1 Cx1 if abs($x) <= 10, robust

** Using rdrobust
rdrobust $y $x, h(10) kernel(uni)

** Plot data in window
rdplot $y $x if abs($x) <= 10, nbins(100) p(1)  ///
    graph_options(title(RD Plot - Senate Elections Data) ///
                  ytitle(Vote Share in Election at time t+1) ///
                  xtitle(Vote Share in Election at time t))

** Bandwidth selection
rdbwselect $y $x
rdbwselect $y $x, all

********************************************************************************
** Local-randomization approach
********************************************************************************
tab T if abs($x) < 1
** Diff-in-means
ttest $y if abs($x) < 1, by(T)
** Linear reg
reg $y T $x Tx1 Cx1 if abs($x) <= 1

** Plot data in window
rdplot $y $x if abs($x) <= 1, ///
    numbinl(100) numbinr(100) p(0)  ///
    graph_options(title(RD Plot - Senate Elections Data) ///
                  ytitle(Vote Share in Election at time t+1) ///
                  xtitle(Vote Share in Election at time t))

** By hand
permute T diffmean=(r(mu_2)-r(mu_1)), reps(1000) nowarn: ttest $y if abs($x)<=1, by(T)

** Randomization inference in W=[-1,1] on placebos
rdrandinf demvoteshlag1 $x, wr(1) wl(-1) reps(1000)
rdrandinf demvoteshlag2 $x, wr(1) wl(-1) reps(1000)

** Remember to have enough repetitions
rdrandinf demvoteshlag2 $x, wr(1) wl(-1) reps(10) 
rdrandinf demvoteshlag2 $x, wr(1) wl(-1) reps(10) seed(4)
rdrandinf demvoteshlag2 $x, wr(1) wl(-1) reps(10) seed(45)
rdrandinf demvoteshlag2 $x, wr(1) wl(-1) reps(10) seed(456)
rdrandinf demvoteshlag2 $x, wr(1) wl(-1) reps(1000) 

* Power is naturally low because of small sample size
rdrandinf demvoteshlag1 $x, wr(1) wl(-1) reps(1) 
rdrandinf demvoteshlag1 $x, wr(1) wl(-1) reps(1) d(6.335)


permute T diffmean=(r(mu_2)-r(mu_1)), reps(1000) nowarn: ttest demvoteshlag1 if abs($x)<=1, by(T) 


** Now select window based on covariates
rdwinselect $x dopen population presdemvoteshlag1, wmin(0.5) wstep(0.125)

rdwinselect $x dopen population presdemvoteshlag1 demvoteshlag1 ///
    demvoteshlag2 demwinprv1 demwinprv2 dmidterm dpresdem, ///
    wmin(0.5) wstep(0.125) reps(100)

** Plot results
rdwinselect $x dopen population presdemvoteshlag1 demvoteshlag1 ///
    demvoteshlag2 demwinprv1 demwinprv2 dmidterm dpresdem, ///
    wmin(0.5) wstep(0.125) nwindows(10) reps(100) plot
	
** Plot outcome
rdplot $y $x if abs($x)<=1, nbins(50) p(1)

** Plot outcome in [-0.75,0.75]	
* linear
rdplot $y $x if abs($x)<=0.75, nbins(50) p(1)
* constant
rdplot $y $x if abs($x)<=0.75, nbins(50) p(0)

** Manual rdrandinf
rdrandinf $y $x, wl(-0.75) wr(0.75) reps(1000) stat(all)

** Automatic rdrandinf
rdrandinf $y $x, cov(dopen population presdemvoteshlag1 demvoteshlag1 ///
    demvoteshlag2 demwinprv1 demwinprv2 dmidterm dpresdem) ///
	wmin(0.5) wstep(0.125) reps(1000) rdwreps(50)

** Other statisics
rdrandinf $y $x, cov(dopen population presdemvoteshlag1 demvoteshlag1 ///
    demvoteshlag2 demwinprv1 demwinprv2 dmidterm dpresdem) stat(all) 


********************************************************************************
** Empirical Illustration: Ludwig and Miller (2007, QJE)
********************************************************************************
clear all

** Load Data
use headstart, clear
des
gl y mort_age59_related_postHS
gl x povrate60
gl z census1960_pop census1960_pctsch1417 census1960_pctsch534 ///
     census1960_pctsch25plus census1960_pop1417 census1960_pop534 ///
	 census1960_pop25plus census1960_pcturban census1960_pctblack

** Summary Stats
sum $y $x
gen T = ($x<=59.1984)
ttest $y, by(T)

** Plot the data
rdplot $y $x, c(59.1984)
rdplot $y $x, c(59.1984) nbins(100)

** Balance Tests
twoway (histogram $x if $x<59.1984, freq width(3) bcolor(red)) ///
       (histogram $x if $x>=59.1984, freq width(3) bcolor(blue) xline(59.1984))

tab T if abs($x-59.1984) <= 1
bitesti 65 35 1/2

rdwinselect $x, c(59.1984)

rdrobust census1960_pop $x, c(59.1984) h(9) kernel(uni)
rdrobust census1960_pctsch534 $x, c(59.1984) h(9) kernel(uni)


** Local Polynomial Approach
rdrobust $y $x, c(59.1984) h(9) kernel(uniform) vce(hc0)

rdrobust $y $x, c(59.1984) h(9) kernel(uni)
rdrobust $y $x, c(59.1984) h(9) kernel(uni) vce(hc1)
rdrobust $y $x, c(59.1984) h(9) kernel(uni) vce(hc2)

rdbwselect $y $x, c(59.1984) kernel(uni)

rdrobust $y $x, c(59.1984) bwselect(msetwo) kernel(uni)

rdrobust $y $x, c(59.1984) bwselect(mserd) kernel(uni)

rdrobust $y $x, c(59.1984) bwselect(msetwo) kernel(uni)

rdrobust $y $x, c(59.1984) bwselect(cerrd) kernel(uni)

** RDROBUST with covariates
qui rdrobust $y $x, c(59.1984)
local len = `e(ci_r_rb)' - `e(ci_l_rb)'
rdrobust $y $x, c(59.1984) covs($z)
display "CI length reduction: " round(((`e(ci_r_rb)'-`e(ci_l_rb)')/`len'-1)*100,.01) "%"

** Local Randomization Approach
rdwinselect $x census1960_pop census1960_pcturban census1960_pctblack, ///
            c(59.1984)

rdrandinf $y $x, c(59.1984) wl(57) wr(61) reps(1000)

rdrandinf $y $x, c(59.1984) wl(57) wr(61) reps(1000) stat(all)


********************************************************************************
** Power Calculations and Sampling Design
********************************************************************************
use senate, clear

rdpower demvoteshfor2 demmv
rdpower demvoteshfor2 demmv, plot
rdpower demvoteshfor2 demmv, plot bwselect(msetwo)

** rdpower against tau = 5
rdpower demvoteshfor2 demmv, tau(5)


** rdpower with covariates
rdpower demvoteshfor2 demmv, tau(5) covs(population dopen dmidterm)


** rdpower with user-specified plot options
rdpower demvoteshfor2 demmv, tau(5) plot graph_range(-9 9) graph_step(2) ///
		                     graph_options(title(Power function) ///
							 xline(0, lcolor(black) lpattern(dash)) ///
							 yline(.05, lpattern(shortdash) lcolor(black)) ///
							 xtitle(tau) ytitle(power) ///
							 graphregion(fcolor(white))) 

							 
** rdpower with rdrobust options
rdpower demvoteshfor2 demmv, tau(5) h(16 18) b(18 20)
rdpower demvoteshfor2 demmv, kernel(uniform) vce(cluster state)
rdpower demvoteshfor2 demmv, bwselect(certwo) vce(hc3) scaleregul(0) rho(1)


** rdpower with conventional inference
rdpower demvoteshfor2 demmv, tau(5) all


** rdpower with user specified bias and variance
qui rdrobust demvoteshfor2 demmv

local samph = e(h_l)
local sampsi_l = e(N_h_l)
local sampsi_r = e(N_h_r)

local bias_l = e(bias_l)/e(h_l)
local bias_r = e(bias_r)/e(h_r)

mat VL_RB = e(V_rb_l)
mat VR_RB = e(V_rb_r)

local Vl_rb = e(N)*e(h_l)*VL_RB[1,1]
local Vr_rb = e(N)*e(h_r)*VR_RB[1,1]

rdpower demvoteshfor2 demmv, tau(5) bias(`bias_l' `bias_r') ///
                             var(`Vl_rb' `Vr_rb') ///
							 samph(`samph') sampsi(`sampsi_l' `sampsi_r')


** rdpower manually increasing variance by 20%
qui rdrobust demvoteshfor2 demmv

mat VL_RB = e(V_rb_l)
mat VR_RB = e(V_rb_r)

local Vl_rb = e(N)*e(h_l)*VL_RB[1,1]*1.2
local Vr_rb = e(N)*e(h_r)*VR_RB[1,1]*1.2

rdpower demvoteshfor2 demmv, tau(5) var(`Vl_rb' `Vr_rb')


** rsampsi with tau = 5
rdsampsi demvoteshfor2 demmv, tau(5)


** rsampsi with tau = 5 setting bandwdith and nratio with plot
rdsampsi demvoteshfor2 demmv, tau(5) beta(.9) samph(18 19) nratio(.5) plot


** rsampsi with conventional inference
rdsampsi demvoteshfor2 demmv, tau(5) all


** rsampsi vs rdpower
qui rdsampsi demvoteshfor2 demmv, tau(5)
rdpower demvoteshfor2 demmv, tau(5) sampsi(r(sampsi_h_l) r(sampsi_h_r))


** rsampsi without data
qui rdsampsi demvoteshfor2 demmv, tau(5)
local init = r(init_cond)
rdsampsi, tau(5) nsamples(r(N_l) r(N_h_l) r(N_r) r(N_h_r)) ///
				 bias(r(bias_l) r(bias_r)) ///
				 var(r(var_l) r(var_r)) ///
				 samph(r(samph_l) r(samph_r)) ///
				 init_cond(`init')



** Using HEAD START data
use headstart, clear

rdpower mort_age59_related_postHS povrate60, c(59.1984)
rdpower mort_age59_related_postHS povrate60, c(59.1984) plot
rdpower mort_age59_related_postHS povrate60, c(59.1984) plot bwselect(msetwo)


