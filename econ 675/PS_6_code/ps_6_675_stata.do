clear all
set more off, perm

cap log close

log using "C:\Users\Nmath_000\Documents\Code\courses\econ 675\ps_6_tex\pset6_stata.smcl", replace

*******************************************************
*** Q2: The Effect of Head Start on Child Mortality ***
*******************************************************
use "C:\Users\Nmath_000\Documents\MI_school\Second Year\675 Applied Econometrics\hw\hw6\HeadStart.dta", clear
cd "C:\Users\Nmath_000\Documents\Code\courses\econ 675\ps_6_tex\"

global y mort_related_post
global z mort_injury_post
global yf mort_related_pre
global x povrate60 
gen treat = ($x > 0)
forvalues p = 0/6 {
gen p`p' = $x^`p'
gen tp`p' = $x^`p'*treat
gen up`p' = $x^`p'*(1-treat)
}
order povrate60 mort* treat* p* t* u*

* Q2.1.1 RD Plots

* Evenly spaced bins, IMSE-optimal
rdplot $yf $x, c(0) binselect(es) ///
    graph_options(title("Evenly-spaced binning, IMSE-optimal")) 

graph save temp1.gph, replace

* Quantile-spaced bins, IMSE-optimal
rdplot $yf $x, c(0) binselect(qs) ///
    graph_options(title("Quantile-spaced binning, IMSE-optimal")) 

graph save temp2.gph, replace


* Evenly spaced bins, IMSE-optimal
rdplot $yf $x, c(0) binselect(esmv) ///
    graph_options(title("Evenly-spaced binning, Minimum-variance")) 

graph save temp3.gph, replace

* Quantile-spaced bins, IMSE-optimal
rdplot $yf $x, c(0) binselect(qsmv) ///
    graph_options(title("Quantile-spaced binning, Minimum-variance")) 

graph save temp4.gph, replace

* Now combine all graphs

gr combine temp1.gph temp2.gph ///
			temp3.gph temp4.gph, col(2) iscale(.5)

graph export $resdir/q211a_stata.png, replace

* Q2.1.2 Falsification Tests	   
* Histograms
twoway (hist $x if treat, freq width(2) bcolor("0 100 0 0")) ///
	   (hist $x if !treat, freq width(2) bcolor("100 0 0 0") xline(0)), ///
	   legend(label(1 "Treated") label(2 "Untreated"))

graph export $resdir/q211b_stata.png, replace

* Local Randomization
rdwinselect $x 

* Continuity in Density
rddensity $x

*/
* Q2.2 Global and Flexible Parametric Methods

* 2.2.1
eststo clear
* Run regressions, save beta and se, graph residuals
forvalues pol = 3/6 {
eststo: reg $y treat p1-p`pol', vce(hc2)
capture drop pred
predict pred
twoway scatter pred $x, title("Order `pol'")
graph save temp`pol'.gph, replace
}

* Export graph
graph combine temp3.gph temp4.gph ///
	temp5.gph temp6.gph, col(2) iscale(.5)

graph export pset6_q221_stata.png, replace

* Export table
esttab using table_q221_stata.tex, b se keep(treat) ///
		noobs nostar nonote mtitles("p:3" "p:4" "p:5" "p:6") nonumbers replace	
		

******************
* 2.2.2	 
******************
eststo clear

* Run regressions, save beta and se, graph residuals
forvalues pol = 3/6 {
eststo: reg $y treat tp1-tp`pol' up1-up`pol', vce(hc2)
capture drop pred
predict pred
twoway scatter pred $x, title("Order `pol'")
graph save temp`pol'.gph, replace
}

* Export graph
graph combine temp3.gph temp4.gph ///
	temp5.gph temp6.gph, col(2) iscale(.5)

graph export pset6_q222_stata.png, replace

* Export table
esttab using pset6_q222_stata.tex,  se keep(treat) ///
		noobs nostar nonote mtitles("p:3" "p:4" "p:5" "p:6") nonumbers replace	

******************
* 2.2.3	 
******************

* Run regressions, save beta and se, graph residuals
foreach h of numlist 1 5 9 18 {
eststo clear
forvalues pol = 0/2 {
eststo: reg $y treat p0-p`pol' if abs($x) < `h'
capture drop pred
predict pred
twoway scatter pred $x, title("Order `pol', h = `h'")
graph save temp`h'`pol'.gph, replace
}
* Export table
esttab using pset6_q223h`h'_stata.tex, b se keep(treat) ///
		noobs nostar nonote mtitles("p:0" "p:1" "p:2") nonumbers replace	
}

* Export graph
graph combine temp10.gph temp11.gph temp12.gph ///
				temp50.gph temp51.gph temp52.gph ///
				temp90.gph temp91.gph temp92.gph ///
				temp180.gph temp181.gph temp182.gph, ///
               col(3) iscale(.5)

graph export pset6_q223_stata.png, replace
	

* Q2.3.1
eststo clear
eststo: rdrobust $y $x, p(0) q(1) all
eststo: rdrobust $y $x, p(1) q(2) all
eststo: rdrobust $y $x, p(2) q(3) all

esttab using pset6_q231_stata.tex, b ci ///
		noobs nostar nonote nonumbers replace mtitles("p:0" "p:1" "p:2")

* Q2.3.2a
rdrobust $yf $x, p(0) q(1) all
rdrobust mort_injury_post $x, p(0) q(1) all
di "Looks like there is no effect on the placebo outcomes"


* Q2.3.2b
foreach k in tri uni epa {
	eststo clear
	
	foreach h of numlist 1/10 {
		eststo: rdrobust $y $x, p(1) q(2) h(`h') kernel(`k') all
	}

		esttab using pset6_q232b`k'_stata.tex, b ///
			noobs nostar nonote nomtitles replace	
}
* Q2.3.2c
sort order

eststo clear
forvalues l = 1/10 {
	eststo: rdrobust $y $x if _n > `l', p(1) q(2) all
}

	esttab using pset6_q232c_stata.tex, b ///
		noobs nostar nonote nomtitles replace	
* Q2.3.2d
eststo clear
forvalues c = -10(2)10 {
	eststo: rdrobust $y $x, p(1) q(2) c(`c') all
}
	esttab using pset6_q232d_stata.tex, ///
			noobs nostar nonote nomtitles replace	

******************
* 2.4	 
******************

* Q2.4.3
eststo clear
forvalues w = .8(.2)2.6 {
	rdrandinf $y $x, wl(-`w') wr(`w') seed(123)
//	estadd scalar beta_bc = e(tau_bc)
//	estimates store m`l'
}
	esttab using $resdir/pset6_q243_stata.tex, ///
			nonote replace	



log close	

translate pset6_stata.smcl pset6_stata.pdf
