* Do file for assignment 1 of pp 713

clear all
set more off, perm

* input directory 
global dir "C:\Users\Nmath_000\Documents\MI_school\Second Year\PP 713\ps1"

* output directory 
global outdir "C:\Users\Nmath_000\Documents\Code\courses\PP 713\ps1_tex\"

* load in data 
use "$dir\ipeds_completions_pp713.dta"


****************************
* Created needed variables *
****************************

* create engdiff 
gen engdiff=(year>=diffyear & diffyear!=. & diff_eng08d==1)
label var engdiff "Engineering tuition differential by year indicator"

* create difference between academic year and year differential was institutied 
gen yearMinusDiffyear = year - diffyear
tab diffyear, m
tab yearMinusDiffyear, m

* now generate variable for  engdiff0_2
gen engdiff_02 = ( yearMinusDiffyear >= 0 & yearMinusDiffyear <= 2 & diff_eng08d == 1)
gen engdiff_3plus = ( yearMinusDiffyear >= 3 & diff_eng08d == 1)


********************************
* run regresson for Question 2 *
********************************
 eststo clear
eststo: reg eng_share engdiff, vce(cluster unitid)


********************************
* run regresson for Question 3 *
********************************
 
eststo: reg eng_share engdiff i.unitid i.year, vce(cluster unitid)

eststo: reg eng_share engdiff i.year, vce(cluster unitid)
eststo: reg eng_share engdiff i.unitid, vce(cluster unitid)

 * xtreg eng_share engdiff_02 engdiff_3plus i.unitid i.year, fe 

********************************
* run regresson for Question 4 *
********************************
eststo: reg eng_share engdiff_02 engdiff_3plus i.unitid i.year , vce(cluster unitid)


eststo: reg eng_share engdiff_02 engdiff_3plus i.unitid i.year if diff_eng08d == 0 | abs(yearMinusDiffyear) <= 4 , vce(cluster unitid) 

**********************
* regression for q 5 *
**********************

eststo: reg eng_share engdiff_02 engdiff_3plus i.inst_division i.unitid i.year if diff_eng08d == 0 | abs(yearMinusDiffyear) <= 4 , vce(cluster unitid) 


***************************
* save regression results *
***************************

esttab using "$outdir\ps1_table.tex", b se r2 keep(engdiff engdiff_02 engdiff_3plus _cons)  /// 
mtitles("(1)" "(2)" "(2)a" "(2)b" "(3)" "(4)" "(5)") nonumbers replace	
		
eststo clear


*******************************
* create vars for event study *
*******************************

gen StartEngDiffpre4 =  (yearMinusDiffyear <= -4 & diff_eng08d == 1)
label var StartEngDiffpre4 "-4"

gen StartEngDiffpre3 = (yearMinusDiffyear == -3 & diff_eng08d == 1)
label var StartEngDiffpre3 "-3"

gen StartEngDiffpre2 = (yearMinusDiffyear == -2 & diff_eng08d == 1)
label var StartEngDiffpre2 "-2"


gen StartEngDiffpre1 = (yearMinusDiffyear == -1 & diff_eng08d == 1)
label var StartEngDiffpre1 "-1"

gen StartEngDiffpost0 = (yearMinusDiffyear == 0 & diff_eng08d == 1)
label var StartEngDiffpost0 "0"

gen StartEngDiffpost1 = (yearMinusDiffyear == 1 & diff_eng08d == 1)
label var StartEngDiffpost1 "1"

gen StartEngDiffpost2 = (yearMinusDiffyear == 2 & diff_eng08d == 1)
label var StartEngDiffpost2 "2"

gen StartEngDiffpost3 = (yearMinusDiffyear == 3 & diff_eng08d == 1)
label var StartEngDiffpost3 "3"

gen StartEngDiffpost4plus =(yearMinusDiffyear >= 4 & diff_eng08d == 1)
label var StartEngDiffpost4plus "4"

***********************
* run event study reg *
***********************
eststo clear
* was going to include bus_share nurse_share  but I realized we dont have the variabes 
* needed to do it 

foreach x of varlist eng_share{

 eststo: reg `x' StartEngDiffpre3 StartEngDiffpre2 StartEngDiffpre1 StartEngDiffpost0 ///
 StartEngDiffpost1 StartEngDiffpost2 StartEngDiffpost3 StartEngDiffpost4plus i.unitid i.year ///
 ///
if diff_eng08d == 0 | abs(yearMinusDiffyear) <= 4 , vce(cluster unitid) 

* make plot 
coefplot, vertical keep(StartEngDiffpre3 StartEngDiffpre2 StartEngDiffpre1 StartEngDiffpost0 ///
StartEngDiffpost1 StartEngDiffpost2 StartEngDiffpost3 StartEngDiffpost4plus) yline(0) ///
 ylabel(-0.04(0.02)0.02) yscale(range(-0.04(0.02)0.02)) ytitle("Coefficient") xtitle("year - year differential enacted")

* save plot 
graph export "$outdir\ps1_figure_`x'.png" , replace


}

**************
* question 9 *
**************

* run regression for question 9 
   eststo: reg eng_share StartEngDiffpre4 StartEngDiffpre3 StartEngDiffpre2 StartEngDiffpost0 ///
 StartEngDiffpost1 StartEngDiffpost2 StartEngDiffpost3 StartEngDiffpost4plus i.unitid i.year ///
 ///
if diff_eng08d == 0 | abs(yearMinusDiffyear) <= 4 , vce(cluster unitid) 

* make plot 
coefplot, vertical keep(StartEngDiffpre4 StartEngDiffpre3 StartEngDiffpre2 StartEngDiffpost0 ///
StartEngDiffpost1 StartEngDiffpost2 StartEngDiffpost3 StartEngDiffpost4plus) yline(0) ///
 ylabel(-0.04(0.02)0.02) yscale(range(-0.04(0.02)0.02)) ytitle("Coefficient") xtitle("year - year differential enacted")

* save plot 
graph export "$outdir\ps1_figure_eng_share2.png" , replace



esttab using "$outdir\ps1_table2.tex", b se r2 ///
 keep(StartEngDiffpre4 StartEngDiffpre3 StartEngDiffpre2 StartEngDiffpre1 StartEngDiffpost0 ///
 StartEngDiffpost1 StartEngDiffpost2 StartEngDiffpost3 StartEngDiffpost4plus _cons)  /// 
 order(StartEngDiffpre4 StartEngDiffpre3 StartEngDiffpre2 StartEngDiffpre1 StartEngDiffpost0 ///
 StartEngDiffpost1 StartEngDiffpost2 StartEngDiffpost3 StartEngDiffpost4plus _cons) ///
replace	
	












