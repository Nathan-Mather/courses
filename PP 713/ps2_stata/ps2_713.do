
*** * Do file for assignment 2 of pp 713

clear all
set more off, perm

* input directory 
global dir "C:\Users\Nmath_000\Documents\MI_school\Second Year\PP 713\ps2"

* output directory 
global outdir "C:\Users\Nmath_000\Documents\Code\courses\PP 713\ps2_tex\"

* load in data 
use "$dir\ps2_dataset.dta"


****************************
*1. Created needed variables *
****************************

* a. indicator for scoreing above 475 in year one 
gen pass_1 = (psut1 >= 475 & psut1 != .) 
replace pass_1 =. if psut1 == .
tab(pass_1)

  label define pass_1L 0 "Below 475" 1 "Above 475"
  label values pass_1 pass_1L


* b. gnerate pre selected variable based on income quintile 
gen pre_sel = (qqt1 <= 4 & qqt1 != .) 
tab qqt1 pre_sel

  label define pre_selL 0 "Not Pre-Selected" 1 "Pre-Selected"
  label values pre_sel pre_selL



* c. running score centered at 475 
gen r_score_1 = psut1 - 475

* make interaction variable 
gen pass_score_1 = pass_1 * r_score_1

* make some labels
label variable r_score_1    "PSU Score - 475"
label variable pass_1       "PSU Score Above 475"
label variable pass_score_1 "PSU Score if Above 475"

*****************************
* 2. descriptive stats 
*******************************

* check if anyone doesn't have a value for PSU in period one 
count if psut1 == .
* none, no need to worry about that 

******
* a. *
******

* pre selected individuals in period 1 
* and proportion of psu takers that are preselected 
tab pre_sel

* save it for latex 
tabout pre_sel using "$outdir\tab2a.tex", ///
replace ///
style(tex) font(bold)

* proportiion above and below cutoff that are preselectd 
tab pass_1 pre_sel, r nof

* save it for latex 
tabout pass_1 pre_sel using "$outdir\tab2aii.tex", ///
replace ///
style(tex) font(bold)  cells(row)


****
*B.*
****
* the forcing variable 
summarize r_score_1
hist r_score_1, freq width(20) start(-320)

* save plot 
graph export "$outdir\2b_hist.png" , replace


****
*C.*
****
* Rates of immediate ennrollement and ever enrollment by group/ 
	
* make labes 
  label define yesno 0 "No" 1 "Yes"
  label values enrolt1 yesno
 label values everenroll1 yesno
  * make by group 
gen Group = pre_sel
replace Group = 2 if pre_sel == 1 & pass_1 == 1

  label define GroupL 0 "Not Pre-Sel" 1 "Pre-Sel Below" 2 "Pre-Sel Above"
  label values Group GroupL

 * check tables 
tab Group enrolt1 , r nof
tab Group everenroll1 , r nof

* save them for latex 
tabout Group enrolt1 using "$outdir\tab2ci.tex", replace ///
style(tex) font(bold)  cells(row)

tabout Group everenroll1 using "$outdir\tab2cii.tex", replace ///
style(tex) font(bold)  cells(row)


****
*D.*
****


 * check tables 
tab qqt1 enrolt1 , r nof
tab qqt1 everenroll1 , r nof

* save them for latex 
tabout qqt1 enrolt1 using "$outdir\tab2di.tex", replace ///
style(tex) font(bold)  cells(row)

tabout qqt1 everenroll1 using "$outdir\tab2dii.tex", replace ///
style(tex) font(bold)  cells(row)




*****************************
* 3. Checking Assumptions
*******************************


* make plot of distributions by income quantile 
twoway                                ///
   (kdensity r_score_1 if qqt1 == 1 ) ///
   (kdensity r_score_1 if qqt1 == 2 ) ///
   (kdensity r_score_1 if qqt1 == 3 ) ///
   (kdensity r_score_1 if qqt1 == 4 ) ///
   (kdensity r_score_1 if qqt1 == 5 ) ///
   ,                                  ///
   legend(order(1 "Income Quant 1" 2 "Income Quant 2" 3 "Income Quant 3" 4 "Income Quant 4" 5 "Income Quant 5")) ///
   ytit("Density") xline(0)


 * save plot 
 graph export "$outdir\3_plot.png" , replace

 
**************************
* 4 replicate reg tables *
**************************


eststo clear

* do the regressions the way they did them 
eststo: reg enrolt1  pass_1 r_score_1 pass_score_1 if qqt1<=4 & abs(r_score_1)<=44, r
estadd scalar Bandwidth = 44 

eststo: reg enrolt1  pass_1 r_score_1 pass_score_1 if pre_sel==0 & abs(r_score_1)<=44, r

estadd scalar Bandwidth = 44 



esttab using "$outdir\ps2_table_4.tex", /// 
mtitles("(1)" "(2)") nonumbers replace label stats(Bandwidth) se

eststo clear



* this is an extension of the Imbens and Kalyanaraman approach. It give similar results 
* but is more robust and bias corrected 
eststo: rdrobust enrolt1 r_score_1 if qqt1 <= 4
display e(h_l) 
display e(h_r)

estadd scalar Bandwidth = e(h_l)

eststo: rdrobust enrolt1 r_score_1 if qqt1 > 4
display e(h_l) 
display e(h_r)
estadd scalar Bandwidth = e(h_l)



esttab using "$outdir\ps2_table_4ii.tex", /// 
mtitles("(1b)" "(2b)") nonumbers replace label stats(Bandwidth) se

****************
*5  Replicate IV *
****************

* dod table 4 regeressions 
ivreg everenroll1  (everelig1=pass_1) r_score_1 pass_score_1 if qqt1<=4 & abs(r_score_1)<=44, r first
reg everenroll1  pass_1 r_score_1 pass_score_1 if pre_sel==0 & abs(r_score_1)<=44, r



****************
* 6 make fig 1 *
****************



rdplot enrolt1 psut1 if qqt1<=4 , c(475) shade ci(95) binselect(espr)
rdplot enrolt1 psut1 if pre_sel==0, c(475) shade ci(95) binselect(espr)



*********************
* q 7 placebo tests *
*********************


* set up a matrix for the results 
matrix Res = J(89,3,.)


forvalues i = 431/519{

* create variables for regression 
gen pass_i = (psut1 >= `i' & psut1 != .) 
gen r_score_i = psut1 - `i'
gen pass_score_i = pass_i * r_score_i

* get matrix position 
local mat_post = `i' - 430

* store cutoff in matrix 
matrix Res[`mat_post',1] =  `i'

* run regression a with these vars 
reg enrolt1  pass_i r_score_i pass_score_i if qqt1<=4 & abs(r_score_i)<=44, r

* store result in matrix 
matrix Res[`mat_post',2] = _b[pass_i]

* run regression b with these vars 
reg enrolt1  pass_i r_score_i pass_score_i if pre_sel==0 & abs(r_score_i)<=44, r

* store result in other matrix column
matrix Res[`mat_post',3] = _b[pass_i]

* drop variables for next iteration 
drop pass_i
drop r_score_i
drop pass_score_i

}

* make the results the data set 
drop _all
svmat float Res

* make a histogram of each 
hist Res2, bin(15) kdens  addplot(pci 0 .175 20 .175, lcolor(black) lwidth(1)) ///
   legend(order(1 "Density" 2 "Kernal Density" 3 "Coefficient at 475"))

hist Res3, bin(15) kdens  addplot(pci 0 .003 100 .003, lcolor(black) lwidth(1))  ///
   legend(order(1 "Density" 2 "Kernal Density" 3 "Coefficient at 475"))



