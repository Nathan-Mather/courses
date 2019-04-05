*** * Do file for assignment 3 of pp 713

clear all
set more off, perm

* input directory 
global dir "C:\Users\Nmath_000\Documents\MI_school\Second Year\PP 713\ps3"

* output directory 
global outdir "C:\Users\Nmath_000\Documents\Code\courses\PP 713\ps3_tex\"

* load in data 
use "$dir\njs_data_pp713.dta"



**************
* Question 1 *
**************

* clear stored models 
eststo clear

* regresss earnins on treatment to get ttest 
eststo: reg esum18i treat

* save table 
esttab using "$outdir\ps3_table_1.tex",  nonumbers replace label se

* lear stored models
eststo clear


**************
* Question 2 *
**************
*lear stored models
eststo clear

* create varlist of variabls for q2
local q2vars sex race age totch18 child_miss bfeduca ed_miss bfyrearn earn_miss site_num
foreach y of varlist `q2vars' {

eststo: reg `y' treat

}

di `qu_q2vars'

esttab using "$outdir\ps3_table_2.tex", /// 
mtitles( "sex" "race" "age" "totch18" "child_miss" "bfeduca" "ed_miss" "bfyrearn" "earn_miss") ///
 nonumbers replace label se ///
keep(treat) 

eststo clear

reg treat `q2vars'
display e(F)
 
 
**************
* Question 3 *
**************

* clear stored models 
eststo clear

* regresss earnins on treatment to get ttest 
eststo: reg esum18i treat `q2vars'

* save table 
esttab using "$outdir\ps3_table_3.tex", mtitles("earnings 18 months after") replace label se

* lear stored models
eststo clear


**************
* Question 5 *
**************


* clear stored models 
eststo clear

* regresss earnins on treatment to get ttest 
eststo: probit treat `q2vars'

* save table 
esttab using "$outdir\ps3_table_5.tex",  mtitles("earnings 18 months after") replace label se

* lear stored models
eststo clear

**************
* Question 6 *
**************

* get predicted values 
predict pr_score 




kdensity  pr_score if treat == 1, lc(red) lw(thick) plot(kdensity pr_score if treat == 0, lc(blue) lp(dash) lw(thick)) legend(order(2 "Treated" 1 "Un-treated"))


graph export "$outdir\6_kdens.png" , replace

* check for common support 
summ pr_score if treat == 1
summ pr_score if treat == 0




**************
* question 7 *
**************

* clear stored models 
eststo clear

 * do it with propensity score matcing and nearest neighbor sample 
eststo:  teffects psmatch (esum18i) (treat sex race age totch18 child_miss bfeduca ed_miss bfyrearn site_num earn_miss, probit), atet
 
  teffects psmatch (esum18i) (treat sex race age totch18 child_miss bfeduca ed_miss bfyrearn site_num, probit), atet
 
* do it with actual nearest neighbor matching 
capture noisily teffects nnmatch (esum18i sex race age totch18 child_miss bfeduca ed_miss bfyrearn earn_miss site_num) ///
 (treat),  biasadj(bfyrearn age bfeduca) ematch(totch18 sex race child_miss  ed_miss earn_miss site_num) atet osample(nomatch_1) 

 capture noisily teffects nnmatch (esum18i sex race age totch18 child_miss bfeduca ed_miss bfyrearn earn_miss site_num) ///
 (treat) if nomatch_1 == 0,  biasadj(bfyrearn age bfeduca totch18) ematch(sex race child_miss  ed_miss earn_miss site_num) atet osample(nomatch_2) 

 capture noisily teffects nnmatch (esum18i sex race age totch18 child_miss bfeduca ed_miss bfyrearn earn_miss site_num) ///
 (treat) if nomatch_2 == 0 &  nomatch_1 == 0,  biasadj(bfyrearn age bfeduca totch18) ematch(sex race child_miss  ed_miss earn_miss site_num) atet osample(nomatch_3) 
 
  eststo: teffects nnmatch (esum18i sex race age totch18 child_miss bfeduca ed_miss bfyrearn earn_miss site_num) ///
 (treat) if nomatch_2 == 0 &  nomatch_1 == 0 & nomatch_3 == 0,  biasadj(bfyrearn age bfeduca totch18) ematch(sex race child_miss  ed_miss earn_miss site_num) atet 
  
  
  
  esttab using "$outdir\ps3_q7_table.tex", /// 
 nonumbers replace label se   mtitles("Propensity score matching" "Nearest Neighbor match")

 
 * clear stored models 
eststo clear





**************
* question 8 *
**************

ssc install sensatt
sensatt esum18i treat sex race age totch18 child_miss bfeduca ed_miss bfyrearn site_num earn_miss, p(sex) reps(100) boot










