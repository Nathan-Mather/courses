

use "C:/Users/Nmath_000/Documents/MI_school/Second Year/621 Labor/Assignments/Assignment_4/cfl_jpe_data.dta", clear




******
* 2b * 
******


* collect y_vars 
local y_vf = "hf"
local y_vh = "hh"
local x_vf  = "lnwf lnwh lnwfwh mu3 sexrat divindex child6 child17 eduf agef  whitef northe northc west"
local x_vh = "lnwf lnwh lnwfwh mu3 sexrat divindex child6 child17 eduh ageh  whiteh northe northc west"

local z_v = "eduf eduf2 eduagef agef agef2 edp_f whitef spanf prof juiff cathf eduh eduh2 eduageh ageh ageh2 edp_h whiteh spanh proh juifh cathh metro cite ville northe northc west sexrat divindex"
             		  

gmm (hf - {xf: `x_vf'} - {f0}) (hh - {xm: `x_vh'} - {h0}), instruments(`z_v') winitial(unadjusted, independent)

* local coef_rat_1=_b[xf:sexrat]/ _b[xf:divindex]
*local coef_rat_2=_b[xm:sexrat]/ _b[xm:divindex]

 
 ******
 * 2c *
 ******
testnl  _b[xm:sexrat] /_b[xf:sexrat] = _b[xm:divindex] / _b[xf:divindex] 

 
 ******
 * 2d *
 ******

testnl  _b[xm:mu3] /_b[xf:mu3] = _b[xm:sexrat] /_b[xf:sexrat] = _b[xm:divindex] / _b[xf:divindex] 


 ******
 * 2e *
 ******
 
local m1 = _b[xm: lnwf]
local m3 = _b[xm: lnwfwh]
local m4 = _b[xm: mu3]
local f2 = _b[xf: lnwh]
local f3 = _b[xf: lnwfwh]
local f4 = _b[xf: mu3]
local f5 = _b[xf:sexrat]
local f6 = _b[xf:divindex]
local delta = `f3'*`m4'-`f4'*`m3'


* get coeff for lnw1
local sr1 = `m1'*`f4'/`delta'
di "coef for lnw1:    " `sr1'

local cwgf = _b[xm:lnwf]*_b[xf:lnwfwh]/`delta'
di `cwgf'

* get coeff for lnw2
local sr2 = `m4'*`f2'/`delta'
di "coef for lnw2:    " `sr2'

* get coeff for lnw1 X lnw2
local sr3 = `m4'*`f4'/`delta'
di "coef for lnw1Xlnw2:    "`sr3'

* get coeff for y 
local sr4 = `m4'*`f3'/`delta'
di "coef nonlabor income:    " `sr4'

* get coeff for s1
local sr5 = `m4'*`f5'/`delta'
di "coef fcr s1:    " `sr5'

 * get coeff for s2
local sr6 = `m4'*`f6'/`delta'
di "coef fcr s2:    "`sr6'
 
 