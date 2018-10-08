

use "C:/Users/Nmath_000/Documents/MI_school/Second Year/621 Labor/Assignments/Assignment_4/cfl_jpe_data.dta", clear




******
* 2b * 
******


* collect y_vars 
local y_vf = "hf"
local y_vh = "hh"
local x_vf  = "lnwf lnwh lnwfwh mu3 sexrat divindex child6 child17 eduf agef  whitef"
local x_vh = "lnwf lnwh lnwfwh mu3 sexrat divindex child6 child17 eduh ageh  whiteh"

local z_v = "agef2 eduf2 edp_f eduagef whitef spanf metro cite ville northe northc west prof juiff cathf sexrat divindex ageh2 eduh2 edp_h eduageh whiteh spanh proh juifh cathh"
		  
		  
local z_vh = "ageh2 eduh2 edp_h eduageh  whiteh spanh metro cite ville northe northc west proh juifh cathh sexrat divindex"

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
 
local m1 = _b[xm: lnwh]
local m3 = _b[xm: lnwfwh]
local m4 = _b[xm: mu3]
local f2 = _b[xf: lnwf]
local f3 = _b[xf: lnwfwh]
local f4 = _b[xf: mu3]
local f5 = _b[xf:sexrat]
local f6 = _b[xf:divindex]
local delta = `f3'*`m4'-`f4'*`m3'

* get coeff for lnw1
local sr1 = `m1'*`f4'/`delta'
di `sr1'

* get coeff for lnw2
local sr2 = `m4'*`f2'/`delta'
di `sr2'

* get coeff for lnw1 X lnw2
local sr3 = `m4'*`f4'/`delta'
di `sr3'

* get coeff for y 
local sr4 = `m4'*`f3'/`delta'
di `sr4'

* get coeff for s1
local sr5 = `m4'*`f5'/`delta'
di `sr5'

 * get coeff for s2
local sr6 = `m4'*`f6'/`delta'
di `sr56'
 
 