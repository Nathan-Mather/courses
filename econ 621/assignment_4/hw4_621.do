

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

 
 
 
 
 
 
 