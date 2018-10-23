set scheme sj
clear all
set mem 100m

sjlog using rivtest1, replace
use http://www.stata.com/data/jwooldridge/eacsap/mroz.dta
ivregress 2sls hours nwifeinc educ age kidslt6 kidsge6 (lwage = exper expersq fatheduc motheduc) if inlf==1 , first vce(robust)
rivtest, ci grid(-1000(10)8000)
sjlog close, replace

sjlog using rivtest2, replace
ivtobit hours educ exper expersq kidslt6 kidsge6 city (nwifeinc = hushrs fatheduc motheduc unem), ll(0) first nolog
rivtest, ci gridmult(14) points(500)
sjlog close, replace
