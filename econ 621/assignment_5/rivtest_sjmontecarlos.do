* monte carlo simulations for rivtest

cap log close
set scheme sj
clear all

* system code
clear
set mem 100m
log using rivtest_sjmontecarlos.log, replace text
display "$S_DATE $S_TIME"
set seed 342985

* simulation model parameters
global sims=5000
global k = 4     	//number of instruments (must be less than 10)
global j = 1		//number of exogenous controls (must be less than 10)
global a0 = .1		//true value of the structural parameter
global b0 = .5 		//true value of the structural parameter
global nu0 = 1		//constant in x stage	
global pi0 = .1		//strength of instrument (coefficient in x stage)
global rho = .5		//covariance from bivariate std normal
global nulltxt "null($b0 )"
global printrate=500
global dotrate=10

* TABLE 1, PANEL A: homoskedastic 2sls with small-sample adjustment
set seed 342985
foreach Nlcl in 200 {
foreach pi0lcl in 0.1 1 {
foreach rholcl in 0.8 0.5 0.1 {

global N=`Nlcl'
global pi0 = `pi0lcl'
global rho = `rholcl'
global obs=max($N ,$sims )

capture {
	drop _all
	set obs $obs
	gen n=_n in 1/$N
	forval a=1/$k {
		gen z`a'=invnorm(uniform()) if !missing(n)
	}
	forval b=1/$j {
		gen w`b'=invnorm(uniform()) if !missing(n)
	}
	gen simindex=_n in 1/$sims
	gen arr = 0 if !missing(simindex)
	gen lmr = 0 if !missing(simindex)
	gen jr =0 if !missing(simindex)
	gen lmjr =0 if !missing(simindex)
	gen clrr =0 if !missing(simindex)
	gen waldr =0 if !missing(simindex)
	gen ssa_arr = 0 if !missing(simindex)
	gen ssa_lmr = 0 if !missing(simindex)
	gen ssa_jr =0 if !missing(simindex)
	gen ssa_lmjr =0 if !missing(simindex)
	gen ssa_clrr =0 if !missing(simindex)
	gen ssa_waldr =0 if !missing(simindex)
	matrix C = (1, $rho \ $rho, 1)
}
forvalues i=1/$sims {
	qui drawnorm u v, corr(C)
	qui gen x = $nu0 + $pi0 *z1 + v if !missing(n)
	qui gen y = $a0 + $b0 *x + .01*w1 + u if !missing(n)
	qui ivregress 2sls y w? (x = z?) if !missing(n), small
	qui rivtest , $nulltxt
	qui replace ssa_arr = 1 if simindex==`i' & `r(ar_p)'<.05
	qui replace ssa_lmr = 1 if simindex==`i' & `r(lm_p)'<.05
	qui replace ssa_jr = 1 if simindex==`i' & `r(j_p)'<.05
	qui replace ssa_lmjr = `r(lmj_r)' if simindex==`i'
	qui replace ssa_clrr = 1 if simindex==`i' & `r(clr_p)'<.05
	qui replace ssa_waldr = 1 if simindex==`i' & `r(wald_p)'<.05
	qui ivregress 2sls y w? (x = z?) if !missing(n)
	qui rivtest , $nulltxt
	qui replace arr = 1 if simindex==`i' & `r(ar_p)'<.05
	qui replace lmr = 1 if simindex==`i' & `r(lm_p)'<.05
	qui replace jr = 1 if simindex==`i' & `r(j_p)'<.05
	qui replace lmjr = `r(lmj_r)' if simindex==`i'
	qui replace clrr = 1 if simindex==`i' & `r(clr_p)'<.05
	qui replace waldr = 1 if simindex==`i' & `r(wald_p)'<.05
	qui keep arr lmr jr lmjr clrr waldr ssa_* z? w? simindex n
	if mod(`i',$printrate )==0	n: sum clrr arr lmr jr lmjr waldr ssa_* if simindex>0&simindex<=`i', separator(0)
}
di "homosk. 2sls SSA N=$N pi0=$pi0 rho=$rho sims=$sims "
proportion clrr arr lmr jr lmjr waldr ssa_* if !missing(simindex)

}
}
}

* TABLE 1, PANEL B: arbitrary heteroskedastic 2sls with small-sample adjustment
set seed 342985
foreach Nlcl in 200 {
foreach pi0lcl in 0.1 1 {
foreach rholcl in 0.8 0.5 0.1 {

global N=`Nlcl'
global pi0 = `pi0lcl'
global rho = `rholcl'
global obs=max($N ,$sims )

capture {
	drop _all
	set obs $obs
	gen n=_n in 1/$N
	forval a=1/$k {
		gen z`a'=invnorm(uniform()) if !missing(n)
	}
	forval b=1/$j {
		gen w`b'=invnorm(uniform()) if !missing(n)
	}
	gen simindex=_n in 1/$sims
	gen arr = 0 if !missing(simindex)
	gen lmr = 0 if !missing(simindex)
	gen jr =0 if !missing(simindex)
	gen lmjr =0 if !missing(simindex)
	gen clrr =0 if !missing(simindex)
	gen waldr =0 if !missing(simindex)
	gen ssa_arr = 0 if !missing(simindex)
	gen ssa_lmr = 0 if !missing(simindex)
	gen ssa_jr =0 if !missing(simindex)
	gen ssa_lmjr =0 if !missing(simindex)
	gen ssa_clrr =0 if !missing(simindex)
	gen ssa_waldr =0 if !missing(simindex)
	matrix C = (1, $rho \ $rho, 1)
	gen scale1=0.5+uniform() if !missing(n)
	gen scale2=0.5+uniform() if !missing(n)
}
forvalues i=1/$sims {
	qui drawnorm u v, corr(C)
	qui gen x = $nu0 + $pi0 *z1 + scale1*v if !missing(n)
	qui gen y = $a0 + $b0 *x + .01*w1 + scale2*u if !missing(n)
	qui ivregress 2sls y w? (x = z?) if !missing(n), vce(robust) small
	qui rivtest , $nulltxt
	qui replace ssa_arr = 1 if simindex==`i' & `r(ar_p)'<.05
	qui replace ssa_lmr = 1 if simindex==`i' & `r(lm_p)'<.05
	qui replace ssa_jr = 1 if simindex==`i' & `r(j_p)'<.05
	qui replace ssa_lmjr = `r(lmj_r)' if simindex==`i'
	qui replace ssa_clrr = 1 if simindex==`i' & `r(clr_p)'<.05
	qui replace ssa_waldr = 1 if simindex==`i' & `r(wald_p)'<.05
	qui ivregress 2sls y w? (x = z?) if !missing(n), vce(robust)
	qui rivtest , $nulltxt
	qui replace arr = 1 if simindex==`i' & `r(ar_p)'<.05
	qui replace lmr = 1 if simindex==`i' & `r(lm_p)'<.05
	qui replace jr = 1 if simindex==`i' & `r(j_p)'<.05
	qui replace lmjr = `r(lmj_r)' if simindex==`i'
	qui replace clrr = 1 if simindex==`i' & `r(clr_p)'<.05
	qui replace waldr = 1 if simindex==`i' & `r(wald_p)'<.05
	qui keep arr lmr jr lmjr clrr waldr ssa_* z? w? simindex n scale?
	if mod(`i',$printrate )==0	n: sum clrr arr lmr jr lmjr waldr ssa_* if simindex>0&simindex<=`i', separator(0)
}
di "arbitary heteroskedastic 2sls SSA N=$N pi0=$pi0 rho=$rho sims=$sims "
proportion clrr arr lmr jr lmjr waldr ssa_* if !missing(simindex)

}
}
}

* TABLE 2, TOP HALF: 2sls with cluster-based serial correlation and heteroskedasticity with small-sample adjustment
set seed 342985
foreach Nlcl in 400 {
foreach clusterslcl in 100 {
foreach pi0lcl in 0.1 1 {
foreach rholcl in 0.8 0.5 0.1 {

global N=`Nlcl'
global clusters=`clusterslcl'
global pi0 = `pi0lcl'
global rho = `rholcl'
global obs=max($N ,$sims )
global rndscale=200

capture {
	drop _all
	set obs $obs
	gen clusterindex=mod(_n,$clusters )+1 in 1/$N
	sort clusterindex
	gen n=_n if !missing(clusterindex)
	forval a=1/$k {
		gen z`a'=invnorm(uniform()) if !missing(clusterindex)
	}
	forval b=1/$j {
		gen w`b'=invnorm(uniform()) if !missing(clusterindex)
	}
	gen simindex=_n in 1/$sims
	gen arr = 0 if !missing(simindex)
	gen lmr = 0 if !missing(simindex)
	gen jr =0 if !missing(simindex)
	gen lmjr =0 if !missing(simindex)
	gen clrr =0 if !missing(simindex)
	gen waldr =0 if !missing(simindex)
	gen ssa_arr = 0 if !missing(simindex)
	gen ssa_lmr = 0 if !missing(simindex)
	gen ssa_jr =0 if !missing(simindex)
	gen ssa_lmjr =0 if !missing(simindex)
	gen ssa_clrr =0 if !missing(simindex)
	gen ssa_waldr =0 if !missing(simindex)
}
forvalues i=1/$sims {
		qui sort clusterindex
		qui gen u=.
		qui gen v=.
		forvalues d=1/$clusters {
			qui sum n if clusterindex==`d', meanonly
			qui local csize = `r(N)'
			qui local gpstart = `r(min)'
			qui mata tempmat = tempmat2 = tempmat3 = .
			qui mata tempmat = invnormal(uniform($rndscale *`csize',`csize'))
			qui mata tempmat2 = invnormal(uniform($rndscale *`csize',`csize'))
			qui mata tempmat = (tempmat,tempmat2*$rho )
			qui mata tempmat3 = tempmat'*tempmat
			qui mata tempmat3[(`csize'+1..2*`csize'),(`csize'+1..2*`csize')]=tempmat3[(`csize'+1..2*`csize'),(`csize'+1..2*`csize')]/(($rho )^2)
			qui mata tempmat = cholesky(tempmat3/($rndscale *`csize'))*invnormal(uniform(2*`csize',1))
			qui mata st_matrix("ujvj",tempmat)
			qui matrix uj = ujvj[1..`csize',1]
			qui matrix vj = ujvj[`csize'+1...,1]
			qui svmat double uj
			qui svmat double vj
			qui replace u=uj1[_n-`gpstart'+1] if clusterindex==`d'
			qui replace v=vj1[_n-`gpstart'+1] if clusterindex==`d'
			qui drop uj1 vj1
			qui matrix drop uj vj ujvj
		}	
		qui gen xclst = $nu0 + $pi0 *z1 + v if !missing(clusterindex)
		qui gen yclst = $a0 + $b0 *xclst + .01*w1 + u if !missing(clusterindex)
		qui ivregress 2sls yclst w? (xclst = z?) if !missing(clusterindex), vce(cluster clusterindex) small
		qui rivtest , $nulltxt
		qui replace ssa_arr = 1 if simindex==`i' & `r(ar_p)'<.05
		qui replace ssa_lmr = 1 if simindex==`i' & `r(lm_p)'<.05
		qui replace ssa_jr = 1 if simindex==`i' & `r(j_p)'<.05
		qui replace ssa_lmjr = `r(lmj_r)' if simindex==`i'
		qui replace ssa_clrr = 1 if simindex==`i' & `r(clr_p)'<.05
		qui replace ssa_waldr = 1 if simindex==`i' & `r(wald_p)'<.05
		qui ivregress 2sls yclst w? (xclst = z?) if !missing(clusterindex), vce(cluster clusterindex)
		qui rivtest , $nulltxt
		qui replace arr = 1 if simindex==`i' & `r(ar_p)'<.05
		qui replace lmr = 1 if simindex==`i' & `r(lm_p)'<.05
		qui replace jr = 1 if simindex==`i' & `r(j_p)'<.05
		qui replace lmjr = `r(lmj_r)' if simindex==`i'
		qui replace clrr = 1 if simindex==`i' & `r(clr_p)'<.05
		qui replace waldr = 1 if simindex==`i' & `r(wald_p)'<.05
		qui keep arr lmr jr lmjr clrr waldr ssa_* z? w? clusterindex simindex n
	if mod(`i',$printrate )==0	n: sum clrr arr lmr jr lmjr waldr ssa_* if simindex>0&simindex<=`i', separator(0)
}
di "clustered 2sls SSA N=$N clusters=$clusters pi0=$pi0 rho=$rho sims=$sims "
proportion clrr arr lmr jr lmjr waldr ssa_* if !missing(simindex)

}
}
}
}

* TABLE 2, BOTTOM HALF: 2sls with cluster-based serial correlation and heteroskedasticity with small-sample adjustment
set seed 342985
foreach Nlcl in 500 {
foreach clusterslcl in 50 {
foreach pi0lcl in 0.1 1 {
foreach rholcl in 0.8 0.5 0.1 {

global N=`Nlcl'
global clusters=`clusterslcl'
global pi0 = `pi0lcl'
global rho = `rholcl'
global obs=max($N ,$sims )
global rndscale=200

capture {
	drop _all
	set obs $obs
	gen clusterindex=mod(_n,$clusters )+1 in 1/$N
	sort clusterindex
	gen n=_n if !missing(clusterindex)
	forval a=1/$k {
		gen z`a'=invnorm(uniform()) if !missing(clusterindex)
	}
	forval b=1/$j {
		gen w`b'=invnorm(uniform()) if !missing(clusterindex)
	}
	gen simindex=_n in 1/$sims
	gen arr = 0 if !missing(simindex)
	gen lmr = 0 if !missing(simindex)
	gen jr =0 if !missing(simindex)
	gen lmjr =0 if !missing(simindex)
	gen clrr =0 if !missing(simindex)
	gen waldr =0 if !missing(simindex)
	gen ssa_arr = 0 if !missing(simindex)
	gen ssa_lmr = 0 if !missing(simindex)
	gen ssa_jr =0 if !missing(simindex)
	gen ssa_lmjr =0 if !missing(simindex)
	gen ssa_clrr =0 if !missing(simindex)
	gen ssa_waldr =0 if !missing(simindex)
}
forvalues i=1/$sims {
		qui sort clusterindex
		qui gen u=.
		qui gen v=.
		forvalues d=1/$clusters {
			qui sum n if clusterindex==`d', meanonly
			qui local csize = `r(N)'
			qui local gpstart = `r(min)'
			qui mata tempmat = tempmat2 = tempmat3 = .
			qui mata tempmat = invnormal(uniform($rndscale *`csize',`csize'))
			qui mata tempmat2 = invnormal(uniform($rndscale *`csize',`csize'))
			qui mata tempmat = (tempmat,tempmat2*$rho )
			qui mata tempmat3 = tempmat'*tempmat
			qui mata tempmat3[(`csize'+1..2*`csize'),(`csize'+1..2*`csize')]=tempmat3[(`csize'+1..2*`csize'),(`csize'+1..2*`csize')]/(($rho )^2)
			qui mata tempmat = cholesky(tempmat3/($rndscale *`csize'))*invnormal(uniform(2*`csize',1))
			qui mata st_matrix("ujvj",tempmat)
			qui matrix uj = ujvj[1..`csize',1]
			qui matrix vj = ujvj[`csize'+1...,1]
			qui svmat double uj
			qui svmat double vj
			qui replace u=uj1[_n-`gpstart'+1] if clusterindex==`d'
			qui replace v=vj1[_n-`gpstart'+1] if clusterindex==`d'
			qui drop uj1 vj1
			qui matrix drop uj vj ujvj
		}	
		qui gen xclst = $nu0 + $pi0 *z1 + v if !missing(clusterindex)
		qui gen yclst = $a0 + $b0 *xclst + .01*w1 + u if !missing(clusterindex)
		qui ivregress 2sls yclst w? (xclst = z?) if !missing(clusterindex), vce(cluster clusterindex) small
		qui rivtest , $nulltxt
		qui replace ssa_arr = 1 if simindex==`i' & `r(ar_p)'<.05
		qui replace ssa_lmr = 1 if simindex==`i' & `r(lm_p)'<.05
		qui replace ssa_jr = 1 if simindex==`i' & `r(j_p)'<.05
		qui replace ssa_lmjr = `r(lmj_r)' if simindex==`i'
		qui replace ssa_clrr = 1 if simindex==`i' & `r(clr_p)'<.05
		qui replace ssa_waldr = 1 if simindex==`i' & `r(wald_p)'<.05
		qui ivregress 2sls yclst w? (xclst = z?) if !missing(clusterindex), vce(cluster clusterindex)
		qui rivtest , $nulltxt
		qui replace arr = 1 if simindex==`i' & `r(ar_p)'<.05
		qui replace lmr = 1 if simindex==`i' & `r(lm_p)'<.05
		qui replace jr = 1 if simindex==`i' & `r(j_p)'<.05
		qui replace lmjr = `r(lmj_r)' if simindex==`i'
		qui replace clrr = 1 if simindex==`i' & `r(clr_p)'<.05
		qui replace waldr = 1 if simindex==`i' & `r(wald_p)'<.05
		qui keep arr lmr jr lmjr clrr waldr ssa_* z? w? clusterindex simindex n
	if mod(`i',$printrate )==0	n: sum clrr arr lmr jr lmjr waldr ssa_* if simindex>0&simindex<=`i', separator(0)
}
di "clustered 2sls SSA N=$N clusters=$clusters pi0=$pi0 rho=$rho sims=$sims "
proportion clrr arr lmr jr lmjr waldr ssa_* if !missing(simindex)

}
}
}
}

* TABLE 3, PANEL A: ivprobit with b0=0
set seed 342985
global b0 = 0 		//true value of the structural parameter
global nulltxt "null($b0 )"

foreach Nlcl in 200 {
foreach pi0lcl in 0.1 1 {
foreach rholcl in 0.8 0.5 0.1 {

global N=`Nlcl'
global pi0 = `pi0lcl'
global rho = `rholcl'
global obs=max($N ,$sims )

capture {
	drop _all
	set obs $obs
	gen n=_n in 1/$N
	forval a=1/$k {
		gen z`a'=invnorm(uniform()) if !missing(n)
	}
	forval b=1/$j {
		gen w`b'=invnorm(uniform()) if !missing(n)
	}
	gen simindex=_n in 1/$sims
	gen arr = 0 if !missing(simindex)
	gen lmr = 0 if !missing(simindex)
	gen jr =0 if !missing(simindex)
	gen lmjr =0 if !missing(simindex)
	gen clrr =0 if !missing(simindex)
	gen waldr =0 if !missing(simindex)
	matrix C = (1, $rho \ $rho, 1)
}
forvalues i=1/$sims {
	qui drawnorm u v, corr(C)
	qui gen x = $nu0 + $pi0 *z1 + v if !missing(n)
	qui gen y = $a0 + $b0 *x + .01*w1 + u if !missing(n)
	qui replace y = y>=0 if !missing(n)
	qui ivprobit y w? (x = z?) if !missing(n), iterate(200)
	if `e(converged)'==0 {
		qui ivprobit y w? (x = z?) if !missing(n), iterate(200) difficult
	}
	if `e(converged)'==0 {
		qui ivprobit y w? (x = z?) if !missing(n), iterate(200) difficult technique(bfgs)
	}
	if `e(converged)'==1 {
		qui rivtest , $nulltxt
		qui replace arr = 1 if simindex==`i' & `r(ar_p)'<.05
		qui replace lmr = 1 if simindex==`i' & `r(lm_p)'<.05
		qui replace jr = 1 if simindex==`i' & `r(j_p)'<.05
		qui replace lmjr = `r(lmj_r)' if simindex==`i'
		qui replace clrr = 1 if simindex==`i' & `r(clr_p)'<.05
		qui replace waldr = 1 if simindex==`i' & `r(wald_p)'<.05
	}
	else {
		qui replace arr = . if simindex==`i'
		qui replace lmr = . if simindex==`i'
		qui replace jr = . if simindex==`i'
		qui replace lmjr = . if simindex==`i'
		qui replace clrr = . if simindex==`i'
		qui replace waldr = . if simindex==`i'
	}
	qui keep arr lmr jr lmjr clrr waldr z? w? simindex n
	if mod(`i',$dotrate )==0	n: di "." _continue
	if mod(`i',$printrate )==0	n: sum clrr arr lmr jr lmjr waldr if simindex>0&simindex<=`i', separator(0)
}
di "ivprobit N=$N b0=$b0 pi0=$pi0 rho=$rho sims=$sims "
proportion clrr arr lmr jr lmjr waldr if !missing(simindex)

}
}
}
global b0 = 0.5 		//true value of the structural parameter
global nulltxt "null($b0 )"


* TABLE 3, PANEL B: ivtobit
set seed 342985
foreach Nlcl in 200 {
foreach pi0lcl in 0.1 1 {
foreach rholcl in 0.8 0.5 0.1 {

global N=`Nlcl'
global pi0 = `pi0lcl'
global rho = `rholcl'
global obs=max($N ,$sims )

capture {
	drop _all
	set obs $obs
	gen n=_n in 1/$N
	forval a=1/$k {
		gen z`a'=invnorm(uniform()) if !missing(n)
	}
	forval b=1/$j {
		gen w`b'=invnorm(uniform()) if !missing(n)
	}
	gen simindex=_n in 1/$sims
	gen arr = 0 if !missing(simindex)
	gen lmr = 0 if !missing(simindex)
	gen jr =0 if !missing(simindex)
	gen lmjr =0 if !missing(simindex)
	gen clrr =0 if !missing(simindex)
	gen waldr =0 if !missing(simindex)
	matrix C = (1, $rho \ $rho, 1)
}
forvalues i=1/$sims {
	qui drawnorm u v, corr(C)
	qui gen x = $nu0 + $pi0 *z1 + v if !missing(n)
	qui gen y = $a0 + $b0 *x + .01*w1 + u if !missing(n)
	qui replace y = 0 if y<=0 & !missing(n)
	qui ivtobit y w? (x = z?) if !missing(n), ll iterate(200)
	if `e(converged)'==0 {
		qui ivtobit y w? (x = z?) if !missing(n), ll iterate(200) difficult
	}
	if `e(converged)'==0 {
		qui ivtobit y w? (x = z?) if !missing(n), ll iterate(200) difficult technique(bfgs)
	}
	if `e(converged)'==1 {
		qui rivtest , $nulltxt
		qui replace arr = 1 if simindex==`i' & `r(ar_p)'<.05
		qui replace lmr = 1 if simindex==`i' & `r(lm_p)'<.05
		qui replace jr = 1 if simindex==`i' & `r(j_p)'<.05
		qui replace lmjr = `r(lmj_r)' if simindex==`i'
		qui replace clrr = 1 if simindex==`i' & `r(clr_p)'<.05
		qui replace waldr = 1 if simindex==`i' & `r(wald_p)'<.05
	}
	else {
		qui replace arr = . if simindex==`i'
		qui replace lmr = . if simindex==`i'
		qui replace jr = . if simindex==`i'
		qui replace lmjr = . if simindex==`i'
		qui replace clrr = . if simindex==`i'
		qui replace waldr = . if simindex==`i'
	}
	qui keep arr lmr jr lmjr clrr waldr z? w? simindex n
	if mod(`i',$dotrate )==0	n: di "." _continue
	if mod(`i',$printrate )==0	n: sum clrr arr lmr jr lmjr waldr if simindex>0&simindex<=`i', separator(0)
}
di "ivtobit N=$N b0=$b0 pi0=$pi0 rho=$rho sims=$sims "
proportion clrr arr lmr jr lmjr waldr if !missing(simindex)

}
}
}

* clean up
display "$S_DATE $S_TIME"
log close
exit








