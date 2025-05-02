clear all

use "study2data.dta"

*post-only
reg aiddv treat PID ideo if pretreat==0
est store m1

*prepost
reg aiddv treat aid1 PID ideo if pretreat==1
est store m2

*difference in coefficients
suest m1 m2
test [m1_mean]treat=[m2_mean]treat


*figure 1
gen x=_n
replace x=. if _n>2
gen est=.
gen se=.

reg aiddv treat PID ideo if pretreat==0
replace se=_se[treat] in 1
replace est=_b[treat] in 1

reg aiddv treat aid1 PID ideo if pretreat==1
replace se=_se[treat] in 2
replace est=_b[treat] in 2

gen lo=est-1.96*se
gen hi=est+1.96*se



twoway (scatter est x if x==1, mcol(black)) (scatter est x if x==2, mcol(gray)) ///
	(rcap lo hi x if x==1, lcol(black)) ///
	(rcap lo hi x if x==2, lcol(gray) graphregion(color(white)) legend(off) ylab(.2(.2)-.6, labsize(medsmall)) ///
	xsc(range(.5 2.5)) xlab(1 "Post-only" 2 "Pre-post", labsize(medsmall)) yline(0) xtitle("") ///
	subtitle("Study 2: Foreign Aid") ///
	plotregion(margin(zero)) graphregion(margin(1 1 1 1)) saving(study2.gph, replace))
	
