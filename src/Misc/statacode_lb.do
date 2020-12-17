
use adlb, clear

keep if studyday <= 10
keep if fas_hcq == 2

mkspline time_1 7 time_2 = studyday
gen day10 = studyday == 10


meglm lbcrpres i.rantrt##c.(time_1 time_2)  || subjectid: , family(gamma)
margins r.rantrt, dydx(time_1)


meglm lbcrpres i.rantrt##i.studyday  || subjectid:, family(gamma) 
margins r.rantrt, subpop(day10)
margins i.rantrt, over(studyday)
marginsplot, xdim(studyday)

