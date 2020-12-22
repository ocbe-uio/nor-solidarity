use advl.dta, clear

keep if studyday >= 0
keep if studyday <= 14
drop if vlsource == 3
*gen vllog10cpkc = log10(vlcpkcell)


mkspline time_1 7 time_2  = studyday
gen tmp = studyday 




tempfile tmp1
tempfile tmp2
tempfile tmp3

mixed vllog10cpkc_imp i.rantrt##c.(time_1 time_2)  || subjectid: studyday, covariance(unstructured)
margins i.rantrt, over(studyday) saving(`tmp1')

margins i.rantrt, dydx(time_1) saving(`tmp2')
margins r.rantrt, dydx(time_1) saving(`tmp3')

use `tmp1', clear
append using `tmp2' `tmp3' , gen(analysis)


