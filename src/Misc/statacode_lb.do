
use adlb, clear

keep if studyday <= 14
keep if fas_hcq == 2

mkspline time_1 7 time_2 = studyday
gen day14 = studyday == 14

meglm lbcrpres i.rantrt##i.studyday  || subjectid:, family(gamma) 
margins r.rantrt, subpop(day14)
margins i.rantrt, over(studyday)
marginsplot, xdim(studyday)

mixed lbcrpres lbcrpres_bl i.rantrt##c.(time_1 time_2) if studyday != 0 || subjectid: , family(gamma)
margins i.rantrt, over(studyday)
marginsplot, xdim(studyday) at(lbcrpres)




tempfile tmp

use adlb, clear

keep if studyday <= 14
keep if fas_hcq == 2

mkspline time_1 7 time_2 = studyday
gen day14 = studyday == 14


meglm lbcrpres i.rantrt##i.studyday  || subjectid:, family(gamma)  
margins i.rantrt, over(studyday) 
marginsplot, xdim(studyday)

mixed lbcrpres i.rantrt##i.studyday  || subjectid:  
margins i.rantrt, over(studyday)  saving(`tmp')
use `tmp', clear

mixed lbcrpres i.rantrt##i.studyday  || subjectid:  
margins studyday#rantrt  




tempfile tmp1
tempfile tmp2
tempfile tmp3
tempfile tmp4

use adlb, clear

keep if studyday <= 14
keep if fas_hcq == 2

mkspline time_1 7 time_2 = studyday
gen day14 = studyday == 14

meglm lbcrpres  i.rantrt##c.(time_1 time_2)  || subjectid:, family(gamma)
margins i.rantrt, dydx(time_1) saving(`tmp1')
margins r.rantrt, dydx(time_1) saving(`tmp2')
margins i.rantrt, subpop(day14) saving(`tmp3')
margins r.rantrt, subpop(day14) saving(`tmp4')

*margins i.rantrt, over(studyday) at(lbcrpres_bl = 100)
*marginsplot, xdim(studyday)

use `tmp1', clear
append using `tmp2' `tmp3' `tmp4', gen(type_c)

gen type_n = type_c
label def type_c1 0 "Slope 1st week by treatment" 1 "Difference in 1st week slope" 2 "Day 14 level by treatment" 3 "Day 14 treatment difference"
label val type_c type_c1

use adlb, clear

tempfile tmp1
tempfile tmp2
tempfile tmp3
tempfile tmp4


mkspline time_1 7 time_2 = studyday
gen day14 = studyday == 14

quietly model} lbcrpres  i.rantrt##c.(time_1 time_2)  || subjectid: 
margins i.rantrt, dydx(time_1) saving(`tmp1')
margins r.rantrt, dydx(time_1) saving(`tmp2')
margins i.rantrt, subpop(day14) saving(`tmp3')
margins r.rantrt, subpop(day14) saving(`tmp4')

use `tmp1', clear
append using `tmp2' `tmp3' `tmp4', gen(type_c)

gen type_n = type_c
label def type_c 0 "Slope 1st week by treatment" 1 "Difference in 1st week slope" 2 "Day 14 level by treatment" 3 "Day 14 treatment difference"
label val type_c type_c


