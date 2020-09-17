
/*********************
Import all Viedoc export files and make them into Stata files

Move the Viedoc csv-files to the Datasets/raw/txt folder and make the Datasets the working directory.

The program relies on the labmask.ado userwritten program, which can be installed by writing
ssc install labutil
**********************/







capture program drop impraw
program define impraw
	version 14.1
	args inname

	/************
	Get the varnames and varlabels
	*************/

	local temp : dir "./raw/txt" files "*_`inname'.csv"
	local filename = `temp'
	local date=substr("`filename'",2,4) + "-" + substr("`filename'",6,2) + "-" + substr("`filename'",8,2)
	capture mkdir raw/txt/`date'
	copy raw/txt/`filename' raw/txt/`date'/`filename', replace


	import delimited  raw/txt/`filename' , varnames(nonames) clear rowrange(1:2) encoding(UTF-8)




	gen tmp=_n
	reshape long v, i(tmp) j(tmp2) string
	destring tmp2, gen(tmp3)
	sort tmp3 tmp
	drop tmp2
	tempfile temp1    /* create a temporary file */
	save "`temp1'"

	rename v name0
	keep if tmp==2
	replace tmp=1
	merge 1:1 tmp3 tmp using "`temp1'", keep(3) nogen
	rename v label
	gen name=strlower(name0)
	drop tmp* name0



	/**************
	Create the list of variables with cd suffix (categorical variables)
	**************/
	local vvars
	local vvarsv


	forvalues i =  1/`= _N' {
		forvalues j = `i'/`= _N'  {
			if name[`i']+"cd" == name[`j'] {
				local tmp=name[`i']
				local tmp2=name[`j']
				local vvars `vvars' `tmp'
				local vvarsv `vvarsv' `tmp2'
			}
		}

	}

	gen slug="label var "+name + `" ""' + label + `"""'



	outfile slug using labels.do, noq replace

	import delimited raw/txt/`filename' , varnames(2) rowrange(2:)  clear encoding(UTF-8)


	do labels.do


	erase labels.do




	display "`vvars'"
	/*********************
	Get the variable order of variables except cd variables
	**********************/

	if "`vvars'" != "" {
		ds `vvarsv' , not
		local order `r(varlist)'


		/***********************
		Value label all categorical variables
		************************/


		foreach x of varlist `vvars' {
				quietly sum `x'cd
				if `r(N)' != 0 {
					labmask `x'cd, values(`x')
				}
				local ln : variable label `x'
				drop `x'
				rename `x'cd `x'
				label var `x' "`ln'"
			}
		}
		else {
			ds *
			local order `r(varlist)'
		}


		/*******************
		Make the dataset back to original order
		********************/
		order `order'

		/***********************
		Value label site and pid
		************************/

		labmask sitecode, values(sitename)
		rename sitecode site
		label var site "Site"
		drop  sitename siteseq subjectseq

		order site subjectid

		char _dta[date] `date'
		save raw/`inname', replace


end

/**************
Run the impraw program for each dataset
******************/

set more off
impraw AE
impraw MedDRA
impraw DM
impraw RAN1
