set more off
use RStataDataIn75289
capture noisily {
/*RSTATA: cut me here*/
tempfile tmp

quietly meglm lbneures i.rantrt i.studyday i.rantrt#i.studyday || subjectid: , family(gaussian) link(log)
quietly margins studyday#rantrt,  saving(`tmp')
use `tmp', clear
/*RSTATA: cut me here*/
} /* end capture noisily */
saveold RStataDataOut75289, version(12)
exit, clear STATA
