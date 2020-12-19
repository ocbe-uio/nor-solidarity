
VIEDOC_EXPORT_NAME = ous_20201121_154631
DATE = $(shell echo $(VIEDOC_EXPORT_NAME) | sed 's/.*_\(....\)\(..\)\(..\)_.*/\1-\2-\3/')
DMC_REPORT = $(DATE)_Nor-Solidarity_DMC_Report.docx
MAIN_REPORT1 = $(DATE)_Nor-Solidarity_Main_Report1.docx
RAW_CSV = $(wildcard data/raw/$(VIEDOC_EXPORT_NAME)/*)
TDMISC = data/td/tdae.rds data/td/tdex.rds data/td/tdsq.rds data/td/tdsc.rds data/td/tdcm.rds data/td/tdds.rds
TD = data/td/tdran.rds $(TDMISC) data/td/tddm.rds data/td/tdrc.rds data/td/tdvs.rds data/td/tdlb.rds
AD = data/ad/adsl.rds data/ad/adae.rds data/ad/adeff.rds data/ad/adex.rds data/ad/addm.rds data/ad/adev.rds
RD = results/rds/mortres.rds results/rds/efflbres.rds

# Set this to FALSE when for the true results.  
PSEUDORANDOM = TRUE

.PHONY: all td ad rd raw dmc_report
all: td ad raw rd
td: $(TD)
ad: $(AD)
rd: $(RD)
raw: data/raw/raw.rds
dmc_report: results/dmc/$(DMC_REPORT)
main_report1: results/main/$(MAIN_REPORT1)

data/raw/raw.rds:  $(RAW_CSV) src/make_raw/make_raw.R src/external/functions.R
	Rscript src/make_raw/make_raw.R $(VIEDOC_EXPORT_NAME)

data/td/tdran.rds: data/raw/raw.rds src/external/functions.R src/make_td/make_tdran.R
	Rscript src/make_td/make_tdran.R

$(TDMISC) &: data/raw/raw.rds data/td/tdran.rds src/external/functions.R src/make_td/make_tdmisc.R
	Rscript src/make_td/make_tdmisc.R

data/td/tddm.rds: data/raw/raw.rds data/td/tdds.rds src/external/functions.R src/make_td/make_tddm.R
	Rscript src/make_td/make_tddm.R
	
data/td/tdlb.rds: data/raw/raw.rds data/td/tddm.rds src/external/functions.R src/make_td/make_tdlb.R
	Rscript src/make_td/make_tdlb.R

data/td/tdrc.rds data/td/tdvs.rds: data/raw/raw.rds src/external/functions.R src/make_td/make_tdrcvs.R
	Rscript src/make_td/make_tdrcvs.R
	
data/ad/adsl.rds: data/raw/raw.rds src/external/functions.R data/td/tddm.rds data/td/tdran.rds data/td/tdsq.rds src/make_ad/make_adsl.R
	Rscript src/make_ad/make_adsl.R $(PSEUDORANDOM)
	
data/ad/addm.rds: $(TD) data/ad/adsl.rds src/external/functions.R src/make_ad/make_addm.R
	Rscript src/make_ad/make_addm.R
	
data/ad/adex.rds: data/td/tdex.rds data/ad/adsl.rds src/make_ad/make_adex.R
	Rscript src/make_ad/make_adex.R
  
data/ad/adae.rds: data/td/tdae.rds data/ad/adsl.rds src/make_ad/make_adae.R
	Rscript src/make_ad/make_adae.R
  
data/ad/adeff.rds: data/td/tdds.rds data/ad/adsl.rds src/make_ad/make_adeff.R
	Rscript src/make_ad/make_adeff.R

data/ad/adev.rds: data/td/tdds.rds  data/td/tdrc.rds data/ad/adsl.rds data/td/tdsq.rds src/make_ad/make_adev.R
	Rscript src/make_ad/make_adev.R $(DATE)

results/rds/mortres.rds results/rds/efflbres.rds &: data/ad/adev.rds data/td/tdlb.rds data/ad/adsl.rds src/external/functions.R src/make_res/res_functions.R src/make_res/stata.R src/make_res/make_res.R
	Rscript src/make_res/make_res.R 
	
results/dmc/$(DMC_REPORT): $(AD) $(TD) src/make_reports/dmc_report.Rmd
	Rscript -e 'rmarkdown::render("src/make_reports/dmc_report.Rmd", \
	output_dir = "results/dmc", output_file = "$(DMC_REPORT)", \
	knit_root_dir = "../../.", \
	params = list(viedoc_export = "$(VIEDOC_EXPORT_NAME)") \
	)'
	
results/main/$(MAIN_REPORT1): $(AD) $(TD) src/make_reports/main_report1.Rmd
	Rscript -e 'rmarkdown::render("src/make_reports/main_report1.Rmd", \
	output_dir = "results/main", output_file = "$(MAIN_REPORT1)", \
	knit_root_dir = "../../.", \
	params = list(viedoc_export = "$(VIEDOC_EXPORT_NAME)",
								pseudorandom = $(PSEUDORANDOM) ) \
	)'



