
VIEDOC_EXPORT_NAME = ous_20201121_154631
DATE = $(shell echo $(VIEDOC_EXPORT_NAME) | sed 's/.*_\(....\)\(..\)\(..\)_.*/\1-\2-\3/')
DMC_REPORT = $(DATE)_Nor-Solidarity_DMC_Report.docx
RAW_CSV = $(wildcard data/raw/$(VIEDOC_EXPORT_NAME)/*)
TDMISC = data/td/tdae.rds data/td/tdex.rds data/td/tdsq.rds data/td/tdsc.rds data/td/tdcm.rds data/td/tdds.rds
TD = data/td/tdran.rds $(TDMISC) data/td/tddm.rds data/td/tdrc.rds data/td/tdvs.rds
AD = data/ad/adsl.rds data/ad/adae.rds data/ad/adeff.rds data/ad/adex.rds data/ad/addm.rds

# Set this to FALSE when for the true results.  
PSEUDORANDOM = FALSE

.PHONY: all td ad raw dmc_report
all: td ad raw 
td: $(TD)
ad: $(AD)
raw: data/raw/raw.rds
dmc_report: results/dmc/$(DMC_REPORT)


data/raw/raw.rds:  $(RAW_CSV) src/make_raw/make_raw.R src/external/functions.R
	Rscript src/make_raw/make_raw.R $(VIEDOC_EXPORT_NAME)

data/td/tdran.rds: data/raw/raw.rds src/external/functions.R src/make_td/make_tdran.R
	Rscript src/make_td/make_tdran.R

$(TDMISC): data/raw/raw.rds data/td/tdran.rds src/external/functions.R src/make_td/make_tdmisc.R
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


#$(AD): $(TD) data/raw/raw.rds src/external/functions.R src/make_ad/make_ad.R
#	Rscript src/make_ad/make_ad.R $(PSEUDORANDOM)
	
results/dmc/$(DMC_REPORT): $(AD) src/make_reports/dmc_report.Rmd
	Rscript -e 'rmarkdown::render("src/make_reports/dmc_report.Rmd", \
	output_dir = "results/dmc", output_file = "$(DMC_REPORT)", \
	knit_root_dir = "../../.", \
	params = list(viedoc_export = "$(VIEDOC_EXPORT_NAME)") \
	)'
	
	
	
#	Rscript -e 'rmarkdown::render("src/make_reports/dmc_report.Rmd", output_format = "word_document", output_file = '$(DMC_REPORT)', knit_root_dir = "../.")'



