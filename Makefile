
VIEDOC_EXPORT_NAME = ous_20201016_084109
DATE = $(shell echo $(VIEDOC_EXPORT_NAME) | sed 's/.*_\(....\)\(..\)\(..\)_.*/\1-\2-\3/')
DMC_REPORT = $(DATE)_Nor-Solidarity_DMC_Report.docx
RAW_CSV = $(wildcard data/raw/$(VIEDOC_EXPORT_NAME)/*)
TD = data/td/tddm.rds data/td/tdran.rds data/td/tdae.rds data/td/tdex.rds data/td/tdsq.rds data/td/tdsc.rds data/td/tdvs.rds 
AD = data/ad/adsl.rds data/ad/adae.rds data/ad/adeff.rds


td: $(TD)
ad: $(AD)
raw: data/raw/raw.rds
dmc_report: results/dmc/$(DMC_REPORT)
.PHONY: all td ad raw dmc_report
all: td ad raw dmc_report

data/raw/raw.rds:  $(RAW_CSV) src/make_raw/make_raw.R src/external/functions.R
	Rscript src/make_raw/make_raw.R $(VIEDOC_EXPORT_NAME)


$(TD): data/raw/raw.rds src/external/functions.R src/make_td/make_td.R
	Rscript src/make_td/make_td.R
	
$(AD): $(TD) data/raw/raw.rds src/external/functions.R src/make_ad/make_ad.R
	Rscript src/make_ad/make_ad.R
	
results/dmc/$(DMC_REPORT): $(AD) src/make_reports/dmc_report.Rmd
	echo $(DMC_REPORT) 
	Rscript -e 'rmarkdown::render("src/make_reports/dmc_report.Rmd", output_dir = "results/dmc", output_file = "$(DMC_REPORT)", knit_root_dir = "../../.")'
	
	
	
#	Rscript -e 'rmarkdown::render("src/make_reports/dmc_report.Rmd", output_format = "word_document", output_file = '$(DMC_REPORT)', knit_root_dir = "../.")'



