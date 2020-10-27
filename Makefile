
VIEDOC_EXPORT_NAME = ous_20201016_084109
RAW_CSV = $(wildcard data/raw/$(VIEDOC_EXPORT_NAME)/*)
TD = data/td/tddm.rds data/td/tdran.rds data/td/tdae.rds data/td/tdex.rds

all: data/raw/raw.rds $(TD)


data/raw/raw.rds:  $(RAW_CSV) src/make_raw/make_raw.R src/external/functions.R
	Rscript src/make_raw/make_raw.R $(VIEDOC_EXPORT_NAME)


$(TD): data/raw/raw.rds src/external/functions.R src/make_td/make_td.R
	Rscript src/make_td/make_td.R
	

