
VIEDOC_EXPORT_NAME = ous_20201016_084109
RAW_CSV = $(wildcard data/raw/$(RAW_FOLDER)/*)

.PHONY: raw_rds
raw_rds: data/raw/raw.rds

raw_rds:  $(RAW_CSV) src/make_raw/make_raw.R src src/external/functions.R
	Rscript src/make_raw/make_raw.R $(VIEDOC_EXPORT_NAME)
