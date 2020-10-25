library(tidyverse)
library(glue)

args <- commandArgs(trailingOnly = TRUE)
if (length(args)==0) {
  export_name <- "ous_20201016_084"
} else if (length(args)==1) {
  export_name <- args[1]
}

args
length(args)
export_name