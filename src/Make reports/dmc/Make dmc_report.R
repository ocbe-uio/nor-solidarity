
setwd(rprojroot::find_rstudio_root_file())

rmarkdown::render(
  "src/Make reports/dmc/dmc_report.Rmd",
  output_format = "word_document",
  output_dir = "results/dmc",
  output_file = glue::glue('{format(Sys.time(), "%Y-%m-%d")} Nor-Solidarity DMC report.docx'),
  knit_root_dir = "../.", 
  clean = FALSE
  )
