args <- commandArgs(TRUE)

rmd_file <- if (length(args) > 0) args[1] else "analysis_covid_evolution.Rmd"
rmarkdown::render(rmd_file)