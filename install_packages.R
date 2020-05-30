#! /usr/local/bin/Rscript

pkgs <- c("dplyr","forcats", "ggplot2", "lubridate", "scales", "zoo")
install.packages(pkgs, dependencies = TRUE, repos = "http://cran.rstudio.com/")