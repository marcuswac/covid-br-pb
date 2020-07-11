#!/bin/bash

# update covid-br-data
cd data
./update_covid_data.sh

# update data submodule
cd ..
git submodule update --recursive --remote

# rendering markdown
git pull
Rscript render_rmarkdown.R index.Rmd && ./update_github.sh
