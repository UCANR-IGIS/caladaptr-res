######################################################################
## caladaptR Workshop Setup
##
## Please run the following lines of code *before* the workshop starts
## to install all the packages that we'll be using during the workshop.
##
## If you have any difficulties please email the instructor.
##
## For more info about caladaptr, visit:
## https://ucanr-igis.github.io/caladaptr/
######################################################################

## Define the required packages (these are all on CRAN)

pkg_req <- c("backports", "conflicted","crayon", "curl", "DBI",
             "dbplyr", "digest", "dplyr", "fastmatch", "ggplot2", "httr", "geojsonsf", "leaflet",
             "lifecycle", "lubridate", "magrittr", "purrr", "remotes", "rmarkdown", "RSQLite", 
             "scales", "sf", "shiny", "stars", "tibble", "tidyr", "tmap", "units", "usethis", "zip")

## OPTION 1. Install a fresh version of *all* required packages (recommended)
##  - if it asks you to restart R more than once, select 'no'.
##  - if it asks whether you want to install from source, select 'no'


install.packages(pkg_req, dependencies = TRUE)

## OPTION 2. Install only missing packages
## Uncomment and run the two next lines to just install missing packages:
# (pkg_missing <- setdiff(pkg_req, rownames(installed.packages())))
# install.packages(pkg_missing, dependencies = TRUE)

## INSTALL caladaptr
##   - If it asks you whether you want to update a zillion packages, you can generally skip these
##   - Windows users *must* have RTools installed to install packages on GitHub

remotes::install_github("ucanr-igis/caladaptr")

## Install caladaptr.apps

remotes::install_github("ucanr-igis/caladaptr.apps")

## Load caladaptr

library(caladaptr)

## DID IT WORK?
##
## Try to fetch some climate data

library(ggplot2); library(units); library(dplyr)

ca_example_apireq(1) %>%
  ca_getvals_tbl() %>%
  mutate(temp_f = set_units(val, degF)) %>%
  ggplot(aes(x = as.Date(dt), y = as.numeric(temp_f))) +
  geom_line(aes(color=gcm)) +
  labs(title = "Daily Max Temp Averaged by Year, Sacramento, RCP4.5", x = "year", y = "temp (F)")

## See a plot? Done!

