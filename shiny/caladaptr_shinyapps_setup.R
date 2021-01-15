######################################################################
## Running caladaptR Demo Shiny Apps from RStudio
##
## Run the following lines of code one-by-one to install the packages 
## needed to run the demo Shiny apps from caladaptR.
##
## For more info about caladaptR, visit: https://ucanr-igis.github.io/caladaptr/
##
######################################################################
## 1) Install packages needed by caladaptR (if you don't already have it installed)

req_pkg <- c("assertthat", "backports", "chillR", "conflicted","crayon", "curl", "DBI",
             "dbplyr", "digest", "dplyr", "fastmatch", "ggplot2", "httr", "geojsonsf", "lubridate",
             "magrittr", "purrr", "remotes", "rmarkdown", "RSQLite", "scales", "sf", "shiny", "stars", "stringr",
             "tibble", "tidyr", "tmap", "units", "usethis", "zip")

## Install a fresh version of *all* required packages (recommended)
##  - if it asks you to restart R more than once, select 'no'.
##  - if it asks whether you want to install from source, select 'no'

install.packages(req_pkg, dependencies = TRUE)

## OR uncomment the next line to just install missing packages:
# install.packages(setdiff(req_pkg, rownames(installed.packages())), dependencies = TRUE)

######################################################################
## 2) Install caladaptR

## WINDOWS USERS ALSO NEED TO  HAVE RTOOLS INSTALLED, SEE:
## https://cran.r-project.org/bin/windows/Rtools/

## (If it asks you whether you want to update a zillion packages, you can generally skip these
## unless its one of the above.)

if (!require(remotes)) install.packages("remotes", dependencies = TRUE) 
remotes::install_github("ucanr-igis/caladaptr")

## Load caladaptr

library(caladaptr)

## Want to test it out immediately? Jump down to step 5 below.

#############################################################
## 3) Install some additional packages needed by the Shiny apps

req_pkg_addl <- c("leaflet", "ggplot2", "tidyr", "stringr", "shinyhelper", "chillR", 
                  "DT", "lubridate", "scales", "conflicted")

## Install just missing packages:
install.packages(setdiff(req_pkg_addl, rownames(installed.packages())), dependencies = TRUE) 

## Or you can install a fresh copy of all of them 
## install.packages(req_pkg_addl, dependencies = TRUE)

#############################################################
## 4) Finally! You can run one of the demo Shiny Apps

## Plot a time series of anuual data
shiny::runUrl("https://github.com/ucanr-igis/caladaptr-res/raw/main/shiny/shiny_timeseries.zip")

## Compute projected chill portions under climate change (for tree crops)
shiny::runUrl("https://github.com/ucanr-igis/caladaptr-res/raw/main/shiny/shiny_chill.zip")


#############################################################
## 5) Getting started - plot some climate data yourself

library(ggplot2); library(units); library(dplyr)

ca_example_apireq(1) %>%
  ca_getvals_tbl() %>%
  mutate(temp_f = set_units(val, degF)) %>%
  ggplot(aes(x = as.Date(dt), y = as.numeric(temp_f))) +
  geom_line(aes(color=gcm)) +
  labs(title = "Annual Max Temp for Sacramento", x = "year", y = "temp (F)")

## See a plot? Done!

