########################################################
## INSTALL PACKAGES WE'LL BE USING FOR THE WORKSHOP
########################################################

## Install sf before everything else
## (b/c on Linux it installs by itself fine, but not as a dependency)
if (!require(sf)) install.packages("sf")

# Install other CRAN packages as needed
pkgs_req <- c("tidyverse", "conflicted", "leaflet", "rmarkdown", "scales")
pkgs_missing <- pkgs_req[!(pkgs_req %in% installed.packages()[,"Package"])]
if (length(pkgs_missing)) install.packages(pkgs_missing)

## Install other packages from the r-universe
## (this can take a while on Linux because many dependencies have to be compiled)
options(repos = c(ajlyons = "https://ajlyons.r-universe.dev",
                  CRAN = "https://cloud.r-project.org"))
install.packages(c('caladaptr', 'caladaptr.apps', 'wrkshputils'))

########################################################
## To verify caladaptR is working properly, run the following:
## (select all the lines and click the 'run' button)
########################################################

library(caladaptr)
library(ggplot2)
library(units)
library(dplyr)

bakersfield_cap <- ca_loc_pt(coords = c(-119.0, 35.4)) %>%
  ca_gcm(gcms[1:4]) %>%
  ca_scenario("rcp85") %>%
  ca_period("year") %>%
  ca_years(start = 2030, end = 2099) %>%
  ca_cvar("tasmax")

bakersfield_cap %>% 
  ca_getvals_tbl() %>%
  mutate(temp_f = set_units(val, degF)) %>%
  ggplot(aes(x = as.Date(dt), y = as.numeric(temp_f))) +
  geom_line(aes(color=gcm)) +
  labs(title = "Daily Max Temp Averaged by Year, Bakersfield, RCP8.5", x = "year", y = "temp (F)")
  
## If you see a plot - it's working!

