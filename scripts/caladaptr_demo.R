# Install the package
# remotes::install_github("ucanr-igis/caladaptr")

# Load the package
library(caladaptr)

# Create a Caladapt API Request for a point location near Fresno

fresno_cap <- ca_loc_pt(coords = c(-119.93, 36.73)) %>%
  ca_gcm(c("HadGEM2-ES", "CNRM-CM5", "CanESM2", "MIROC5")) %>% 
  ca_scenario(c("rcp45", "rcp85")) %>%
  ca_period("year") %>%
  ca_years(start = 2020, end = 2099) %>%
  ca_cvar(c("tasmin"))

## Plot the API request object to double-check the location

plot(fresno_cap)

## Fetch the data, and put it into a data frame (tibble)

fresno_tbl <- fresno_cap %>% ca_getvals_tbl()

## View the results

fresno_tbl

## Add a column that converts the temperature values from Kelvin to 
## Fahrenheit 

library(dplyr); library(units)

fresno_tempf_tbl <- fresno_tbl %>%
  mutate(temp_f = set_units(val, degF))

## View the results

fresno_tempf_tbl

## Plot the results

library(ggplot2)

ggplot(data = fresno_tempf_tbl, aes(x = as.Date(dt), y = as.numeric(temp_f))) +
  geom_line(aes(color=gcm)) +
  facet_wrap( ~ scenario) +
  labs(title = "Minimum Daily Temperature Averaged by Year", 
       x = "year", y = "temp (F)")


