## This is a modified version of chillR::make_hourly_temps()
## https://cran.r-project.org/web/packages/chillR/index.html
## Author: Eike Luedeling 
## Modified by: Andy Lyons

## THIS FUNCTION IS CALLED BY A SHINY APP
## see https://github.com/UCANR-IGIS/caladaptr-res/tree/main/shiny/chill

ca_make_hourly_temps <-  function (year_file, latitude) {

  ## Expectations:
  ## year_file is a data frame with lots of rows and the following columns:
  ## Year, Month, Day, gcm, scenario, gs, Tmax, Tmin  (Tmax and Tmin are type numeric - not UNITS)

  ## We're *not* going to load chillR because one of its dependencies is problematic.
  ## Instead the Shiny app that calls this function sources the required functions from chillR 
  # if (!requireNamespace("chillR")) stop("Sorry chillR is a required namespace")
  
  ## lubridate is checked in app.R, don't need to do it again
  ## if (!requireNamespace("lubridate")) stop("Sorry package lubridate is required for this function.")
  
  if(missing(latitude)) stop("'latitude' not specified")
  if(length(latitude) > 1) stop("'latitude' has more than one element")
  if(!is.numeric(latitude)) stop("'latitude' is not numeric")
  if(latitude > 90 | latitude < (-90)) stop("'latitude' should be between -90 and 90")

  ## Filter out rows that have NAs in Tmin and Tmax,
  year_file <- year_file[which(!is.na(year_file$Tmin) & !is.na(year_file$Tmax)), ] 
  
  ## and convert Tmin and Tmax to numeric (in case they come in as "units")  - NO LONGER NEEDED
  ## %>% mutate(Tmin = as.numeric(Tmin), Tmax = as.numeric(Tmax))

  ## If there's no JDay column, create it
  ## Note: lubridate::make_date is *much* faster than base::ISODate
  
  if(!"JDay" %in% colnames(year_file)) {
    year_file[["JDay"]] <- lubridate::make_date(year = year_file[["Year"]],
                                                month = year_file[["Month"]],
                                                day = year_file[["Day"]]) %>% 
      lubridate::yday()
  }
    
  ## Compute day length. Day_times will be a list.
  Day_times <- chillR::daylength(latitude = latitude,
                       JDay = c(year_file$JDay[1] - 1,
                              year_file$JDay,
                              year_file$JDay[nrow(year_file)]+1))

  ## TUrn 99 and -99 to 0, 12, and 24
  Day_times$Sunrise[which(Day_times$Sunrise == 99)] <- 0
  Day_times$Sunrise[which(Day_times$Sunrise == -99)] <- 12
  Day_times$Sunset[which(Day_times$Sunset == 99)] <- 24
  Day_times$Sunset[which(Day_times$Sunset == -99)] <- 12

  ## Add columns to year_file that will be needed to interpolate hourly temps:
  ## sunrise, sunset, Daylength, prev sunset, next sunrise, previous tmax, next tmin

  year_file$Sunrise <- Day_times$Sunrise[2:(length(Day_times$Sunrise)-1)]
  year_file$Sunset <- Day_times$Sunset[2:(length(Day_times$Sunset)-1)]
  year_file$Daylength <- Day_times$Daylength[2:(length(Day_times$Daylength)-1)]
  year_file$prev_Sunset <- Day_times$Sunset[1:(length(Day_times$Sunset)-2)]
  year_file$next_Sunrise <- Day_times$Sunrise[3:length(Day_times$Sunrise)]
  year_file$prev_max <- year_file$Tmax[c(NA,1:(nrow(year_file)-1))]
  year_file$next_min <- year_file$Tmin[c(2:nrow(year_file),NA)]
  year_file$prev_min <- year_file$Tmin[c(NA,1:(nrow(year_file)-1))]

  year_file$Tsunset <- year_file$Tmin + (year_file$Tmax-year_file$Tmin) *
    sin((pi * (year_file$Sunset - year_file$Sunrise) / (year_file$Daylength + 4)))

  year_file$prev_Tsunset<-year_file$prev_min + (year_file$prev_max - year_file$prev_min) *
    sin((pi * (year_file$Daylength) / (year_file$Daylength + 4)))

  ## WE'RE DONE ADDING COLUMNS TO year_file FOR THE INTERPOLATION

  ## GET READY FOR THE LOOP

  ## If we're return long format, we :
  ## 1) identify the column numbers we want to keep
  ## 2) create a blank data frame with 0 rows that we'll append as we go

  ## Record the column numbers that we want to retain  
  keep_cols_idx <- which(names(year_file) %in% c("Year", "Month", "Day", "gs", "gcm", "scenario"))

  ## Create an empty list to hold the hourly results
  res_lst <- vector("list", 24)

  for (hour_int in 0:23) {

    ## Find the rows where this hour_int is before sunrise
    ## Time this next part, it seems slow
    c_morn <- which(hour_int <= year_file$Sunrise)  

    ## If the first row (day) is part of c_morn, take it out
    ## because you can't compute temperatures before sunrise on the first day 
    ## because there is no time or temperature for the previous sunset
    
    if(1 %in% c_morn) {
      if(!length(c_morn) == 1) {
        c_morn <- c_morn[2:length(c_morn)]
      }
    } 

    ## Find the rows where this hour_int is between sunrise and sunset
    c_day <- which(hour_int > year_file$Sunrise & hour_int <= year_file$Sunset)

    ## Find the rows where this hour_int is after sunset
    c_eve <- which(hour_int >= year_file$Sunset)

    ## If the very last last record is in the 'after sunset' category, remove it
    ## can't compute temperatures after sunset for last day
    if(nrow(year_file) %in% c_eve) c_eve <- c_eve[1:(length(c_eve)-1)] 

    ## Create a vector of NAs to store the hourly temperature values
    hourly_temp <- rep(NA, nrow(year_file))

    ## Fill in the temp for those days when hour_int is before sunset
    hourly_temp[c_morn] <-
      year_file$prev_Tsunset[c_morn] -  #prev temp at sunset
      ((year_file$prev_Tsunset[c_morn] - year_file$Tmin[c_morn]) /
         log(max(1, 24 - (year_file$prev_Sunset[c_morn] - year_file$Sunrise[c_morn]))) *
         log(hour_int + 24 - year_file$prev_Sunset[c_morn] + 1))

    ## Fill in the temp for those days when hour_int is between sunrise and sunset
    hourly_temp[c_day] <-
      year_file$Tmin[c_day] +
      (year_file$Tmax[c_day] - year_file$Tmin[c_day]) *
      sin((pi * (hour_int - year_file$Sunrise[c_day]) /
             (year_file$Daylength[c_day]+4)))

    ## Fill in the temp for those days when hour_int is after sunrise
    hourly_temp[c_eve] <-
      year_file$Tsunset[c_eve] - 
      ((year_file$Tsunset[c_eve] - year_file$next_min[c_eve]) /
         log(24 - (year_file$Sunset[c_eve] - year_file$next_Sunrise[c_eve]) + 1) *
         log(hour_int - year_file$Sunset[c_eve] + 1))

    ## Gap fill NA values on the first day (probably due to hour < Sunrise) with the daily min
    if (is.na(hourly_temp[1])) {
      hourly_temp[1] <- year_file[1, "Tmin", drop = TRUE]
    }

    ## Likewise fill in NA values on the last day (probably due to hour_int > Sunset) with the daily min
    if (is.na(hourly_temp[length(hourly_temp)])) {
      hourly_temp[length(hourly_temp)] <- year_file[length(hourly_temp), "Tmin", drop = TRUE]
    }

    if (anyNA(hourly_temp)) {
      cat(" =============== FOUND A NA IN houlrly_temp ===================== \n")
    }
    
    ## Add this vector of hourly temps to res_lst
    res_lst[[hour_int + 1]] <- hourly_temp

  }

  ## We're done. Return a tibble with the results.
  do.call(rbind, lapply(1:24, function(i) tibble(year_file[ , keep_cols_idx], 
                                                  Hour = i - 1,
                                                  Temp = res_lst[[i]])))

}
