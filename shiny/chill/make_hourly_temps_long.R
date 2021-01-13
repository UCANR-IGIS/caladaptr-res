## This is a modified version of chillr::make_hourly_temps()
## https://cran.r-project.org/web/packages/chillR/index.html
## Author: Eike Luedeling 
## Modified by: Andy Lyons

make_hourly_temps_long <-  function (latitude, year_file, keep_sunrise_sunset = FALSE, long_format = TRUE) {

    requireNamespace("chillR")  
  
    if(missing(latitude)) stop("'latitude' not specified")
    if(length(latitude) > 1) stop("'latitude' has more than one element")
    if(!is.numeric(latitude)) stop("'latitude' is not numeric")
    if(latitude > 90 | latitude < (-90)) warning("'latitude' is usually between -90 and 90")

    ## Filter out rows that have NAs in Tmin and Tmax
    year_file <- year_file[which(!is.na(year_file$Tmin) & !is.na(year_file$Tmax)), ]

    ## If there's no JDay column, create it
    if(!"JDay" %in% colnames(year_file))
      year_file[,"JDay"] <- strptime(paste(year_file$Month,"/",year_file$Day,"/",year_file$Year,sep=""),"%m/%d/%Y")$yday + 1

    ## Make a copy of the column names
    preserve_columns <- colnames(year_file)

    ## Compute day length
    Day_times <- chillR::daylength(latitude=latitude,
                         JDay=c(year_file$JDay[1]-1,
                                year_file$JDay,
                                year_file$JDay[nrow(year_file)]+1))

    ## TUrn 99 and -99 to 0, 12, and 24
    Day_times$Sunrise[which(Day_times$Sunrise == 99)] <- 0
    Day_times$Sunrise[which(Day_times$Sunrise == -99)] <- 12
    Day_times$Sunset[which(Day_times$Sunset == 99)] <- 24
    Day_times$Sunset[which(Day_times$Sunset == -99)] <- 12

    ## Add columns that will be needed to interpolate hourly temps:
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
      sin((pi * (year_file$Sunset - year_file$Sunrise) / (year_file$Daylength+4)))

    year_file$prev_Tsunset<-year_file$prev_min + (year_file$prev_max - year_file$prev_min)*
      sin((pi*(year_file$Daylength) / (year_file$Daylength+4)))

    ## WE'RE DONE ADDING COLUMNS TO year_file FOR THE INTERPOLATION

    ## GET READY FOR THE LOOP

    ## If we're return long format, we :
    ## 1) identify the column numbers we want to keep
    ## 2) create a blank data frame with 0 rows that we'll append as we go

    if (long_format) {
      keep_cols_idx <- which(names(year_file) %in% preserve_columns)
      if (keep_sunrise_sunset) {
        keep_cols_idx <- c(keep_cols_idx,
                           which(names(year_file) %in% c("Sunrise", "Sunset")))
      }
      res <- year_file[integer(0), keep_cols_idx]
    }

    ## Identify the column where we'll start adding columns
    colnum <- ncol(year_file) + 1

    ## Create an object for all the column numbers
    hourcol <- c(colnum:(colnum+23))

    for (hour in 0:23) {

      ## Do some kind of correction
      no_riseset <- which(year_file$Daylength %in% c(0,24,-99))
      year_file[no_riseset, colnum + hour] <- ((year_file$Tmax + year_file$Tmin) / 2)[no_riseset]

      ## Find the rows where this hour is before sunrise
      c_morn <- which(hour <= year_file$Sunrise)

      ## If the first row is part of c_morn, take it out
      ## because you can't compute temperatures before sunrise on the first day 
      ## because there is no time or temperature for the previous sunset
      
      if(1 %in% c_morn) {
        if(!length(c_morn) == 1) {
          c_morn <- c_morn[2:length(c_morn)]
        }
      } else {
        # c_morn <- c()    ## THIS LOOKS LIKE A POSSIBLE ERROR FROM THE ORIGINAL
      }

      ## Find the rows where this hour is between sunrise and sunset
      c_day <- which(hour > year_file$Sunrise & hour <= year_file$Sunset)

      ## Find the rows where this hour is after sunset
      c_eve <- which(hour >= year_file$Sunset)

      ## If the very last last record is in the 'after sunset' category, remove it
      if(nrow(year_file) %in% c_eve) c_eve <- c_eve[1:(length(c_eve)-1)] #can't compute temperatures after sunset for last day

      if (long_format) {

        ## Create a vector of NAs to store the hourly temperature values
        hourly_temp <- rep(NA, nrow(year_file))

        ## Fill in the temp for this hour for those days when this hour is before sunset
        hourly_temp[c_morn] <-
          year_file$prev_Tsunset[c_morn] -  #prev temp at sunset
          ((year_file$prev_Tsunset[c_morn] - year_file$Tmin[c_morn]) /
             log(max(1, 24 - (year_file$prev_Sunset[c_morn] - year_file$Sunrise[c_morn]))) *
             log(hour + 24 - year_file$prev_Sunset[c_morn] + 1))

        ## Fill in the temp for this hour when this hour is between sunrise and sunset
        hourly_temp[c_day] <-
          year_file$Tmin[c_day] +
          (year_file$Tmax[c_day] - year_file$Tmin[c_day]) *
          sin((pi * (hour-year_file$Sunrise[c_day]) /
                 (year_file$Daylength[c_day]+4)))

        ## Fill in the temp for this hour when this hour is after sunrise
        hourly_temp[c_eve] <-
          year_file$Tsunset[c_eve] - #temp at sunset
          ((year_file$Tsunset[c_eve]-year_file$next_min[c_eve]) /
             log(24 - (year_file$Sunset[c_eve] - year_file$next_Sunrise[c_eve]) + 1) *
             log(hour - year_file$Sunset[c_eve] + 1))

        ## Fill in NA values on the first day (probably due to hour < Sunrise) with the daily min
        if (is.na(hourly_temp[1])) {
          hourly_temp[1] <- year_file[1, "Tmin", drop = TRUE]
        }

        ## Likewise fill in NA values on the last day (probably due to hour > Sunset) with the daily min
        if (is.na(hourly_temp[length(hourly_temp)])) {
          hourly_temp[length(hourly_temp)] <- year_file[length(hourly_temp), "Tmin", drop = TRUE]
        }

        ## Append these to the result
        res <- rbind(res,
                     tibble(year_file[ , keep_cols_idx],
                                Hour = hour,
                                Temp = hourly_temp))

      } else {

        ## Fill in the  rows for this hour
        year_file[c_morn, colnum + hour] <-
          year_file$prev_Tsunset[c_morn] -
          ((year_file$prev_Tsunset[c_morn] - year_file$Tmin[c_morn]) /
             log(max(1, 24 - (year_file$prev_Sunset[c_morn] - year_file$Sunrise[c_morn]))) *
             log(hour + 24 - year_file$prev_Sunset[c_morn] + 1))

        ## Fill in the day rows
        year_file[c_day, colnum + hour] <-
          year_file$Tmin[c_day] +
          (year_file$Tmax[c_day] - year_file$Tmin[c_day]) *
          sin((pi * (hour-year_file$Sunrise[c_day]) /
                 (year_file$Daylength[c_day]+4)))

        ## Fill in the evening rows
        year_file[c_eve, colnum + hour] <-
          year_file$Tsunset[c_eve] - #temp at sunset
          ((year_file$Tsunset[c_eve]-year_file$next_min[c_eve]) /
             log(24 - (year_file$Sunset[c_eve] - year_file$next_Sunrise[c_eve]) + 1) *
             log(hour - year_file$Sunset[c_eve] + 1))

      }

    }

    if (long_format) {
      ## We're done. Just return the result.
      res

    } else {
      
      ## Edit the new column names
      colnames(year_file)[(ncol(year_file)-23):(ncol(year_file))]<-c(paste("Hour_",0:23,sep=""))
      
      if (!keep_sunrise_sunset)
        year_file <- year_file[,c(preserve_columns,paste("Hour_",0:23,sep=""))]

      if (keep_sunrise_sunset)
        year_file <- year_file[,c(preserve_columns,"Sunrise","Sunset","Daylength",paste("Hour_",0:23,sep=""))]

      ## Fill in NA temperature values of the first day (for which there is no previous day sunset temp) with tmin

      year_file[1,(ncol(year_file) - 23):(ncol(year_file))][which(is.na(year_file[1,(ncol(year_file)-23):(ncol(year_file))]))] <-
        year_file[1,"Tmin"]

      year_file[nrow(year_file),(ncol(year_file)-23):(ncol(year_file))][which(is.na(year_file[nrow(year_file),(ncol(year_file) - 23):(ncol(year_file))]))] <-
        year_file[nrow(year_file),"Tmin"]

      return(year_file)

    }

  }
