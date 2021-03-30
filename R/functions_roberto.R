# functions ----------------------------------------------------------
#' readObservations
#'
#' @param subfolder subfolder
#' @param rainFile rainFile
#' @param runoffFile runoffFile
#' @param temperatureFile temperatureFile
#' @param dateTimetz dateTimetz
#' @param dateTimeformat dateTimeformat
#' @param to_mmperhour to_mmperhour
#' @param NAval NAval
#'
#' @return ????
#' @export
#' @importFrom utils read.table
readObservations <- function(subfolder,
                             rainFile, runoffFile, temperatureFile,
                             dateTimetz, dateTimeformat,
                             to_mmperhour,
                             NAval){

  # make file paths
  filepaths <- sapply(X = c(rainFile, runoffFile, temperatureFile),
                      FUN = function(x){
                        file.path(subfolder, x)})

  # read data
  x <- lapply(X = filepaths,
              FUN = utils::read.table,
              sep = ";",
              header = TRUE,
              dec = ".",
              colClasses = c("character", "numeric"))

  names(x) <- c('rain', 'runoff', 'temperature')

  # format dateTime
  for(i in seq_along(x)){
    x[[i]]$dateTime <- as.POSIXct(
      x[[i]]$dateTime,
      format = dateTimeformat,
      tz = dateTimetz)
  }

  # convert rainfall and runoff units to mm/hour
  x$rain$rain <- x$rain$rain * to_mmperhour$rain
  x$runoff$runoff <- x$runoff$runoff * to_mmperhour$runoff

  # deal with NAs
  allocateNA <- function(a, NAvali){
    na <- a[[2]][a[[2]] == NAvali]
    if(length(na) > 0) {a[[2]][a[[2]] == NAvali] <- NA}
    return(a)
  }

  for(i in seq_along(x)){
    x[[i]] <- allocateNA(a = x[[i]], NAvali = NAval[i])
  }

  return(x)
}

#' readPredictions
#'
#' @param subfolder subfolder
#' @param rainFile rainFile
#' @param runoffFile runoffFile
#' @param temperatureFile temperatureFile
#' @param dateTimetz dateTimetz
#' @param dateTimeformat dateTimeformat
#' @param to_mmperhour to_mmperhour
#' @param parTcontinuous parTcontinuous
#'
#' @return ????
#' @export
#' @importFrom swmmr read_out
#' @importFrom xts tzone
#' @importFrom zoo coredata index
readPredictions <- function(subfolder,
                            rainFile, runoffFile, temperatureFile,
                            dateTimetz, dateTimeformat, to_mmperhour,
                            parTcontinuous){

  mod <- list()

  # read SWMM runoff
  mod$runoff <- swmmr::read_out(file.path(subfolder, runoffFile),
                                iType = 0,
                                object_name = 'S1',
                                vIndex = 4)

  mod$runoff <- mod$runoff$S1$runoff_flow
  xts::tzone(mod$runoff) <- dateTimetz
  mod$runoff <- mod$runoff * to_mmperhour$runoff
  mod$runoff <- data.frame(dateTime = zoo::index(mod$runoff),
                           runoff = zoo::coredata(mod$runoff))

  # read SWMM rainfall
  mod$rain <- utils::read.table(file.path(subfolder, rainFile), header=FALSE)
  date <- paste(mod$rain[[2]], mod$rain[[3]], mod$rain[[4]], sep = '-')
  time <- paste(mod$rain[[5]], mod$rain[[6]], sep = ':')
  mod$rain$dateTime <- as.POSIXct(paste(date, time),
                                  format = dateTimeformat,
                                  tz = dateTimetz)
  mod$rain$rain <- mod$rain[[7]]
  mod$rain <- data.frame(dateTime = mod$rain$dateTime,
                         rain = mod$rain$rain * to_mmperhour$rain)

  # read SWMM temperature (daily Tmin and Tmax)
  mod$TmaxTminDay <- utils::read.table(file.path(subfolder, temperatureFile),
                                header=FALSE)
  date <- paste(mod$TmaxTminDay[[2]],
                mod$TmaxTminDay[[3]],
                mod$TmaxTminDay[[4]],
                sep = '-')
  mod$TmaxTminDay$Tmax <- mod$TmaxTminDay[[5]]
  mod$TmaxTminDay$Tmin <- mod$TmaxTminDay[[6]]
  mod$TmaxTminDay <- data.frame(date = as.Date(date),
                                Tmax = mod$TmaxTminDay$Tmax,
                                Tmin = mod$TmaxTminDay$Tmin)

  # make continuous temperature series following SWMM's approach. see
  # SWMM hydrological reference manual, chapter 2.4
  data <- mod$TmaxTminDay
  tAxis <- mod$runoff$dateTime

  # longitude correction of local clock time
  dtlong <- 4*(parTcontinuous$longitude -
                 parTcontinuous$standardMeridian)

  mod$temperature <- data.frame(dateTime = mod$runoff$dateTime,
                                temperature = NA)
  mod$temperature$temperature <- sapply(
    X = seq_along(tAxis),
    FUN = function(i){

      # grab date, Tmin and Tmax
      timei <- tAxis[i]
      datei <- as.Date(timei)
      houri <- as.numeric(format(timei, format = '%H'))

      # grab current day's Tmin and Tmax
      Tmin <- data$Tmin[data$date == datei]
      Tmax <- data$Tmax[data$date == datei]

      # difference between the previous day’s maximum temperature and
      # the current day’s minimum temperature.
      TmaxPrev <- ifelse(i == 1,
                         parTcontinuous$TmaxDay0,
                         data[data$date == as.Date(
                           format(timei, format='%Y-%m-%d')) - 1,
                           'Tmax'])
      dt1 <- TmaxPrev - Tmin

      # earth's declination, in radians (D = julian day)
      D <- as.numeric(format(datei, format = '%j'))
      delta <- 23.45*pi/180*cos(2*pi/365*(172 - D))

      # hour angle of the sun (phi = latitude)
      hasun <- (12/pi)*acos(-tan(delta) * tan(parTcontinuous$latitude))

      # sunrise and sunset times
      hsr <- 12 - hasun + dtlong/60
      hss <- 12 + hasun + dtlong/60

      # hours at which Tmin and Tmax occur
      hourTmin = hsr
      hourTmax = hss - 3

      # temperature at timei
      if(houri < hourTmin){

        Ti <- Tmin + dt1/2*sin((pi*(hourTmin - houri))/(hourTmin + 24 - hourTmax))

      } else {

        if((hourTmin <= houri) & (houri <= hourTmax)){

          Ti <- (Tmin + Tmax)/2 +
            (Tmax - Tmin)/2*sin(
              pi*((hourTmax + hourTmin)/2 - houri) /
                (hourTmin - hourTmax))

        } else {

          Ti <- Tmax - (Tmax - Tmin)/2*sin(
            pi*(houri - hourTmax)/(hourTmin + 24 - hourTmax))
        }
      }

      return(Ti)

    })

  return(mod)
}

#' makeRainfallRunoffEvents
#'
#' @param rainfalldata rainfalldata
#' @param runoffdata runoffdata
#'
#' @return ????
#' @export
#' @importFrom dplyr left_join
#' @importFrom stats approx
#' @importFrom kwb.event getEvents

makeRainfallRunoffEvents <- function(rainfalldata, runoffdata){

  # function takes rainfall and runoff in mm/hour, which is what function
  # 'readObservations' generates

  # helper function for filtering storms from rainfall series
  filterstorm <- function(data, tBeg, tEnd){
    return(data[data$dateTime >= tBeg &
                  data$dateTime <= tEnd, ])
  }

  # helper function for numerical integration
  trapezIntegr <- function(data, column, tconv){

    data <- data[!is.na(data[[column]]), ]

    AA   <- data[[column]][2:nrow(data)]

    aa   <- data[[column]][1:(nrow(data) - 1)]

    hh   <- (as.numeric(data$dateTime[2:nrow(data)]) -
               as.numeric(data$dateTime[1:(nrow(data) - 1)]))*tconv

    return(sum((AA + aa)/2*hh))
  }

  # helper function to count no. days in monitoring time series
  countdays <- function(timeseries){

    day <- unique(as.Date(timeseries$dateTime))

    yearmonth <- as.character(format(day, format = '%Y-%m'))

    ndaysmonth <- stats::aggregate(x = day,
                            by = list(yearmonth),
                            FUN = length)

    colnames(ndaysmonth) <- c('yearmonth', 'ndays')

    return(ndaysmonth)
  }

  # interpolate runoff data onto time axis of rainfall data
  runoffdata2 <- stats::approx(
    x=runoffdata$dateTime,
    y=runoffdata$runoff,
    xout=rainfalldata$dateTime)

  runoffdata <- data.frame(
    dateTime = runoffdata2$x,
    runoff = runoffdata2$y)

  # make joint rainfall runoff events by summing rainfall + runnof and
  # getting events from the summed series
  rainrunoff <- data.frame(
    dateTime = runoffdata$dateTime,
    rainrunoff = rainfalldata$rain + runoffdata$runoff)

  rainrunoffevents <- kwb.event::getEvents(
    rainData = rainrunoff,
    seriesName = "rainrunoff",
    signalThreshold = 0)

  # add rainfall [mm], runoff and runoff coefficient
  x <- lapply(
    X = seq_along(rainrunoffevents$iBeg),
    FUN = function(i){

      tBeg <- rainrunoffevents$tBeg[i]
      tEnd <- rainrunoffevents$tEnd[i]

      rainsel <- filterstorm(rainfalldata, tBeg, tEnd)
      runoffsel <- filterstorm(runoffdata, tBeg, tEnd)

      eventrain <- trapezIntegr(rainsel, 'rain', tconv = 1/3600)
      eventrunoff <- trapezIntegr(runoffsel, 'runoff', tconv = 1/3600)
      eventruncoeff <- eventrunoff/eventrain

      return(cbind(rain = eventrain,
                   runoff = eventrunoff,
                   runoffcoefficient = eventruncoeff))})

  x <- data.frame(do.call(rbind, x))

  rainrunoffevents <- cbind(rainrunoffevents, x)


  # keep only full months

  ndays <- countdays(timeseries = rainrunoff)

  rainrunoffevents$yearmonth <- as.character(format(rainrunoffevents$tBeg, format = '%Y-%m'))

  rainrunoffevents <- dplyr::left_join(rainrunoffevents, ndays, by = 'yearmonth')

  rainrunoffevents <- rainrunoffevents[rainrunoffevents$ndays >= 28, ]

  return(rainrunoffevents)
}

removeBadEvents <- function(events, mindur_sec,
                            removeruncoeffNA,
                            removeruncoeff_gt_1,
                            removezerorain){

  events <- events[events$dur > mindur_sec, ]

  if(removeruncoeffNA){
    events <- events[!is.na(events$runoffcoefficient), ]
  }

  if(removezerorain){
    events <- events[events$rain > 0, ]
  }

  if(removeruncoeff_gt_1){
    events <- events[events$runoffcoefficient <= 1, ]
  }
}

TmaxADWP <- function(data){

  events <- data$rain_runoff
  rawrain <- data$rain
  rawtemperature <- data$temperature

  x <- sapply(
    X = seq_len(nrow(events)),
    FUN = function(i){

      tBeg <- events$tBeg[i]
      rainBefore <- rawrain[rawrain$dateTime < tBeg, ]
      indexRain <- which(rainBefore[[2]]>0)

      if(length(indexRain) > 0){

        rainBefore <- rainBefore[indexRain, ]
        tEndPrev <- max(rainBefore$dateTime)

        # select temperature data for time period between tEndPrev and tBeg
        ttsel <- rawtemperature[rawtemperature$dateTime >= tEndPrev &
                                  rawtemperature$dateTime <= tBeg, ]

        TmaxADWP <- max(ttsel$temperature, na.rm=TRUE)

      } else {

        TmaxADWP <- NA
      }

      return(TmaxADWP)
    })

  return(x)
}

#' monthlyPattern
#'
#' @param data data
#'
#' @return ???
#' @export
#' @importFrom stats aggregate
monthlyPattern <- function(data){

  x <- data$rain_runoff[, c('tBeg', 'rain', 'runoff',
                            'runoffcoefficient',
                            'TmaxADWP')]

  x$yearMonth <- as.character(format(x$tBeg, format = '%Y%m'))

  x.monthly <- stats::aggregate(
    . ~ yearMonth,
    data = x,
    FUN = function(x) c(sum = sum(x), mean = mean(x)))

  x.monthly <- data.frame(yearMonth = x.monthly$yearMonth,
                          rain = x.monthly$rain[, 'sum'],
                          runoff = x.monthly$runoff[, 'sum'],
                          meanTmaxADWP = x.monthly$TmaxADWP[, 'mean'])

  x.monthly$runoffcoefficient <- x.monthly$runoff/x.monthly$rain

  return(x.monthly)
}

# compute runoff volume for runoff in mm/s
#' computeVol
#' @description compute runoff volume for runoff in mm/s
#' @param data data
#' @param timeColumn timeColumn
#' @param Qcolumn Qcolumn
#'
#' @return ???
#' @export

computeVol <- function(data, timeColumn, Qcolumn){

  AA <- data[-1, Qcolumn]
  aa <- data[-nrow(data), Qcolumn]
  hh <- as.numeric(data[-1, timeColumn]) -
    as.numeric(data[-nrow(data), timeColumn])

  Vtot <- sum((AA + aa)/2*hh)

  return(Vtot)
}


