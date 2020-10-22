# kuras (Neubrandenburg): rain mm/5min, runoff mm/5min, dt =5min, green roof area = 101 m2
# basar (Berlin): rain mm/5min, runoff mm/5min, dt = 5min, green roof area = 194 m2

# load observed rainfall, runoff and temperature, transforming to mm/hour
obs.neubrandenburg <- readObservations(
  subfolder = 'data_green_roof',
  rainFile = 'obs_rain_5min_Neubrandenburg.txt',
  runoffFile = 'obs_runoff_5min_Neubrandenburg.txt',
  temperatureFile = 'obs_temperature_10min_Neubrandenburg.txt',
  dateTimetz = 'Etc/GMT-1', 
  dateTimeformat = '%Y-%m-%d %H:%M:%S',
  to_mmperhour = list(rain=1/(5/60), runoff=1/(5/60)), 
  NAval = list(rain = -999, runoff = -999, temperature = -999))

obs.berlin <- readObservations(
  subfolder = 'data_green_roof',
  rainFile = 'obs_rain_5min_Berlin.txt',
  runoffFile = 'obs_runoff_5min_Berlin.txt',
  temperatureFile = 'obs_temperature_10min_Berlin.txt',
  dateTimetz = 'Etc/GMT-1', 
  dateTimeformat = '%Y-%m-%d %H:%M:%S',
  to_mmperhour = list(rain=1/(5/60), runoff=1/(5/60)),
  NAval = list(rain = -999, runoff = -999, temperature = -999))

# load modeled runoff, together with rainfall and temperature used as model inputs,
# transforming to mm/hour
# since SWMM uses daily Tmax, Tmin and a sinusoidal function to produce continuous
# T data, readPredictions does the same using the same formulas given in SWMM's
# reference manual
mod.neubrandenburg <- readPredictions(
  subfolder = 'models_green_roof',
  rainFile = 'obs_rain_5min_Neubrandenburg.txt',
  runoffFile = 'neubrand.out', # SWMM output file
  temperatureFile = 'obs_temp_daily_Neubrandenburg.txt',
  dateTimetz = 'Etc/GMT-1',
  dateTimeformat = '%Y-%m-%d %H:%M',
  to_mmperhour = list(rain = 1/(5/60), runoff = 3600/101),
  parTcontinuous = list(longitude = -13.26,
                        standardMeridian = -15,
                        latitude = 53.56,
                        TmaxDay0 = 17))
  
mod.berlin <- readPredictions(
  subfolder = 'models_green_roof',
  rainFile = 'obs_rain_5min_Berlin.txt',
  runoffFile = 'bbr18.out', # SWMM output file
  temperatureFile = 'obs_temp_daily_Berlin.txt',
  dateTimetz = 'Etc/GMT-1',
  dateTimeformat = '%Y-%m-%d %H:%M',
  to_mmperhour = list(rain = 1/(5/60), runoff = 3600/194),
  parTcontinuous = list(longitude = -13.41,
                        standardMeridian = -15,
                        latitude = 52.50,
                        TmaxDay0 = 9))

# make joint rainfall-runoff events (observed)
obs.neubrandenburg$rain_runoff <- makeRainfallRunoffEvents(
  rainfalldata = obs.neubrandenburg$rain,
  runoffdata = obs.neubrandenburg$runoff)

obs.berlin$rain_runoff <- makeRainfallRunoffEvents(
  rainfalldata = obs.berlin$rain,
  runoffdata = obs.berlin$runoff)

# make joint rainfall-runoff events (modeled)
mod.neubrandenburg$rain_runoff <-  makeRainfallRunoffEvents(
  rainfalldata = mod.neubrandenburg$rain,
  runoffdata = mod.neubrandenburg$runoff)

mod.berlin$rain_runoff <- makeRainfallRunoffEvents(
  rainfalldata = mod.berlin$rain,
  runoffdata = mod.berlin$runoff)

# compute max. temperature in antecedent dry weather period (ADWP) (observed)
obs.neubrandenburg$rain_runoff$TmaxADWP <- TmaxADWP(obs.neubrandenburg)
obs.berlin$rain_runoff$TmaxADWP <- TmaxADWP(obs.berlin)

# compute max. temperature in antecedent dry weather period (ADWP) (modeled)
mod.neubrandenburg$rain_runoff$TmaxADWP <- TmaxADWP(mod.neubrandenburg)
mod.berlin$rain_runoff$TmaxADWP <- TmaxADWP(mod.berlin)

# monthly patterns in reality vs. monthly patterns in SWMM
obs.neubrandenburg.monthly <- monthlyRunoffCoeff(obs.neubrandenburg)
obs.berlin.monthly <- monthlyRunoffCoeff(obs.berlin)
mod.neubrandenburg.monthly <- monthlyRunoffCoeff(mod.neubrandenburg)
mod.berlin.monthly <- monthlyRunoffCoeff(mod.berlin)

summary(lm(runoff ~ rain + meanTmaxADWP, data = obs.neubrandenburg.monthly))
summary(lm(runoff ~ rain + meanTmaxADWP, data = mod.neubrandenburg.monthly))







# remove unusable events (too short, NAs, runoff coeff >1)  
rrEvents.neu <- rrEvents.neu[rrEvents.neu$dur>300, ]
rrEvents.neu <- stats::na.omit(rrEvents.neu)
rrEvents.neu <- rrEvents.neu[rrEvents.neu$runoffCoeff<=1, ]


# Summarize rrEvents.neu to Monthly aggregation
obs.events <- rrEvents.neu %>% 
  mutate(day=day(tBeg),
         week=week(tBeg),
         month=month(tBeg),
         year=year(tBeg),
         id=as.Date(paste(year, month, 1, sep = '-'), '%Y-%m-%d')) %>%
  group_by(id) %>%  
  summarise(dur=sum(dur/3600), 
            adwp = sum(pBefore/86400),
            hN=sum(hN),
            RO=sum(ROvol), 
            runoffCoeff = RO/hN,
            retCoeff = 1 - runoffCoeff,
            Rs=sum(Rs),
            ET=sum(ET),
            Tmax = max(Tmax), 
            Tmin = min(Tmin),
            Wind = mean(Wind),
            n_events=n())


sim <- swmmr::run_swmm(inp = 'models_green_roof/neubrand.INP')

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
              FUN = read.table,
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

readPredictions <- function(subfolder, rainFile, runoffFile, temperatureFile,
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
  mod$rain <- read.table(file.path(subfolder, rainFile), header=FALSE)
  date <- paste(mod$rain[[2]], mod$rain[[3]], mod$rain[[4]], sep = '-')
  time <- paste(mod$rain[[5]], mod$rain[[6]], sep = ':')
  mod$rain$dateTime <- as.POSIXct(paste(date, time),
                                  format = dateTimeformat,
                                  tz = dateTimetz)
  mod$rain$rain <- mod$rain[[7]]
  mod$rain <- data.frame(dateTime = mod$rain$dateTime,
                         rain = mod$rain$rain * to_mmperhour$rain)
  
  # read SWMM temperature (daily Tmin and Tmax)
  mod$TmaxTminDay <- read.table(file.path(subfolder, temperatureFile), 
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
                (hourTmax - hourTmin))
          
        } else {
          
          Ti <- Tmax - (Tmax - Tmin)/2*sin(
            pi*(houri - hourTmax)/(hourTmin + 24 - hourTmax))
        }
      }
      
      return(Ti)
      
    })
  
  return(mod)
}

makeRainfallRunoffEvents <- function(rainfalldata, runoffdata){
  
  # function takes rainfall and runoff in mm/hour, which is what function
  # 'readObservations' generates
  
  runoffdata2 <- approx(
    x=runoffdata$dateTime, 
    y=runoffdata$runoff,
    xout=rainfalldata$dateTime)
  
  runoffdata <- data.frame(
    dateTime = runoffdata2$x,
    runoff = runoffdata2$y)
  
  rainrunoff <- data.frame(
    dateTime = runoffdata$dateTime,
    rainrunoff = rainfalldata$rain + runoffdata$runoff)
  
  rainrunoffevents <- kwb.event::getEvents(
    rainData = rainrunoff, 
    seriesName = "rainrunoff")
  
  filterstorm <- function(data, tBeg, tEnd){
    return(data[data$dateTime >= tBeg &
                  data$dateTime <= tEnd, ])
  }
  
  x <- lapply(X = seq_along(rainrunoffevents$iBeg),
              FUN = function(i){
                
                tBeg <- rainrunoffevents$tBeg[i]
                tEnd <- rainrunoffevents$tEnd[i]
                
                rainsel <- filterstorm(rainfalldata, tBeg, tEnd)
                runoffsel <- filterstorm(runoffdata, tBeg, tEnd)
                
                eventrain <- trapezIntegr(rainsel, 'rain')
                eventrunoff <- trapezIntegr(runoffsel, 'runoff')
                
                return(cbind(rain = eventrain, 
                             runoff = eventrunoff))})
  
  x <- data.frame(do.call(rbind, x))
  
  rainrunoffevents <- cbind(rainrunoffevents, x)
  
  return(rainrunoffevents)
}

trapezIntegr <- function(data, column){
  
  data <- data[!is.na(data[[column]]), ]
  
  AA   <- data[[column]][2:nrow(data)]
  
  aa   <- data[[column]][1:(nrow(data) - 1)]
  
  hh   <- (as.numeric(data$dateTime[2:nrow(data)]) - 
             as.numeric(data$dateTime[1:(nrow(data) - 1)]))/3600
  
  return(sum((AA + aa)/2*hh))
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

monthlyRunoffCoeff <- function(data){
  
  x <- data$rain_runoff[, c('tBeg', 'rain', 'runoff', 'TmaxADWP')]
  x <- na.omit(x)
  x$yearMonth <- as.character(format(x$tBeg, format = '%Y%m'))
  
  x.monthly <- data.frame(
    aggregate(x = x[, c('rain', 'runoff')], 
              by = list(x$yearMonth), 
              FUN = sum),
    meanTmaxADWP = aggregate(x = x[, 'TmaxADWP'],
                             by = list(x$yearMonth),
                             FUN = mean)[, 2])
  
  names(x.monthly)[1] <- 'yearMonth'
  
  x.monthly$runCoeff <- x.monthly$runoff/x.monthly$rain
  
  return(x.monthly)
}









# green roof BWSTI
{
  # grab observed and modeled event data, putting all hydrological flows in l/m2/sec
  {
    setwd("Y:/WWT_Department/Projects/KEYS/Data-Work packages/WP1_sponge_city_elements/LIDmodels/greenRoof/output")
    
    mod <- tbl_df(read.table("mod_beijing_event_2015_1.txt", skip=4, header=FALSE, 
                             colClasses=c("character", "character", "numeric", "numeric"),
                             col.names=c("date", "time", "rainfall", "runoff")))
    
    mod %>%
      mutate(dateTime=as.POSIXct(paste(mod$date, mod$time),
                                 format="%m/%d/%Y %H:%M:%S", 
                                 tz="Etc/GMT+8"),
             hN=rainfall,
             RO=runoff/65) %>%
      select(dateTime, hN, RO) -> mod
    
    setwd("Y:/WWT_Department/Projects/KEYS/Data-Work packages/WP1_sponge_city_elements/LIDmodels/greenRoof/Timeseries/")
    obs <- tbl_df(read.table("beijing_obs_runoff_events_BWSTI.txt", 
                             header=FALSE, skip=1, sep="\t",
                             colClasses=c("character", "numeric"),
                             col.names=c("dateTime", "mm5min")))
    
    obs$lm2s <- obs$mm5min/(5*60)
    obs$dateTime <- as.POSIXct(obs$dateTime, format="%m/%d/%Y %H:%M", tz="Etc/GMT+8")
    
  }
  
  # plot individual events (obs and mod) and compute RMSE
  {
    obsVol <- computeVol(dischargeData=obs, Qcolumn="lm2s", 
                         tBeg=obs$dateTime[1], tEnd=obs$dateTime[nrow(obs)])
    modVol <- computeVol(dischargeData=mod, Qcolumn="RO", 
                         tBeg=mod$dateTime[1], tEnd=mod$dateTime[nrow(mod)])
    obsQmax <- max(obs$lm2s, na.rm=TRUE)
    modQmax <- max(mod$RO, na.rm=TRUE)
    
    c(obsVol=obsVol, modVol=modVol, obsQmax=obsQmax, modQmax=modQmax)
    
    tBeg <- mod$dateTime[1]
    tEnd <- mod$dateTime[nrow(mod)]
    tAx  <- seq(tBeg, tEnd, by=3600)
    par(mar=c(3,5,1,1))
    plot(obs$dateTime, obs$lm2s, xlim=c(tBeg, tEnd), ylim=c(0, 0.005), xaxt="n", las=2, ylab="", 
         type="o", lwd=2)
    lines(mod$dateTime, mod$RO, col="red")
    mtext(side=2, text="L/m2/s", line=3.5, cex=1.5)
    axis(1, at=tAx, labels=format(tAx, format="%H:%M\n%d-%m"), padj=0.3)
    
    
    #   
    # rmseVol  <- sqrt(sum((obsVol - modVol)^2)/nrow(obs))
    # rmsePeak <- sqrt(sum((obsQmax - modQmax)^2)/nrow(obs))
    # 
    # par(mfcol=c(1, 2), mar=c(4,5,1,2))
    # plot(obs$Vol, Qmod$Vol, xlim=c(0, 60), ylim=c(0, 60),
    #      xlab=expression(paste("Obs. Event Volume [ ", L/m^2, "]")),
    #      ylab=expression(paste("Mod. Event Volume [ ", L/m^2, "]")))
    # abline(a=0, b=1)
    # text(x=0, y=60, labels=expression(paste("RMSE = 12.6 ", L/m^2)), adj=0)
    # plot(Qobs$Qmax, Qmod$Qmax, xlim=c(0, 2e-3), ylim=c(0, 2e-3),
    #      xlab=expression(paste("Obs. Event Peak [ ", L/s/m^2, "]")),
    #      ylab=expression(paste("Mod. Event Peak [ ", L/s/m^2, "]")))
    # abline(a=0, b=1)
    # text(x=0, y=0.002, labels=expression(paste("RMSE = 37 ", L/h/m^2)), adj=0)
  }
  
  
  
}




# compare model and measurements
{
  # grab modeled values (converting l/s to l/h)
  {
    setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_DatenAnalyse/swmm_BBR18")
    
    mod <- tbl_df(read.table("grRunoff.txt", skip=4, header=FALSE, 
                             colClasses=c("character", "character", "numeric"),
                             col.names=c("date", "time", "runoff")))
    
    mod %>%
      mutate(dateTime=as.POSIXct(paste(mod$date, mod$time),
                                 format="%m/%d/%Y %H:%M:%S", 
                                 tz="Etc/GMT-1"),
             Q=runoff*3600) %>%
      select(dateTime, Q) -> mod
  }
  
  # grab observed values
  {
    obs <- readRoof(site="BBR")
  }
  
  
  plotRoofEvent(site="BBR", 
                tBeg="2019-01-21 00:00",
                tEnd="2019-06-13 08:00",
                roofData=obs,
                rainData=rainData, 
                rainGauge="BlnX",
                rainScale=50, dt=24*15*3600, Qmax=800)
  lines(mod$dateTime, mod$Q, col="red")
  
}




checkRain(tBeg="2018-09-01 00:00",
          tEnd="2018-09-01 23:59",
          dt=2*3600,
          diN = 1)


tbeg <- as.POSIXct('2015-04-25 00:00:00', format='%Y-%m-%d %H:%M:%S', 
                   tz='Etc/GMT-1')
tend <- as.POSIXct('2015-04-28 00:00:00', format='%Y-%m-%d %H:%M:%S', 
                   tz='Etc/GMT-1')
xax <- seq(tbeg, tend, by=3600*24*1)
plot(runoffdata$dateTime, runoffdata$runoff, xlim=c(tbeg, tend), type='l', 
     ylim=c(0, 20))
axis(1, at = xax, labels = format(xax, format = '%m-%d'), las=2)
lines(rainfalldata$dateTime, rainfalldata$rain, col='blue')
lines(rainrunoff$dateTime, rainrunoff$rainrunoff, col='red')


par(mfcol=c(2, 1), mar=c(3, 3, 1, 1))

# how good is SWMM's temperature prediction?
tbeg <- as.POSIXct('2014-09-12 15:00:00')
tend <- as.POSIXct('2014-09-30 20:00:00')
plot(obs.neubrandenburg$temperature, xlim=c(tbeg, tend))
lines(mod.neubrandenburg$temperature, col = 'red')

x <- obs.neubrandenburg$temperature
y <- mod.neubrandenburg$temperature
index <- match(y$dateTime, x$dateTime)
index <- index[!is.na(index)]
x <- x[index , ]

dim(y); dim(x)

plot(x$temperature , y$temperature)

