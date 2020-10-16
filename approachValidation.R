# original units runoff:
# kuras (Neubrandenburg): rain and runoff mm/5min, area = 101 m2
# basar (Berlin): rain mm/5min, runoff  l/h, area = 194 m2

obs.neubrandenburg <- readObservations(
  subfolder = 'data_green_roof',
  rainFile = 'obs_rain_5min_Neubrandenburg.txt',
  runoffFile = 'obs_runoff_5min_Neubrandenburg.txt',
  temperatureFile = 'obs_temp_10min_Neubrandenburg.txt',
  dateTimetz = 'Etc/GMT-1', 
  dateTimeformat = '%Y-%m-%d %H:%M:%S',
  to_mmperhour = list(rain=1/(5/60), runoff=1/(5/60)))

obs.berlin <- readObservations(
  subfolder = 'data_green_roof',
  rainFile = 'obs_rain_5min_Berlin.txt',
  runoffFile = 'obs_runoff_5min_Berlin.txt',
  temperatureFile = 'obs_temp_10min_Berlin.txt',
  dateTimetz = 'Etc/GMT-1', 
  dateTimeformat = '%Y-%m-%d %H:%M:%S',
  to_mmperhour = list(rain=1, runoff=1/194))

mod.neubrandenburg <- readPredictedRunoff(
  subfolder = 'models_green_roof',
  to_mmperhour =  3600/101)


readObservations <- function(subfolder, rainFile, runoffFile, temperatureFile,
                             dateTimetz, dateTimeformat,
                             to_mmperhour){
  
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
  
  return(x)
}


readPredictedRunoff <- function(subfolder, to_mmperhour){
  
  mod <- read.table(file.path(subfolder, 'mod_runoff_5min_Neubrandenburg.txt'), 
                    skip=4, 
                    header=FALSE, 
                    colClasses=c("character", "character", "numeric"),
                    col.names=c("date", "time", "runoff"))
  
  mod$dateTime <- as.POSIXct(paste(mod$date, mod$time),
                             format = '%m/%d/%Y %H:%M:%S', 
                             tz ='Etc/GMT-1')
  
  mod <- mod[, c('dateTime', 'runoff')]
  
  mod$runoff <- mod$runoff * to_mmperhour
  
  return(mod)
}



tbeg <- as.POSIXct('2015-06-01 00:00:00', format='%Y-%m-%d %H:%M:%S', tz='Etc/GMT-1')
tend <- as.POSIXct('2015-07-30 00:00:00', format='%Y-%m-%d %H:%M:%S', tz='Etc/GMT-1')
plot(x$rain$dateTime, x$rain$rain, type='l', xlim=c(tbeg, tend))
lines(x$runoff$dateTime, x$runoff$runoff, col='blue')


# montly patterns in reality vs. monthly patterns in SWMM

# green roof berlin, neubrandenburg, beijing





# grab weather data
tempGR <- tbl_df(read.table("neubrand_temp_daily_2014-2016.txt", header=FALSE,
                            colClasses=c(rep("character", times=4),
                                         rep("numeric", times=2)),
                            col.names=c("station", "year", "month", "day", "Tmax", "Tmin")))

tempGR$dateTime <- as.POSIXct(paste(tempGR$year, tempGR$month, tempGR$day),
                              tz="Etc/GMT-1",
                              format="%Y %m %d")

t1  <- as.POSIXct("2014-09-01 00:00:00", tz="Etc/GMT-1")
t2  <- as.POSIXct("2016-02-01 00:00:00", tz="Etc/GMT-1")
tAx <- seq(from=t1, to=t2, by="month")

plot(tempGR$dateTime, tempGR$Tmax, type="l", ylim=c(-20, 40), xaxt="n")
lines(tempGR$dateTime, tempGR$Tmin, type="l")
axis(1, at=tAx, labels=format(tAx, format="%d-%m\n%Y"), padj=0.1, cex.axis=0.65)
abline(h=0)
abline(v=as.POSIXct(c("2014-12-08 00:00:00",
                      "2015-11-15 00:00:00",
                      "2015-01-07 00:00:00"), 
                    tz="Etc/GMT-1"))

tempGR %>%
  select(station, dateTime, Tmax, Tmin) %>%
  mutate(dateTime=format(dateTime, format="%Y %m %d")) -> tempGR2



# find common events obs and mod and make event volumes and peaks
{
  obs2         <- approx(x=obs$dateTime, y=obs$RO, xout=mod$dateTime)
  obsmod       <- data.frame(dateTime=obs2$x,
                             obsmod=obs2$y + mod$RO)
  eventsObsMod <- getEvents(rainData=obsmod, seriesName="obsmod")
  eventsObsMod <- filter(eventsObsMod, dur>60*10) # exclude events < 10 min. duration
  
  Qobsi <- data.frame(tBeg=NA, tEnd=NA, Vol=NA, Qmax=NA)
  Qmodi <- data.frame(tBeg=NA, tEnd=NA, Vol=NA, Qmax=NA)
  Qobs  <- data.frame()
  Qmod  <- data.frame()
  
  for(i in 1:nrow(eventsObsMod))
  {
    tBeg <- eventsObsMod$tBeg[i]
    tEnd <- eventsObsMod$tEnd[i]
    
    obsi <- filter(obs, dateTime >= tBeg & dateTime <= tEnd)
    modi <- filter(mod, dateTime >= tBeg & dateTime <= tEnd)
    
    Qobsi$tBeg <- tBeg
    Qobsi$tEnd <- tEnd
    Qobsi$Vol  <- computeVol(dischargeData=obsi, Qcolumn="RO", tBeg=tBeg, tEnd=tEnd)
    Qobsi$Qmax <- max(obsi$RO, na.rm=TRUE)
    
    Qmodi$tBeg <- tBeg
    Qmodi$tEnd <- tEnd
    Qmodi$Vol  <- computeVol(dischargeData=modi, Qcolumn="RO", tBeg=tBeg, tEnd=tEnd)
    Qmodi$Qmax <- max(modi$RO, na.rm=TRUE)
    
    Qmod <- rbind(Qmod, Qmodi)
    Qobs <- rbind(Qobs, Qobsi)
  }
  
  Qobs$tBeg <- as.POSIXct(Qobs$tBeg, origin="1970-01-01 00:00:00", tz="Etc/GMT-1")
  Qobs$tEnd <- as.POSIXct(Qobs$tEnd, origin="1970-01-01 00:00:00", tz="Etc/GMT-1")
  
  Qmod$tBeg <- as.POSIXct(Qmod$tBeg, origin="1970-01-01 00:00:00", tz="Etc/GMT-1")
  Qmod$tEnd <- as.POSIXct(Qmod$tEnd, origin="1970-01-01 00:00:00", tz="Etc/GMT-1")
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

