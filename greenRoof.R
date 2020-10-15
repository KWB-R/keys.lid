setwd("//Medusa/Projekte$/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/Rscripts")
source("BaSaR-Funktionen.R")

# green roof Neubrandemburg
{
  # make wind data for Hochschule Neubrandemburg using BaSaR data base for Berlin
  {
    windData <- tbl_df(loadWindData())
    
    windData[is.na(windData$dateTime), ]
    
    windData %>%
      mutate(year=year(dateTime),
             month=month(dateTime)) %>%
      group_by(year, month) %>%
      summarize(ws=mean(speed_BBR, na.rm=TRUE)) %>%
      ungroup() %>%
      select(year, month, ws) -> windGR
  
    # convert monthly m/s to km/h  
    windGR$kmh <- windGR$ws*3.6
    windGR <- windGR[order(windGR$month),]
  }
  
  # grab temperature data for green roof at Hochschule Neubrandemburg
  {
    setwd("Y:/WWT_Department/Projects/KEYS/Data-Work packages/WP1_sponge_city_elements/LIDmodels/greenRoof/Timeseries/")
    
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
    
    #write.table(tempGR2, file="TempGR_HSNB.txt", col.names=FALSE, row.names=FALSE,
    #            quote=FALSE)
    
    
  }
  
  # grab and format observed green roof runoff at Hochschule Neubrandemburg (l/s/m2)
  {
    path <- "Y:/SUW_Department/Projects/KURAS/Data-Work packages/AP3_2_Oberflaechenwasser/Monitoring/Gruendach/03_Daten_Ergebnisse/01_Hydraulik/04_KalibrierteDaten/Kalibrierte_Regen_Abflussdaten.csv"
    
    obs <- tbl_df(read.table(path, 
                             header=FALSE, 
                             skip=1, 
                             colClasses=c("character", "character", rep("numeric", times=5)),
                             col.names=c("date", "time", "rain","tipKD", "tipGD", "roKD", "roGD")))
    
    obs %>%
      mutate(dateTime=as.POSIXct(paste(obs$date, obs$time), format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT-1"),
             RO=roGD/(5*60),
             cumRO=cumsum(RO)) %>%
      select(dateTime, RO, cumRO, rain) -> obs
  }

  # rain statistics for Hochschule Neubrandemburg
  {
    rain        <- select(obs, dateTime, rain)
    rain.events <- getEvents(rain, "rain", signalThreshold=0)
    
    for(i in 1:nrow(rain.events))
    {
      rainSel          <- rain[rain$dateTime >= rain.events$tBeg[i] &
                                 rain$dateTime <= rain.events$tEnd[i], ]
      rain.events$hN[i]    <- sum(rainSel$rain, na.rm=TRUE)
      rain.events$iNmax[i] <- max(rainSel$rain, na.rm=TRUE)
    }
    
    max(rain.events$hN, na.rm=TRUE)       # mm
    max(rain.events$dur, na.rm=TRUE)/3600 # hours
    max(rain.events$iNmax, na.rm=TRUE)*12 # mm/hour
    
    filter(rain.events, iNmax == max(rain.events$iNmax, na.rm=TRUE))
    
    
    
    tBeg <- rain.events$tBeg[1]-3600
    tEnd <- rain.events$tEnd[1]+3600
    tAx  <- seq(tBeg, tEnd, by=3600)
    ev <- filter(rain, dateTime >= tBeg & dateTime <= tEnd)
    par(mar=c(7,3,1,1))
    plot(ev$dateTime, ev$rain, xlim=c(tBeg, tEnd), ylim=c(0, 0.5), type="l", xaxt="n", xlab="")
    axis(1, at=tAx, labels=format(tAx, "%d-%b %H:%M"), las=2)
  }
  
  # historical event statistics in Berlin (Dahlem rain series from KURAS)
  {
    setwd("Y:/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/OTHER/historicDahlem/")
    rd <- tbl_df(read.table("raindataWithZeros.csv", 
                            header=TRUE,
                            sep=";",
                            colClasses=c("character", "numeric"),
                            col.names=c("dateTime", "hN5min")))
    
    rainInterp <- approx(x=obs$dateTime, y=obs$rain, 
                         xout=seq(from=obs$dateTime[1], 
                                  to=obs$dateTime[nrow(obs)], 
                                  by=5*60))
    rainInterp <- data.frame(dateTime=rainInterp$x, 
                             hN5min=rainInterp$y)
    
    rainEv <- getEvents(rd, "hN5min", eventSeparationTime = 6*3600)
    
    rainEvi <- data.frame(tBeg=NA, tEnd=NA, hN=NA, iNmax=NA, dur_minutes=NA)
    rainEv2 <- data.frame()
    
    for(i in 1:nrow(rainEv))
    {
      tBeg <- rainEv$tBeg[i]
      tEnd <- rainEv$tEnd[i]
      
      raini <- filter(obs, dateTime >= tBeg & dateTime <= tEnd)
      
      rainEvi$tBeg        <- tBeg
      rainEvi$tEnd        <- tEnd
      rainEvi$hN          <- sum(raini$rain, na.rm=TRUE)
      rainEvi$iNmax       <- max(raini$rain, na.rm=TRUE)
      rainEvi$dur_minutes <- rainEv$dur[i]/60
      
      rainEv2 <- rbind(rainEv2, rainEvi)
    }
    
    # maximum duration, depth and intensity
    hNmax  <- max(rainEv2$hN, na.rm=TRUE)
    durMax <- max(rainEv2$dur_minutes/60)
    iNmax  <- max(rainEv2$iNmax)/5*60
    
    hist(rainEv2$hN, breaks=seq(0, ceiling(hNmax), by=2), xaxt="n")
    axis(1, at=seq(0, ceiling(hNmax), by=2))
    box()
    
    # number of events > 5 mm
    rainEv2 <- filter(rainEv2, hN>5)
    nrow(rainEv2)
    
    # rolling sum with time window tw
    {
      tw <- 10*60 # min
      dt <- 5
      
      xx      <- obs$rain
      rollsum <- numeric()
      for(i in 1:(length(xx) - tw/dt ))
      {
        rollsum[i] <- sum(xx[i:(i+(tw/dt))])
      }
      
      max(rollsum)
    }
  }
  
  
  # grab and format modeled green roof runoff at Hochschule Neubrandemburg (l/s/m2)
  {
    setwd("Y:/WWT_Department/Projects/KEYS/Data-Work packages/WP1_sponge_city_elements/LIDmodels/greenRoof/output")
    
    mod <- tbl_df(read.table("mod_neubrandenburg.txt", skip=4, header=FALSE, 
                             colClasses=c("character", "character", "numeric"),
                             col.names=c("date", "time", "runoff")))
    
    mod %>%
      mutate(dateTime=as.POSIXct(paste(mod$date, mod$time),
                                 format="%m/%d/%Y %H:%M:%S", 
                                 tz="Etc/GMT-1"),
             RO=runoff/(0.0101*10000),
             cumRO=cumsum(RO)) %>%
      select(dateTime, RO, cumRO) -> mod
  }
  
  # make rainfall data frame using rainfall data in obs
  {
    tBeg <- as.POSIXct("2014-09-12 00:00", format="%Y-%m-%d %H:%M", tz="Etc/GMT-1")
    tEnd <- as.POSIXct("2015-12-06 23:59", format="%Y-%m-%d %H:%M", tz="Etc/GMT-1")
    
    obs %>% 
      filter(dateTime >= tBeg & dateTime <= tEnd) %>%
      select(dateTime, rain) %>%
      mutate(year=year(dateTime)) %>%
      mutate(month=month(dateTime)) %>%
      mutate(day=day(dateTime)) %>%
      mutate(hour=hour(dateTime)) %>%
      group_by(year, month, day, hour) %>%
      summarize(dateTime=as.POSIXct(first(dateTime), tz="Etc/GMT-1"),
                hN=sum(rain, na.rm=TRUE),
                hNcum=cumsum(hN)) %>%
      ungroup() %>%
      select(dateTime, hN, hNcum ) -> rain
  }
  
  # plot cumulative time series for obs and mod
  {
    tBeg <- as.POSIXct("2014-09-12 00:00", format="%Y-%m-%d %H:%M", tz="Etc/GMT-1")
    tEnd <- as.POSIXct("2015-12-06 23:59", format="%Y-%m-%d %H:%M", tz="Etc/GMT-1")
    tAx  <- seq(tBeg, tEnd, by="month")
    
    plot(obs$dateTime, obs$cumRO, type="p", pch=20, cex=0.5, 
         xlim=c(tBeg, tEnd-3*86400), ylim=c(0, 3), xaxt="n")
    points(mod$dateTime, mod$cumRO, pch=20, cex=0.5, col="red")
    points(obs$dateTime, cumsum(obs$rain)/(5*60), pch=20, cex=0.5, col="blue")
    axis(1, at=tAx, labels=format(tAx, format="%d-%b\n%Y"), padj=0.5)
  }
  
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
  
  # relationship rainfall vs. discharge
  {
    obs$joint <- obs$RO + obs$rain
    
    Qevents <- getEvents(obs, "joint", signalThreshold = 0)
    
    for(i in 1:nrow(Qevents))
    {
      Qsel <- filter(obs, dateTime >= Qevents$tBeg[i] & dateTime <= Qevents$tEnd[i])
      
      Qevents$hN[i]   <- sum(Qsel$rain, na.rm=TRUE)
      Qevents$Qvol[i] <- computeVol(dischargeData = Qsel, Qcolumn = "RO",
                                 tBeg = Qevents$tBeg[i],
                                 tEnd = Qevents$tEnd[i])
    }
    
    Qevents <- filter(Qevents, !is.na(Qvol))
    Qretention <- Qevents$Qvol/Qevents$hN
    Qretention <- Qretention[Qretention<1 & Qretention>0]
    hist(Qretention, xlim=c(0,1))
    
    
    1-sum(Qevents$Qvol)/sum(Qevents$hN)
    
    plot(Qevents$hN, Qevents$Qvol, xlim=c(0, 60))
    
  }
  
  # plot individual events for obs and mod and add RMSE
  {
    rmseVol  <- sqrt(sum((Qobs$Vol - Qmod$Vol)^2)/nrow(Qobs))
    rmsePeak <- sqrt(sum((Qobs$Qmax - Qmod$Qmax)^2)/nrow(Qobs))
    
    par(mfcol=c(1, 2), mar=c(4,5,1,2))
    plot(Qobs$Vol, Qmod$Vol, xlim=c(0, 60), ylim=c(0, 60),
         xlab=expression(paste("Obs. Event Volume [ ", L/m^2, "]")),
         ylab=expression(paste("Mod. Event Volume [ ", L/m^2, "]")))
    abline(a=0, b=1)
    text(x=0, y=60, labels=expression(paste("RMSE = 4.01 ", L/m^2)), adj=0)
    plot(Qobs$Qmax, Qmod$Qmax, xlim=c(0, 2e-3), ylim=c(0, 2e-3),
         xlab=expression(paste("Obs. Event Peak [ ", L/s/m^2, "]")),
         ylab=expression(paste("Mod. Event Peak [ ", L/s/m^2, "]")))
    abline(a=0, b=1)
    text(x=0, y=0.002, labels=expression(paste("RMSE = 0.59 ", L/h/m^2)), adj=0)
  }
  
  # statistics of observed rainfall and discharge events
  {
    # discharge
    {
      obsEv <- getEvents(obs, "RO")
      obsEv <- filter(obsEv, dur>300)
      
      Qobsi <- data.frame(tBeg=NA, tEnd=NA, Vol=NA, Qmax=NA)
      Qobs  <- data.frame()
      
      for(i in 1:nrow(obsEv))
      {
        tBeg <- obsEv$tBeg[i]
        tEnd <- obsEv$tEnd[i]
        
        obsi <- filter(obs, dateTime >= tBeg & dateTime <= tEnd)
        
        Qobsi$tBeg <- tBeg
        Qobsi$tEnd <- tEnd
        Qobsi$Vol  <- computeVol(dischargeData=obsi, Qcolumn="RO", tBeg=tBeg, tEnd=tEnd)
        Qobsi$Qmax <- max(obsi$RO, na.rm=TRUE)
        
        Qobs <- rbind(Qobs, Qobsi)
      }
      
      max(Qobs$Qmax)*3600
      
    }
    
    # rainfall
    {
      rainEv <- getEvents(obs, "rain")
      
      rainEvi <- data.frame(tBeg=NA, tEnd=NA, hN=NA, iNmax=NA, dur_minutes=NA)
      rainEv2 <- data.frame()
      
      for(i in 1:nrow(rainEv))
      {
        tBeg <- rainEv$tBeg[i]
        tEnd <- rainEv$tEnd[i]
        
        raini <- filter(obs, dateTime >= tBeg & dateTime <= tEnd)
        
        rainEvi$tBeg        <- tBeg
        rainEvi$tEnd        <- tEnd
        rainEvi$hN          <- sum(raini$rain, na.rm=TRUE)
        rainEvi$iNmax       <- max(raini$rain, na.rm=TRUE)
        rainEvi$dur_minutes <- rainEv$dur[i]/60
        
        rainEv2 <- rbind(rainEv2, rainEvi)
      }
      
      rainEv2 <- filter(rainEv2, hN>5)
      
      max(rainEv2$dur_minutes/60)
      
    }
    
    
    
  }
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

