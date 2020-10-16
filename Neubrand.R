library(dplyr)
library(lubridate)

#### 1. KURAS monitoring data
# grab and format rainfall & runoff data at KURAS Neubrandenburg (l/m2/s)
setwd("Y:/WWT_Department/Projects/KEYS/Data-Work packages/WP1_sponge_city_elements/_Data/DataGermany/Neubrand")
obs.neu <- dplyr::tbl_df(read.table("rain_runoff_Neubrandenburg.txt", 
                                    sep=";",  
                                    header=T, 
                                    dec=".", 
                                    colClasses=c("character", "numeric", "numeric"),
                                    col.names = c("dateTime", "rain", "runoff")))

# convert runoff units to (mm/5mins) and select columns            
obs.neu$dateTime <- as.POSIXct(obs.neu$dateTime, 
                               format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT-1")
obs.neu <- obs.neu %>% 
  dplyr::mutate(RO=runoff/(5*60), 
                RO_rain = rain + RO) %>%
  dplyr::select(dateTime, rain, runoff, RO, RO_rain) %>%
  dplyr::filter(dateTime >= "2014-10-01 01:00:00", dateTime <= "2016-01-31 23:59:00")


# make joint rainfall-runoff events
computeVol <- function(dischargeData, Qcolumn, tBeg, tEnd) # volume/sec
{
  tBeg <- as.POSIXct(tBeg, format="%Y-%m-%d %H:%M", tz="Etc/GMT-1")
  tEnd <- as.POSIXct(tEnd, format="%Y-%m-%d %H:%M", tz="Etc/GMT-1")
  
  Qsel <- filter(dischargeData, dateTime >= tBeg & dateTime <= tEnd & !is.na(Qcolumn))
  
  AA   <- pull(Qsel, Qcolumn)[2:(nrow(Qsel))]
  aa   <- pull(Qsel, Qcolumn)[1:(nrow(Qsel)-1)]
  hh   <- as.numeric(Qsel$dateTime[2:(nrow(Qsel))] - 
                       Qsel$dateTime[1:(nrow(Qsel)-1)])*60
  Vtot <- sum((AA + aa)/2*hh)
  
  return(Vtot)
} 

  rrEvents.neu <- kwb.event::getEvents(rainData=obs.neu,
                                   seriesName="RO_rain",
                                   signalThreshold=0)
  
  rrEvents.neu$hN <- NA
  rrEvents.neu$ROvol <- NA
  rrEvents.neu$runoffCoeff <- NA
  rrEvents.neu$intensity <- NA
 
   
  for(i in 1:nrow(rrEvents.neu)){
    sel <- dplyr::filter(obs.neu, dateTime>=rrEvents.neu$tBeg[i], dateTime<=rrEvents.neu$tEnd[i])
    
    rrEvents.neu$hN[i]    <- sum(sel$rain)
    rrEvents.neu$ROvol[i] <- computeVol(dischargeData=sel,
                                    Qcolumn="RO",
                                    tBeg=rrEvents.neu$tBeg[i], 
                                    tEnd=rrEvents.neu$tEnd[i])
    rrEvents.neu$runoffCoeff[i] <- rrEvents.neu$ROvol[i]/rrEvents.neu$hN[i]
    rrEvents.neu$intensity[i] <- rrEvents.neu$hN[i]/rrEvents.neu$dur[i]*3600
  }
  
# remove unusable events (too short, NAs, runoff coeff >1)  
  rrEvents.neu <- rrEvents.neu[rrEvents.neu$dur>300, ]
  rrEvents.neu <- stats::na.omit(rrEvents.neu)
  rrEvents.neu <- rrEvents.neu[rrEvents.neu$runoffCoeff<=1, ]
  

# read temperature data [C]
temp.neu <-  dplyr::tbl_df(read.table("temp_Neubrand.txt",
                                     header=T,
                                     sep=";", dec = ".",
                                     colClasses=c("character", "numeric", "numeric"),
                                     col.names = c("dateTime", "Temp", "Tdew"))) 
temp.neu$dateTime <- as.POSIXct(temp.neu$dateTime, 
                               format="%Y-%m-%d %H:%M:%S",
                               tz="Etc/GMT-1")


# read solar radiation data [MJ/m2]
  rad.neu <-  dplyr::tbl_df(read.table("rad_Neubrand.txt",
                                    header=T,
                                    sep=";",
                                    colClasses=c("character","numeric"))) 
  rad.neu$dateTime <- as.POSIXct(rad.neu$dateTime, 
                              format="%Y-%m-%d %H:%M:%S",
                              tz="Etc/GMT-1")

  
# read wind speed data [m/s]
   wind.neu <-  dplyr::tbl_df(read.table("wind_Neubrand.txt",
                                        header=T,
                                        sep=";",
                                        colClasses=c("character","numeric"))) 
   wind.neu$dateTime <- as.POSIXct(temp.neu$dateTime, 
                                  format="%Y-%m-%d %H:%M:%S",
                                  tz="Etc/GMT-1")
   
   
# combine weather parameters 
  weather.neu <- temp.neu %>% mutate(rad.neu$Rad, wind.neu$Wind) %>% 
    rename(Rad = 'rad.neu$Rad', Wind = 'wind.neu$Wind') 

  
# find time period between end of previous rain event (tEndPrev) and beginning of current event (tBeg) 
  
  for(i in 1:nrow(rrEvents.neu)){
    tBeg       <- rrEvents.neu$tBeg[i]
    rainBefore <- dplyr::filter(obs.neu, dateTime < tBeg)
    indexRain  <- which(rainBefore[[2]]>0) 
    rainBefore <- rainBefore[indexRain, ] 
    tEndPrev   <- max(rainBefore$dateTime)
    
  # select weather data for time period between tEndPrev and tBeg
    Wsel <- dplyr::filter(weather.neu, dateTime >= tEndPrev & dateTime <= tBeg) 
    
    rrEvents.neu$Tmax[i] <- max(Wsel$Temp, na.rm=T) 
    rrEvents.neu$Tmin[i] <- min(Wsel$Temp, na.rm=T)
    rrEvents.neu$Tmean[i] <- mean(Wsel$Temp, na.rm=T)
    rrEvents.neu$Tdew[i] <- mean(Wsel$Tdew, na.rm=T)
    rrEvents.neu$Wind[i] <- mean(Wsel$Wind, na.rm=T)  
    rrEvents.neu$Rs[i] <- sum(Wsel$Rad)
}
  
# Calculate Evapotranspiration (FAO Penman-Monteith equation)
  # Slope vapor pressure curve [kPa/C]
  rrEvents.neu$D = 4098*{0.6108*exp(17.27*rrEvents.neu$Tmean/(rrEvents.neu$Tmean+237.3))}/(rrEvents.neu$Tmean+237.3)^2
  # es: Saturation vapor pressure [kPa]
  rrEvents.neu$es = {0.6108*exp(17.27*rrEvents.neu$Tmax/(rrEvents.neu$Tmax+237.3)) + 0.6108*exp(17.27*rrEvents.neu$Tmin/(rrEvents.neu$Tmin+237.3))}/2
  # ea: Actual vapor pressure [kPa]
  rrEvents.neu$ea = 0.6108*exp(17.27*rrEvents.neu$Tdew/(rrEvents.neu$Tdew+237.3))
  # Rnl: Net longwave radaiation (Stefan-Boltzmann constant = 4.903e-09 MJ/K4/m2, Rs/Rso = 0.7 for normal clear day)
  rrEvents.neu$Rnl=4.903e-09*{(rrEvents.neu$Tmax+273.15)^4 + (rrEvents.neu$Tmin)^4}/2*(0.34-0.14*sqrt(rrEvents.neu$ea))*(1.35*0.7-0.35)
  # Rn: Net radiation = Net shortwave(Rs*(1-0.23)) - Net longwave(Rnl) radiation, albedo 0.23 = green grass reference crop
  rrEvents.neu$Rn=rrEvents.neu$Rs*0.77-rrEvents.neu$Rnl 
  # ET: Reference Evapotranspiration [mm]
  # g = 0.067 kPa/C (Pshchrometric constant at standard atmospheric pressure 101.3 kPa)
  rrEvents.neu$ET = {0.408*rrEvents.neu$D*(rrEvents.neu$Rn) + 0.067*900/(rrEvents.neu$Tmean+273)*rrEvents.neu$Wind*(rrEvents.neu$es-rrEvents.neu$ea)}/{rrEvents.neu$D + 0.067*(1+0.34*rrEvents.neu$Wind)}

  rrEvents.neu <- dplyr::select(rrEvents.neu, tBeg, tEnd, dur, pBefore, hN, ROvol, runoffCoeff, intensity, ET, Rs, Tmax, Tmin, Wind)
  
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
             
  
  # Linear Regression assessment 
  mod <- lm(data=obs.events, formula= runoffCoeff ~ hN + Tmax)
  summary(mod)
  
  # plot using hN in x-axis and runoffCoeff in y-axis
  plot(obs.events$hN, obs.events$runoffCoeff)
  pred <- predict(mod)
  points(obs.events$hN, pred, col="red", pch=20)
  
  
  plot(obs.events$runoffCoeff, pred, xlab=expression("Observed"),
       ylab=expression("Predicted"), xlim = c(0,0.8),ylim = c(0,0.8))
  abline(a=0, b=1)
  


   #### 2. SWMM modelling runoff data (rain data input: 5 min interval)
  
  swmm.rain <- dplyr::tbl_df(read.table("mod_neubrand_rain_5min.txt",
                                        sep="", header = F,
                                        dec=".",
                                        quote="\"",
                                        colClasses= c("character","character","character","character","character","character","numeric"),
                                        col.names = c("station","year","month","day","hour","minute","rain")))
  
  swmm.rain <- swmm.rain %>% 
    dplyr::mutate(date=paste(year, month, day, sep = '-'),
                  time=paste(hour,minute,"00", sep = ':'),
                  dateTime=as.POSIXct(paste(date, time), 
                                      format="%Y-%m-%d %H:%M:%S", 
                                      tz="Etc/GMT-1")) %>%
    dplyr::select(dateTime, rain) %>%
    dplyr::filter(dateTime >= "2014-10-01 01:00:00", dateTime <= "2015-11-30 23:55:00")
  
  
  swmm.runoff <- dplyr::tbl_df(read.table("mod_neubrand_runoff_5min.txt",
                                          sep="", header = T,
                                          dec=".",
                                          quote="\"",
                                          colClasses=c("character", "character","numeric"),
                                          col.names = c("date","time","runoff")))
  
  swmm.runoff <- swmm.runoff %>% 
    dplyr::mutate(dateTime=as.POSIXct(paste(date, time), 
                                      format="%m-%d-%Y %H:%M:%S", 
                                      tz="Etc/GMT-1"),
                  RO=runoff/101) %>%  
    dplyr::select(dateTime,RO,runoff) %>%
    dplyr::filter(dateTime >= "2014-10-01 01:00:00", dateTime <= "2015-11-30 23:55:00")
  

  # combine SWMM rain and runoff data
  swmm.neu <- swmm.rain %>% mutate(swmm.runoff$RO) %>% 
    rename(RO = 'swmm.runoff$RO') %>%
    mutate(RO_rain = rain + RO) %>%
    select(dateTime, rain, RO, RO_rain)
  
  
  computeVol <- function(dischargeData, Qcolumn, tBeg, tEnd) # volume/sec
  {
    tBeg <- as.POSIXct(tBeg, format="%Y-%m-%d %H:%M", tz="Etc/GMT-1")
    tEnd <- as.POSIXct(tEnd, format="%Y-%m-%d %H:%M", tz="Etc/GMT-1")
    
    Qsel <- filter(dischargeData, dateTime >= tBeg & dateTime <= tEnd & !is.na(Qcolumn))
    
    AA   <- pull(Qsel, Qcolumn)[2:(nrow(Qsel))]
    aa   <- pull(Qsel, Qcolumn)[1:(nrow(Qsel)-1)]
    hh   <- as.numeric(Qsel$dateTime[2:(nrow(Qsel))] - 
                         Qsel$dateTime[1:(nrow(Qsel)-1)])*60
    Vtot <- sum((AA + aa)/2*hh)
    
    return(Vtot)
  } 
  
  rrEvents.swmm <- kwb.event::getEvents(rainData=swmm.neu,
                                        seriesName="RO_rain",
                                        signalThreshold=0)
  
  rrEvents.swmm$hN <- NA
  rrEvents.swmm$ROvol <- NA
  rrEvents.swmm$runoffCoeff <- NA

  
  for(i in 1:nrow(rrEvents.swmm)){
    sel <- dplyr::filter(swmm.neu, dateTime>=rrEvents.swmm$tBeg[i], dateTime<=rrEvents.swmm$tEnd[i])
    
    rrEvents.swmm$hN[i]    <- sum(sel$rain)
    rrEvents.swmm$ROvol[i] <- computeVol(dischargeData=sel,
                                         Qcolumn="RO",
                                         tBeg=rrEvents.swmm$tBeg[i], 
                                         tEnd=rrEvents.swmm$tEnd[i])
    rrEvents.swmm$runoffCoeff[i] <- rrEvents.swmm$ROvol[i]/rrEvents.swmm$hN[i]
    
  }
  
  # remove unusable events (too short, NAs, runoff coeff >1)  
  rrEvents.swmm <- rrEvents.swmm[rrEvents.swmm$dur>600, ]
  rrEvents.swmm <- stats::na.omit(rrEvents.swmm)
  rrEvents.swmm <- rrEvents.swmm[rrEvents.swmm$runoffCoeff<=1, ]
  
  
  # read temperature data [C]
  temp.neu <-  dplyr::tbl_df(read.table("temp_Neubrand.txt",
                                        header=T,
                                        sep=";", dec = ".",
                                        colClasses=c("character", "numeric", "numeric"),
                                        col.names = c("dateTime", "Temp", "Tdew"))) 
  temp.neu$dateTime <- as.POSIXct(temp.neu$dateTime, 
                                  format="%Y-%m-%d %H:%M:%S",
                                  tz="Etc/GMT-1")
  
  
  
  
  # read solar radiation data [MJ/m2]
  rad.neu <-  dplyr::tbl_df(read.table("rad_Neubrand.txt",
                                       header=T,
                                       sep=";",
                                       colClasses=c("character","numeric"))) 
  rad.neu$dateTime <- as.POSIXct(rad.neu$dateTime, 
                                 format="%Y-%m-%d %H:%M:%S",
                                 tz="Etc/GMT-1")
  
  
  
  # read wind speed data [m/s]
  wind.neu <-  dplyr::tbl_df(read.table("wind_Neubrand.txt",
                                        header=T,
                                        sep=";",
                                        colClasses=c("character","numeric"))) 
  wind.neu$dateTime <- as.POSIXct(wind.neu$dateTime, 
                                  format="%Y-%m-%d %H:%M:%S",
                                  tz="Etc/GMT-1")
  
  
  
  # combine weather parameters 
  weather.neu <- temp.neu %>% mutate(rad.neu$Rad, wind.neu$Wind) %>% 
    rename(Rad = 'rad.neu$Rad', Wind = 'wind.neu$Wind') 
  
  # find time period between end of previous rain event (tEndPrev) and beginning of current event (tBeg) 
  
  for(i in 1:nrow(rrEvents.swmm)){
    tBeg       <- rrEvents.swmm$tBeg[i]
    rainBefore <- dplyr::filter(swmm.neu, dateTime < tBeg)
    indexRain  <- which(rainBefore[[2]]>0) 
    rainBefore <- rainBefore[indexRain, ] 
    tEndPrev   <- max(rainBefore$dateTime)
    
    # select weather data for time period between tEndPrev and tBeg
    Wsel <- dplyr::filter(weather.neu, dateTime >= tEndPrev & dateTime <= tBeg) 
    
    
    rrEvents.swmm$Tmax[i] <- max(Wsel$Temp, na.rm=T) 
    rrEvents.swmm$Tmin[i] <- min(Wsel$Temp, na.rm=T)
    rrEvents.swmm$Tmean[i] <- mean(Wsel$Temp, na.rm=T)
    rrEvents.swmm$Tdew[i] <- mean(Wsel$Tdew, na.rm=T)
    rrEvents.swmm$Wind[i] <- mean(Wsel$Wind, na.rm=T)  
    rrEvents.swmm$Rs[i] <- sum(Wsel$Rad)
  }
  

  # Calculate Evapotranspiration (FAO Penman-Monteith equation)
  # Slope vapor pressure curve [kPa/C]
  rrEvents.swmm$D = 4098*{0.6108*exp(17.27*rrEvents.swmm$Tmean/(rrEvents.swmm$Tmean+237.3))}/(rrEvents.swmm$Tmean+237.3)^2
  # es: Saturation vapor pressure [kPa]
  rrEvents.swmm$es = {0.6108*exp(17.27*rrEvents.swmm$Tmax/(rrEvents.swmm$Tmax+237.3)) + 0.6108*exp(17.27*rrEvents.swmm$Tmin/(rrEvents.swmm$Tmin+237.3))}/2
  # ea: Actual vapor pressure [kPa]
  rrEvents.swmm$ea = 0.6108*exp(17.27*rrEvents.swmm$Tdew/(rrEvents.swmm$Tdew+237.3))
  # Rnl: Net longwave radaiation (Stefan-Boltzmann constant = 4.903e-09 MJ/K4/m2, Rs/Rso = 0.7 for normal clear day)
  rrEvents.swmm$Rnl=4.903e-09*{(rrEvents.swmm$Tmax+273.15)^4 + (rrEvents.swmm$Tmin)^4}/2*(0.34-0.14*sqrt(rrEvents.swmm$ea))*(1.35*0.7-0.35)
  # Rn: Net radiation = Net shortwave(Rs*(1-0.23)) - Net longwave(Rnl) radiation, albedo 0.23 = green grass reference crop
  rrEvents.swmm$Rn=rrEvents.swmm$Rs*0.77-rrEvents.swmm$Rnl 
  # ETo: Reference Evapotranspiration [mm]
  # g = 0.067 kPa/C (Pshchrometric constant at standard atmospheric pressure 101.3 kPa)
  rrEvents.swmm$ET = {0.408*rrEvents.swmm$D*(rrEvents.swmm$Rn) + 0.067*900/(rrEvents.swmm$Tmean+273)*rrEvents.swmm$Wind*(rrEvents.swmm$es-rrEvents.swmm$ea)}/{rrEvents.swmm$D + 0.067*(1+0.34*rrEvents.swmm$Wind)}
  
  rrEvents.swmm <- dplyr::select(rrEvents.swmm, tBeg, tEnd, dur, pBefore, hN, ROvol, runoffCoeff, ET, Rs, Tmax, Tmin, Wind)
  
  
  # summarize rrEvents.swmm to  Monthly aggregation
  swmm.events <- rrEvents.swmm %>% 
    mutate(day=day(tBeg),
           week=week(tBeg),
           month=month(tBeg),
           year=year(tBeg),
           id=as.Date(paste(year, month, 1, sep = '-'), '%Y-%m-%d'),
           idPlot=paste(month,year, sep="-")) %>%
    group_by(id) %>%  
    summarise(dur=sum(dur/3600), 
              adwp = sum(pBefore/86400),
              hN=sum(hN),
              RO=sum(ROvol), 
              runoffCoeff = RO/hN,
              Rs=sum(Rs),
              ET=sum(ET),
              Tmax = max(Tmax), 
              Tmin = min(Tmin),
              Wind = mean(Wind),
              n_events=n(),
              idPlot=first(idPlot))
  
  mod <- lm(data=swmm.events, formula= runoffCoeff ~ hN + Tmax)
  summary(mod)

  
  