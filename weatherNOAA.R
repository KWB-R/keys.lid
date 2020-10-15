library(rnoaa)
library(dplyr)
library(lubridate)

# get API key from NCDC (U.S. National Climate Data Center): https://www.ncdc.noaa.gov/cdo-web/token

# enter key
options(noaakey = "UXDSLkshBhXrXjBrtnKIATTTqoRIIEjI")

# find stations in 30 km radius around beijing and shenzhen
nearShenzhen <- isd_stations_search(lat=22.664760, lon=114.001761, radius=30,
                                    bbox = NULL)
nearBeijing <- isd_stations_search(lat=39.896404, lon=116.678764, radius=30,
                                   bbox = NULL)

# write these tables to disk
#write.table(nearShenzhen, file="wstationsSh.txt", quote=FALSE, sep=";",
#            col.names=TRUE, row.names=FALSE, )

#write.table(nearBeijing, file="wstationsBe.txt", quote=FALSE, sep=";",
#            col.names=TRUE, row.names=FALSE, )

# Beijing stations:
# BEI-JING/TONG-XIAN    545130  99999 1980 07 03 - 2001 07 25
# PEKING                999999  44101 1946 04 30 - 1946 06 30 
# BEIJING - CAPITAL INT 545110  99999 1945 10 31 - 2019 06 18
# BEI-JING/NAN-YUAN     545120  99999 1980 07 01 - 2002 02 09

# Shenzhen stations:
# BAOAN INTL            594930  99999 1957 06 01 - 2019 06 18
# TA KWU LING           450320  99999 1992 12 04 - 2019 06 18
# LAU FAU SHAN          450350  99999 2004 07 13 - 2019 06 18
# TAI MO SHAN           450090  99999 1947 12 31 - 1956 03 31

# get NOAA data
{
  city <- "Beijing"
  usaf <- "545110"
  wban <- "99999"
  for(i in 1980:2019)
  {
    downi <- isd(usaf=usaf, 
                 wban=wban,
                 year=i, 
                 overwrite=TRUE, 
                 cleanup=TRUE,
                 additional=TRUE, 
                 parallel=FALSE, 
                 cores=getOption("cl.cores", 2), 
                 progress=TRUE, 
                 force=TRUE)
    
    NA -> downi$AA1_depth[downi$AA1_depth=="9999"]
    NA -> downi$AA1_period_quantity_hrs[downi$AA1_period_quantity_hrs=="99"]
    NA -> downi$temperature[downi$temperature=="+9999"]
    NA -> downi$wind_speed[downi$wind_speed=="9999"]
    
    downi %>%
      select(usaf_station,
             date,
             time,
             wind_speed,
             temperature,
             AA1_period_quantity_hrs,
             AA1_depth) %>%
      mutate(dateTime=as.POSIXct(paste(downi$date, downi$time), 
                                 format="%Y%m%d %H%M", 
                                 tz="Etc/GMT+8"),
             AA1_period_quantity_hrs=as.numeric(AA1_period_quantity_hrs),
             hN_mm_hr=as.numeric(AA1_depth)/10/AA1_period_quantity_hrs,
             TdegC=as.numeric(temperature)/10,
             windSpeed_m_s=as.numeric(wind_speed)/10) -> downi
    
    write.table(downi, file=paste0(city, "_", usaf, "_", i, ".txt"), 
                quote=FALSE, row.names=FALSE, sep=";")
    
    assign(paste0(city, "_", usaf, "_", i), downi, envir=.GlobalEnv)
  }
}

# make single data frame per city
{
  # Beijing
  {
    setwd("Y:/WWT_Department/Projects/KEYS/Data-Work packages/WP1_sponge_city_elements/internet_met_data/noaaBeijing")

    files <- list.files()
    
    noaaBeijing  <- data.frame()
    
    for(i in 1:length(files))
    {
      filei <- files[i]
      yeari <- tbl_df(read.table(filei, header=TRUE, sep=";",
                                 colClasses=c(rep("character", times=3),
                                              rep("numeric", times=4),
                                              "character",
                                              rep("numeric", times=3))))
      yeari %>%
        select(usaf_station, 
               dateTime, 
               hN_mm_hr, 
               TdegC, 
               windSpeed_m_s) -> noaaBeijingi
      
      noaaBeijing <- rbind(noaaBeijing, noaaBeijingi)
      
    }
    
    noaaBeijing$dateTime <- as.POSIXct(noaaBeijing$dateTime,
                                       format="%Y-%m-%d %H:%M:%S",
                                       tz="Etc/GMT+8")
    
    # write to disk
    #write.table(noaaBeijing, file="noaaBeijing.txt", quote=FALSE, sep=";", row.names=FALSE)
  }
  
  # Shenzhen
  {
    setwd("Y:/WWT_Department/Projects/KEYS/Data-Work packages/WP1_sponge_city_elements/internet_met_data/noaaShenzhen")
    
    files <- list.files()
    
    noaaShenzhen  <- data.frame()
    
    for(i in 1:length(files))
    {
      filei <- files[i]
      yeari <- tbl_df(read.table(filei, header=TRUE, sep=";",
                                 colClasses=c(rep("character", times=3),
                                              rep("numeric", times=4),
                                              "character",
                                              rep("numeric", times=3))))
      yeari %>%
        select(usaf_station, 
               dateTime, 
               hN_mm_hr, 
               TdegC, 
               windSpeed_m_s) -> noaaShenzheni
      
      noaaShenzhen <- rbind(noaaShenzhen, noaaShenzheni)
    }
    
    noaaShenzhen$dateTime <- as.POSIXct(noaaShenzhen$dateTime,
                                       format="%Y-%m-%d %H:%M:%S",
                                       tz="Etc/GMT+8")
    
    # write to disk
    write.table(noaaShenzhen, file="noaaShenzhen.txt", quote=FALSE, sep=";", row.names=FALSE)
  }
}

# make daily temperature time series for SWMM
{
  noaaBeijing %>%
    mutate(year=year(dateTime)) %>%
    mutate(month=month(dateTime)) %>%
    mutate(day=day(dateTime)) %>%
    mutate(station=usaf_station) %>%
    group_by(year, month, day, station) %>%
    summarize(date=format(as.Date(first(dateTime), format="%Y-%m-%d %H:%M:%S"),
                          format="%Y %m %d"),
              Tmax=max(TdegC, na.rm=TRUE),
              Tmin=min(TdegC, na.rm=TRUE)) %>%
    ungroup() %>%
    select(station, date, Tmax, Tmin) -> tempBeij
    
  noaaShenzhen %>%
    mutate(year=year(dateTime)) %>%
    mutate(month=month(dateTime)) %>%
    mutate(day=day(dateTime)) %>%
    mutate(station=usaf_station) %>%
    group_by(year, month, day, station) %>%
    summarize(date=format(as.Date(first(dateTime), format="%Y-%m-%d %H:%M:%S"),
                          format="%Y %m %d"),
              Tmax=max(TdegC, na.rm=TRUE),
              Tmin=min(TdegC, na.rm=TRUE)) %>%
    ungroup() %>%
    select(station, date, Tmax, Tmin) -> tempShen
  
  setwd("Y:/WWT_Department/Projects/KEYS/Data-Work packages/WP1_sponge_city_elements/LIDmodels/grBerlin/Timeseries/")
  #write.table(tempBeij, file="tempBeij.txt", col.names=FALSE, row.names=FALSE,
  #            quote=FALSE, sep=" ")
  #write.table(tempShen, file="tempShen.txt", col.names=FALSE, row.names=FALSE,
  #            quote=FALSE, sep=" ")
}

# look at rainfall data from https://clim-engine-development.appspot.com/fewsNet
{
  # grab and format data
  {
    setwd("Y:/WWT_Department/Projects/KEYS/Data-Work packages/WP1_sponge_city_elements/internet_met_data/")
    beij <- tbl_df(read.table("beijingClimEng.txt", skip=6,
                              header=FALSE, colClasses=c("character", "numeric"),
                              col.names=c("date", "hN")))
    shen <- tbl_df(read.table("shenzhenClimEng.txt", skip=6,
                              header=FALSE, colClasses=c("character", "numeric"),
                              col.names=c("date", "hN")))
    
    beij$date <- as.Date(beij$date, format="%Y-%m-%d")
    shen$date <- as.Date(shen$date, format="%Y-%m-%d")
  }
  
  # make yearly sums and maxima
  {
    beij %>%
      mutate(year=year(date)) %>%
      group_by(year) %>%
      summarize(hNyear=sum(hN, na.rm=TRUE),
                iNmax=max(hN, na.rm=TRUE)) %>%
      ungroup() %>%
      select(year, hNyear, iNmax) -> yearlyBeijing
    
    shen %>%
      mutate(year=year(date)) %>%
      group_by(year) %>%
      summarize(hNyear=sum(hN, na.rm=TRUE),
                iNmax=max(hN, na.rm=TRUE)) %>%
      ungroup() %>%
      select(year, hNyear, iNmax) -> yearlyShenzhen
    
    yearlyBeijing  <- filter(yearlyBeijing, year < 2019)
    yearlyShenzhen <- filter(yearlyShenzhen, year < 2019)
    
  }
  
  # make statistics
  {
    # prob. distr. of total annual rainfall
    {
      xx1ann <- sort(yearlyBeijing$hNyear)
      Fx1ann <- (1:length(xx1))/(1 + length(xx1))
      
      xx2 <- sort(yearlyShenzhen$hNyear)
      Fx2 <- (1:length(xx2))/(1 + length(xx2))
    }
    
    # prob. distr. of annual maximum daily rainfall
    {
      xx1 <- sort(yearlyBeijing$iNmax)
      Fx1 <- (1:length(xx1))/(1 + length(xx1))
      
      xx2 <- sort(yearlyShenzhen$iNmax)
      Fx2 <- (1:length(xx2))/(1 + length(xx2))
    }
    
    # plots
    {
      plot(xx1, Fx1, xlim=c(0, 500), pch=20)
      points(xx2, Fx2, col="red", pch=20)
      
    }
  }
}

# look at BWSTI event-based precipitation and runoff data
{
  setwd("Y:/WWT_Department/Projects/KEYS/Data-Work packages/WP1_sponge_city_elements/LIDmodels/grBerlin/Timeseries/")

  # read and format data
  {
    # 2014
    rbwsti2014 <- tbl_df(read.table("rainBWSTI_2014.txt",
                                    header=FALSE,
                                    colClasses=c(rep("character", times=6),
                                                 "numeric"),
                                    col.names=c("station", "year", "month",
                                                "day", "hours", "minutes",
                                                "hN")))
    
    rbwsti2014$dateTime <- paste0(rbwsti2014$year, "-",
                                  rbwsti2014$month, "-",
                                  rbwsti2014$day, " ",
                                  rbwsti2014$hours, ":",
                                  rbwsti2014$minutes)
    
    rbwsti2014$dateTime <- as.POSIXct(rbwsti2014$dateTime,
                                      format="%Y-%m-%d %H:%M",
                                      tz="Etc/GMT+8")
    
    # 2015
    rbwsti2015 <- tbl_df(read.table("rainBWSTI_2015.txt",
                                    header=FALSE,
                                    colClasses=c(rep("character", times=6),
                                                 "numeric"),
                                    col.names=c("station", "year", "month",
                                                "day", "hours", "minutes",
                                                "hN")))
    
    rbwsti2015$dateTime <- paste0(rbwsti2015$year, "-",
                                  rbwsti2015$month, "-",
                                  rbwsti2015$day, " ",
                                  rbwsti2015$hours, ":",
                                  rbwsti2015$minutes)
    
    rbwsti2015$dateTime <- as.POSIXct(rbwsti2015$dateTime,
                                      format="%Y-%m-%d %H:%M",
                                      tz="Etc/GMT+8")
    
    # 2016
    rbwsti2016 <- tbl_df(read.table("rainBWSTI_2016.txt",
                                    header=FALSE,
                                    colClasses=c(rep("character", times=6),
                                                 "numeric"),
                                    col.names=c("station", "year", "month",
                                                "day", "hours", "minutes",
                                                "hN")))
    
    rbwsti2016$dateTime <- paste0(rbwsti2016$year, "-",
                                  rbwsti2016$month, "-",
                                  rbwsti2016$day, " ",
                                  rbwsti2016$hours, ":",
                                  rbwsti2016$minutes)
    
    rbwsti2016$dateTime <- as.POSIXct(rbwsti2016$dateTime,
                                      format="%Y-%m-%d %H:%M",
                                      tz="Etc/GMT+8")
  }
  
  tBeg <- as.POSIXct("2014-06-10 00:00:00", tz="Etc/GMT+1")
  tEnd <- as.POSIXct("2014-06-10 23:00:00", tz="Etc/GMT+1")
  tAx  <- seq(tBeg, tEnd, by=3600)
  
  plot(rbwsti2014$dateTime, rbwsti2014$hN, xlim=c(tBeg, tEnd), xaxt="n")
  axis(1, at=tAx, labels=format(tAx, format="%H:%M\n%d-%m\n%Y"), padj=0.5)
  
  0.0065
  
}




# login data for chinese climate administration
# roberto.tatis-muvdi@kompetenz-wasser.de
# nmicKEYS01
# rtmKWB

# robetatis@yahoo.com
# ptbs011001
# rtmKWB2

# robetatis
# modBeij011

