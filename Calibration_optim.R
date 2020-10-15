
### Calibration with LID-Parameters for NSE

input <- read_inp(x = "Validation_greenroof3.inp")

seq <- seq.POSIXt(ISOdate(2015,4,25,00,00,tz="UTC"),
                  ISOdate(2015,4,28,00,00,tz="UTC"),
                  by="5 min")
# runoff results of the green roof
runoff <- rainData_r[which(rainData_r$DateTime == head(seq,1)) : which(rainData_r$DateTime == tail(seq,1)), c(1, 3, 6, 7)]

#ThS <- 90 # Soil Thickness
Por <- 0.6 # Porosity
FC <- 0.27 # Field Capacity
WP <- 0.13 # Wilting Point
Con <- 100 # Conductivity  
ConS <- 14 # Conductivity Slope
#SucH <- 50 # Suction Head
ThSt <- 20 # Storage Thickness
VR <- 0.7 # Void Ratio
FlowC <- 2 # Flow Coefficent
FlowE <- 1 # Flow Exponent 
Off <- 13.7 # Offset Height
#Sub <- 20.18 # Subcatchment Width

parameters <- c(Por, FC, WP, Con, ConS, ThSt, VR, FlowC, FlowE, Off) # 

NSE <- function(parameters) { ## Function to compute NSE between modeld and measured Runoff
  
  #input$lid_controls$Par1[3] <- parameters[1] # Soil Thickness
  input$lid_controls$Par2[3] <- parameters[1] # Porosity
  input$lid_controls$Par3[3] <- parameters[2] # Field Capacity
  input$lid_controls$Par4[3] <- parameters[3] # Wilting Point
  input$lid_controls$Par5[3] <- parameters[4] # Conductivity
  input$lid_controls$Par6[3] <- parameters[5] # Conductivity Slope
  #input$lid_controls$Par7[3] <- parameters[7] # Suction Head
  input$lid_controls$Par1[4] <- parameters[6] # Storage Thickness
  input$lid_controls$Par2[4] <- parameters[7] # Void Fraction
  input$lid_controls$Par1[5] <- parameters[8] # Flow Coefficient
  input$lid_controls$Par2[5] <- parameters[9] # Flow Exponent
  input$lid_controls$Par3[5] <- parameters[10] # Offset Height
  #input$lid_usage$Width <- parameters[13] # Subcatchment Width
  
  # save the changed input file
  write_inp(input,"Validation_greenroof3.inp") 
  
  # run swimm with changed input file
  files <- run_swmm(inp = "Validation_greenroof3.inp") 
  
  # read out results for itype 3 = system and vIndex 4 = runoff 
  results <- read_out(files$out, iType = 3, vIndex = c(1,4)) 
  
  # change timezone to UTC
  indexTZ(results$system_variable$total_runoff) <- "UTC"
  indexTZ(results$system_variable$total_rainfall) <- "UTC"
  # write model runoff in data frame and join it with measured runoff (measured runoff has some missing values)
  runoff_sim <- list()
  
  # extract model runoff and measured greenroof runoff for each calibration rain event (1-8)
  for (j in seq_along(cal_rain_events$Event)){
    
    runoff_sim[[j]] <- data.frame(matrix(
      data = NA,
      ncol = 1,
      nrow = length(seq.POSIXt(cal_rain_events$tBeg[j], (cal_rain_events$tEnd[j] + 300*144), by="5 min"))))
    
    colnames(runoff_sim[[j]])<-"sim" 
    
    runoff_sim[[j]]$DateTime <- seq.POSIXt(cal_rain_events$tBeg[j], (cal_rain_events$tEnd[j] + 300*144), by="5 min")
    
    runoff_sim[[j]]$sim <- (as.numeric(coredata(results$system_variable$total_runoff[which(index(results$system_variable$total_runoff) == cal_rain_events$tBeg[j]) : which(index(results$system_variable$total_runoff) == (cal_rain_events$tEnd[j] + 300*144))])))*300/101
    
    runoff_sim[[j]]$rain <- (as.numeric(coredata(results$system_variable$total_rainfall[which(index(results$system_variable$total_rainfall) == cal_rain_events$tBeg[j]) : which(index(results$system_variable$total_rainfall) == (cal_rain_events$tEnd[j] + 300*144))])))/12
    
    runoff_sim[[j]] <- semi_join(runoff_sim[[j]], runoff[which(runoff$DateTime == cal_rain_events$tBeg[j]) : which(runoff$DateTime == (cal_rain_events$tEnd[j] + 300*144)), ], by="DateTime")
    
    runoff_sim[[j]]$GD <- runoff$GD[which(runoff$DateTime == cal_rain_events$tBeg[j]) : which(runoff$DateTime == (cal_rain_events$tEnd[j] + 300*144))]
    
  }
  
  # calculate gof results for each event
  sim_p<-c(0,1,0)
  obs_p<-c(0,1,0)
  
  zero_gof<-gof(sim_p,obs_p)
  
  sim_results<-data.frame(matrix(
    data = NA,
    ncol = length(runoff_sim),
    nrow = nrow(zero_gof)))
  
  n_rows <- nrow(sim_results)
  
  for (k in seq_along(runoff_sim)){
    
    simulated <- runoff_sim[[k]]$sim
    observed <- runoff_sim[[k]]$GD
    
    if (sum(simulated) == 0 && sum(observed) == 0) {
      
      gof_result <- zero_gof
      
    } else if (sum(simulated) != 0 && sum(observed) == 0){
      
      gof_result <- gof(observed, simulated, na.rm = FALSE)
      
    } else {
      
      gof_result <- try(gof(simulated, observed, na.rm = FALSE))
      
    }
    
    if (inherits(gof_result, "try-error")) {
      
      kwb.utils::printIf(TRUE, simulated)
      kwb.utils::printIf(TRUE, observed)
      
      stop("Error in gof, see inputs above")
    }
    
    sim_results[, k] <- gof_result
    
  }
  
  # write mean gof results in data frame
  -(mean(as.numeric(sim_results[9, ])))
  
}

optim_NM <- optim(par = parameters, fn = NSE, method = "Nelder-Mead")


### 

### Calibration with LID-Parameters for NSE

input <- read_inp(x = "Validation_greenroof3.inp")

seq <- seq.POSIXt(ISOdate(2015,4,25,00,00,tz="UTC"),
                  ISOdate(2015,4,28,00,00,tz="UTC"),
                  by="5 min")
# runoff results of the green roof
runoff <- rainData_r[which(rainData_r$DateTime == head(seq,1)) : which(rainData_r$DateTime == tail(seq,1)), c(1, 3, 6, 7)]

#ThS <- 90 # Soil Thickness
Por <- 0.6 # Porosity
FC <- 0.27 # Field Capacity
WP <- 0.13 # Wilting Point
Con <- 100 # Conductivity  
ConS <- 14 # Conductivity Slope
#SucH <- 50 # Suction Head
ThSt <- 20 # Storage Thickness
VR <- 0.7 # Void Ratio
FlowC <- 2 # Flow Coefficent
FlowE <- 8.5 # Flow Exponent 
Off <- 16.2 # Offset Height
#Init_Sat <- 20 # Initial Saturation
#Sub <- 20.18 # Subcatchment Width

parameters <- c(Por, FC, WP, Con, ConS, ThSt, VR, FlowC, FlowE, Off) # 

NSE <- function(parameters) { ## Function to compute NSE between modeld and measured Runoff
  
  #input$lid_controls$Par1[3] <- parameters[1] # Soil Thickness
  input$lid_controls$Par2[3] <- parameters[1] # Porosity
  input$lid_controls$Par3[3] <- parameters[2] # Field Capacity
  input$lid_controls$Par4[3] <- parameters[3] # Wilting Point
  input$lid_controls$Par5[3] <- parameters[4] # Conductivity
  input$lid_controls$Par6[3] <- parameters[5] # Conductivity Slope
  #input$lid_controls$Par7[3] <- parameters[7] # Suction Head
  input$lid_controls$Par1[4] <- parameters[6] # Storage Thickness
  input$lid_controls$Par2[4] <- parameters[7] # Void Fraction
  input$lid_controls$Par1[5] <- parameters[8] # Flow Coefficient
  input$lid_controls$Par2[5] <- parameters[9] # Flow Exponent
  input$lid_controls$Par3[5] <- parameters[10] # Offset Height
  #input$lid_usage$Width <- parameters[13] # Subcatchment Width
  #input$lid_usage$InitSat <- parameters[11]
  
  
  # save the changed input file
  write_inp(input,"Validation_greenroof3.inp") 
  
  # run swimm with changed input file
  files <- run_swmm(inp = "Validation_greenroof3.inp") 
  
  # read out results for itype 3 = system and vIndex 4 = runoff 
  results <- read_out(files$out, iType = 3, vIndex = c(4)) 
  
  # change timezone to UTC
  indexTZ(results$system_variable$total_runoff) <- "UTC"
  
  # extract model runoff and measured greenroof runoff for each calibration rain event (1-8)
    
    runoff_sim <- data.frame(matrix(
      data = NA,
      ncol = 1,
      nrow = length(seq)))
    
    colnames(runoff_sim)<-"sim" 
    
    runoff_sim$DateTime <- seq
    
    runoff_sim$sim <- (as.numeric(coredata(results$system_variable$total_runoff ))) *300/101
    
    runoff_sim <- semi_join(runoff_sim, runoff, by="DateTime")
    
    runoff_sim$GD <- runoff$GD
    
    simulated <- runoff_sim$sim
    observed <- runoff_sim$GD
    
    gof_result <- gof(simulated, observed, na.rm = FALSE)
    
    (sum(simulated)-sum(observed))*gof_result[6]
    
    #-(as.numeric(gof_results[9]))
  
}

optim_event <- optim(par = parameters, fn = NSE, method = "Nelder-Mead")


sum(simulated)-sum(observed)
max(simulated)-max(observed)

x11()
plot(simulated ~ runoff_sim$DateTime, type = "l", col = "red",
     ylim = c(0,0.15))
lines(observed ~ runoff_sim$DateTime, type = "l", col = "black")
lines(runoff$KD ~ runoff$DateTime, type = "l", col = "blue")

### Calibration with LID-Parameters for Max

input <- read_inp(x = "Validation_greenroof3.inp")

seq <- seq.POSIXt(ISOdate(2014,9,12,14,30,tz="UTC"),
                  ISOdate(2016,1,31,23,55,tz="UTC"),
                  by="5 min")
# runoff results of the green roof
runoff <- rainData_r[which(rainData_r$DateTime == head(seq,1)) : which(rainData_r$DateTime == tail(seq,1)), c(1, 3, 6, 7)]

ThS <- 89.97 # Soil Thickness
Por <- 0.39 # Porosity
FC <- 0.187 # Field Capacity
WP <- 0.025 # Wilting Point
Con <- 96.1 # Conductivity  
ConS <- 23.29 # Conductivity Slope
SucH <- 50.2 # Suction Head
ThSt <- 9.9 # Storage Thickness
VR <- 0.62 # Void Ratio
FlowC <- 3.72 # Flow Coefficent
FlowE <- 2.1 # Flow Exponent 
Off <- 10.53 # Offset Height
Sub <- 20.36 # Subcatchment Width

parameters <- c(ThS, Por, FC, WP, Con, ConS, SucH, ThSt, VR, FlowC, FlowE, Off, Sub) # 

Peak <- function(parameters) { ## Function to compute NSE between modeld and measured Runoff
  
  input$lid_controls$Par1[3] <- parameters[1] # Soil Thickness
  input$lid_controls$Par2[3] <- parameters[2] # Porosity
  input$lid_controls$Par3[3] <- parameters[3] # Field Capacity
  input$lid_controls$Par4[3] <- parameters[4] # Wilting Point
  input$lid_controls$Par5[3] <- parameters[5] # Conductivity
  input$lid_controls$Par6[3] <- parameters[6] # Conductivity Slope
  input$lid_controls$Par7[3] <- parameters[7] # Suction Head
  input$lid_controls$Par1[4] <- parameters[8] # Storage Thickness
  input$lid_controls$Par2[4] <- parameters[9] # Void Fraction
  input$lid_controls$Par1[5] <- parameters[10] # Flow Coefficient
  input$lid_controls$Par2[5] <- parameters[11] # Flow Exponent
  input$lid_controls$Par3[5] <- parameters[12] # Offset Height
  input$lid_usage$Width <- parameters[13] # Subcatchment Width
  
  # save the changed input file
  write_inp(input,"Validation_greenroof3.inp") 
  
  # run swimm with changed input file
  files <- run_swmm(inp = "Validation_greenroof3.inp") 
  
  # read out results for itype 3 = system and vIndex 4 = runoff 
  results <- read_out(files$out, iType = 3, vIndex = c(4)) 
  
  # change timezone to UTC
  indexTZ(results$system_variable$total_runoff) <- "UTC"
  
  # write model runoff in data frame and join it with measured runoff (measured runoff has some missing values)
  runoff_sim <- list()
  
  # extract model runoff and measured greenroof runoff for each calibration rain event (1-8)
  for (j in seq_along(cal_rain_events$Event)){
    
    runoff_sim[[j]] <- data.frame(matrix(
      data = NA,
      ncol = 1,
      nrow = length(seq.POSIXt(cal_rain_events$tBeg[j], (cal_rain_events$tEnd[j] + 300*144), by="5 min"))))
    
    colnames(runoff_sim[[j]])<-"sim" 
    
    runoff_sim[[j]]$DateTime <- seq.POSIXt(cal_rain_events$tBeg[j], (cal_rain_events$tEnd[j] + 300*144), by="5 min")
    
    runoff_sim[[j]]$sim <- (as.numeric(coredata(results$system_variable$total_runoff[which(index(results$system_variable$total_runoff) == cal_rain_events$tBeg[j]) : which(index(results$system_variable$total_runoff) == (cal_rain_events$tEnd[j] + 300*144))])))*300/101
    
    runoff_sim[[j]] <- semi_join(runoff_sim[[j]], runoff[which(runoff$DateTime == cal_rain_events$tBeg[j]) : which(runoff$DateTime == (cal_rain_events$tEnd[j] + 300*144)), ], by="DateTime")
    
    runoff_sim[[j]]$GD <- runoff$GD[which(runoff$DateTime == cal_rain_events$tBeg[j]) : which(runoff$DateTime == (cal_rain_events$tEnd[j] + 300*144))]
    
  }
  
  # calculate gof results for each event
  sim_p<-c(0,1,0)
  obs_p<-c(0,1,0)
  
  zero_gof<-gof(sim_p,obs_p)
  
  sim_results<-data.frame(matrix(
    data = NA,
    ncol = length(runoff_sim),
    nrow = 3))
  
  n_rows <- nrow(sim_results)
  
  for (k in seq_along(runoff_sim)){
    
    simulated <- runoff_sim[[k]]$sim
    observed <- runoff_sim[[k]]$GD
    
    if (sum(simulated) == 0 && sum(observed) == 0) {
      
      max_result <- 0
      
    } else if (sum(simulated) != 0 && sum(observed) == 0){
      
      max_result <- sqrt((max(simulated) - max(observed))^2)
      
    } else {
      
      max_result <- sqrt((max(simulated) - max(observed))^2)
      
    }
    
    if (inherits(gof_result, "try-error")) {
      
      kwb.utils::printIf(TRUE, simulated)
      kwb.utils::printIf(TRUE, observed)
      
      stop("Error in gof, see inputs above")
    }
    
    sim_results[1, k] <- max_result
    sim_results[2, k] <- sum(simulated)
    sim_results[3, k] <- sum(observed)
  }
  
  # write mean gof results in data frame
  mean(as.numeric(sim_results[1, ])) + ((sqrt((sum(sim_results[2, ]) - sum(sim_results[3, ]))^2))/100)
  
}

optim_PS_NM_1 <- optim(par = parameters, fn = Peak, method = "Nelder-Mead")


### Calibration with Evaporation Parameters

input <- read_inp(x = "Validation_greenroof3.inp")

seq <- seq.POSIXt(ISOdate(2014,9,12,14,30,tz="UTC"),
                  ISOdate(2016,1,31,23,55,tz="UTC"),
                  by="5 min")
# runoff results of the green roof
runoff <- rainData_r[which(rainData_r$DateTime == head(seq,1)) : which(rainData_r$DateTime == tail(seq,1)), c(1, 3, 6, 7)]

x.1 <- mean(results$system_variable$potential_evaporation[which(index(results$system_variable$actual_evaporation) == 
                                                                  (as.POSIXct(ISOdate(2015,1,1,0,05,tz="UTC")))):
                                                            which(index(results$system_variable$actual_evaporation) ==
                                                                  as.POSIXct(ISOdate(2015,2,1,0,05,tz="UTC")))])# Jan

x.2 <- mean(results$system_variable$potential_evaporation[which(index(results$system_variable$actual_evaporation) == 
                                                                  (as.POSIXct(ISOdate(2015,2,1,0,05,tz="UTC")))):
                                                            which(index(results$system_variable$actual_evaporation) ==
                                                                    as.POSIXct(ISOdate(2015,3,1,0,05,tz="UTC")))])# Feb

x.3 <- mean(results$system_variable$potential_evaporation[which(index(results$system_variable$actual_evaporation) == 
                                                                  (as.POSIXct(ISOdate(2015,3,1,0,05,tz="UTC")))):
                                                            which(index(results$system_variable$actual_evaporation) ==
                                                                    as.POSIXct(ISOdate(2015,4,1,0,05,tz="UTC")))])# Mar

x.4 <- mean(results$system_variable$potential_evaporation[which(index(results$system_variable$actual_evaporation) == 
                                                                  (as.POSIXct(ISOdate(2015,4,1,0,05,tz="UTC")))):
                                                            which(index(results$system_variable$actual_evaporation) ==
                                                                    as.POSIXct(ISOdate(2015,5,1,0,05,tz="UTC")))])# Apr

x.5 <- mean(results$system_variable$potential_evaporation[which(index(results$system_variable$actual_evaporation) == 
                                                                  (as.POSIXct(ISOdate(2015,5,1,0,05,tz="UTC")))):
                                                            which(index(results$system_variable$actual_evaporation) ==
                                                                    as.POSIXct(ISOdate(2015,6,1,0,05,tz="UTC")))])# Mai

x.6 <- mean(results$system_variable$potential_evaporation[which(index(results$system_variable$actual_evaporation) == 
                                                                  (as.POSIXct(ISOdate(2015,6,1,0,05,tz="UTC")))):
                                                            which(index(results$system_variable$actual_evaporation) ==
                                                                    as.POSIXct(ISOdate(2015,7,1,0,05,tz="UTC")))])# Jun

x.7 <- mean(results$system_variable$potential_evaporation[which(index(results$system_variable$actual_evaporation) == 
                                                                  (as.POSIXct(ISOdate(2015,7,1,0,05,tz="UTC")))):
                                                            which(index(results$system_variable$actual_evaporation) ==
                                                                    as.POSIXct(ISOdate(2015,8,1,0,05,tz="UTC")))])# Jul

x.8 <- mean(results$system_variable$potential_evaporation[which(index(results$system_variable$actual_evaporation) == 
                                                                  (as.POSIXct(ISOdate(2015,8,1,0,05,tz="UTC")))):
                                                            which(index(results$system_variable$actual_evaporation) ==
                                                                    as.POSIXct(ISOdate(2015,9,1,0,05,tz="UTC")))])# Aug

x.9 <- mean(results$system_variable$potential_evaporation[which(index(results$system_variable$actual_evaporation) == 
                                                                  (as.POSIXct(ISOdate(2015,9,1,0,05,tz="UTC")))):
                                                            which(index(results$system_variable$actual_evaporation) ==
                                                                    as.POSIXct(ISOdate(2015,10,1,0,05,tz="UTC")))])# Sep

x.10 <- mean(results$system_variable$potential_evaporation[which(index(results$system_variable$actual_evaporation) == 
                                                                   (as.POSIXct(ISOdate(2015,10,1,0,05,tz="UTC")))):
                                                             which(index(results$system_variable$actual_evaporation) ==
                                                                     as.POSIXct(ISOdate(2015,11,1,0,05,tz="UTC")))])# Okt

x.11 <- mean(results$system_variable$potential_evaporation[which(index(results$system_variable$actual_evaporation) == 
                                                                   (as.POSIXct(ISOdate(2015,11,1,0,05,tz="UTC")))):
                                                             which(index(results$system_variable$actual_evaporation) ==
                                                                     as.POSIXct(ISOdate(2015,12,1,0,05,tz="UTC")))])# Nov

x.12 <- mean(results$system_variable$potential_evaporation[which(index(results$system_variable$actual_evaporation) == 
                                                                   (as.POSIXct(ISOdate(2015,12,1,0,05,tz="UTC")))):
                                                             which(index(results$system_variable$actual_evaporation) ==
                                                                     as.POSIXct(ISOdate(2016,1,1,0,05,tz="UTC")))])# Dez

parameters <- c(x.1, x.2, x.3, x.4, x.5, x.6, x.7, x.8, x.9, x.10, x.11, x.12) # 

Peak <- function(parameters) { ## Function to compute NSE between modeld and measured Runoff
  
  input$evaporation$Parameters[1]<-paste(parameters[1], 
                                         parameters[2],
                                         parameters[3],
                                         parameters[4],
                                         parameters[5],
                                         parameters[6],
                                         parameters[7],
                                         parameters[8],
                                         parameters[9],
                                         parameters[10],
                                         parameters[11],
                                         parameters[12]) 
  
  # save the changed input file
  write_inp(input,"Validation_greenroof3.inp") 
  
  # run swimm with changed input file
  files <- run_swmm(inp = "Validation_greenroof3.inp") 
  
  # read out results for itype 3 = system and vIndex 4 = runoff 
  results <- read_out(files$out, iType = 3, vIndex = c(4)) 
  
  # change timezone to UTC
  indexTZ(results$system_variable$total_runoff) <- "UTC"
  
  # write model runoff in data frame and join it with measured runoff (measured runoff has some missing values)
  runoff_sim <- list()
  
  # extract model runoff and measured greenroof runoff for each calibration rain event (1-8)
  for (j in seq_along(cal_rain_events$Event)){
    
    runoff_sim[[j]] <- data.frame(matrix(
      data = NA,
      ncol = 1,
      nrow = length(seq.POSIXt(cal_rain_events$tBeg[j], (cal_rain_events$tEnd[j] + 300*144), by="5 min"))))
    
    colnames(runoff_sim[[j]])<-"sim" 
    
    runoff_sim[[j]]$DateTime <- seq.POSIXt(cal_rain_events$tBeg[j], (cal_rain_events$tEnd[j] + 300*144), by="5 min")
    
    runoff_sim[[j]]$sim <- (as.numeric(coredata(results$system_variable$total_runoff[which(index(results$system_variable$total_runoff) == cal_rain_events$tBeg[j]) : which(index(results$system_variable$total_runoff) == (cal_rain_events$tEnd[j] + 300*144))])))*300/101
    
    runoff_sim[[j]] <- semi_join(runoff_sim[[j]], runoff[which(runoff$DateTime == cal_rain_events$tBeg[j]) : which(runoff$DateTime == (cal_rain_events$tEnd[j] + 300*144)), ], by="DateTime")
    
    runoff_sim[[j]]$GD <- runoff$GD[which(runoff$DateTime == cal_rain_events$tBeg[j]) : which(runoff$DateTime == (cal_rain_events$tEnd[j] + 300*144))]
    
  }
  
  # calculate gof results for each event
  sim_p<-c(0,1,0)
  obs_p<-c(0,1,0)
  
  zero_gof<-gof(sim_p,obs_p)
  
  sim_results<-data.frame(matrix(
    data = NA,
    ncol = length(runoff_sim),
    nrow = 3))
  
  n_rows <- nrow(sim_results)
  
  for (k in seq_along(runoff_sim)){
    
    simulated <- runoff_sim[[k]]$sim
    observed <- runoff_sim[[k]]$GD
    
    if (sum(simulated) == 0 && sum(observed) == 0) {
      
      max_result <- 0
      
    } else if (sum(simulated) != 0 && sum(observed) == 0){
      
      max_result <- sqrt((max(simulated) - max(observed))^2)
      
    } else {
      
      max_result <- sqrt((max(simulated) - max(observed))^2)
      
    }
    
    if (inherits(gof_result, "try-error")) {
      
      kwb.utils::printIf(TRUE, simulated)
      kwb.utils::printIf(TRUE, observed)
      
      stop("Error in gof, see inputs above")
    }
    
    sim_results[1, k] <- max_result
    sim_results[2, k] <- sum(simulated)
    sim_results[3, k] <- sum(observed)
  }
  
  # write mean gof results in data frame
  mean(as.numeric(sim_results[1, ])) + ((sqrt((sum(sim_results[2, ]) - sum(sim_results[3, ]))^2))/400)
  
}

optim_test_evap2 <- optim(par = parameters, fn = Peak)

### Calibration with LID & Evaporation Parameters

ThS <- 90 # Soil Thickness
Por <- 0.6 # Porosity
FC <- 0.145 # Field Capacity
WP <- 0.066 # Wilting Point
Con <- 92.5 # Conductivity  
ConS <- 18.7 # Conductivity Slope
SucH <- 50 # Suction Head
ThSt <- 10 # Storage Thickness
VR <- 0.7 # Void Ratio
FlowC <- 3.69 # Flow Coefficent
FlowE <- 1.95 # Flow Exponent 
Off <- 10.32 # Offset Height
Sub <- 20.18 # Subcatchment Width

x.1 <- optim_test_evap1$par[1]
x.2 <- optim_test_evap1$par[2]
x.3 <- optim_test_evap1$par[3]
x.4 <- optim_test_evap1$par[4]
x.5 <- optim_test_evap1$par[5]
x.6 <- optim_test_evap1$par[6]
x.7 <- optim_test_evap1$par[7]
x.8 <- optim_test_evap1$par[8]
x.9 <- optim_test_evap1$par[9]
x.10 <- optim_test_evap1$par[10]
x.11 <- optim_test_evap1$par[11]
x.12 <- optim_test_evap1$par[12]

parameters <- c(ThS, Por, FC, WP, Con, ConS, SucH, ThSt, VR, FlowC, FlowE, Off, Sub, x.1, x.2, x.3, x.4, x.5, x.6, x.7, x.8, x.9, x.10, x.11, x.12) # 

NSE <- function(parameters) { ## Function to compute NSE between modeld and measured Runoff
  
  input$lid_controls$Par1[3] <- parameters[1] # Soil Thickness
  input$lid_controls$Par2[3] <- parameters[2] # Porosity
  input$lid_controls$Par3[3] <- parameters[3] # Field Capacity
  input$lid_controls$Par4[3] <- parameters[4] # Wilting Point
  input$lid_controls$Par5[3] <- parameters[5] # Conductivity
  input$lid_controls$Par6[3] <- parameters[6] # Conductivity Slope
  input$lid_controls$Par7[3] <- parameters[7] # Suction Head
  input$lid_controls$Par1[4] <- parameters[8] # Storage Thickness
  input$lid_controls$Par2[4] <- parameters[9] # Void Fraction
  input$lid_controls$Par1[5] <- parameters[10] # Flow Coefficient
  input$lid_controls$Par2[5] <- parameters[11] # Flow Exponent
  input$lid_controls$Par3[5] <- parameters[12] # Offset Height
  input$lid_usage$Width <- parameters[13] # Subcatchment Width
  
  input$evaporation$Parameters[1] <- paste(parameters[14], 
                                         parameters[15],
                                         parameters[16],
                                         parameters[17],
                                         parameters[18],
                                         parameters[19],
                                         parameters[20],
                                         parameters[21],
                                         parameters[22],
                                         parameters[23],
                                         parameters[24],
                                         parameters[25]) 
  input$evaporation$Parameters[2] <- "YES"
  
  # save the changed input file
  write_inp(input,"Validation_greenroof3.inp") 
  
  # run swimm with changed input file
  files <- run_swmm(inp = "Validation_greenroof3.inp") 
  
  # read out results for itype 3 = system and vIndex 4 = runoff 
  results <- read_out(files$out, iType = 3, vIndex = c(4)) 
  
  # change timezone to UTC
  indexTZ(results$system_variable$total_runoff) <- "UTC"
  
  # write model runoff in data frame and join it with measured runoff (measured runoff has some missing values)
  runoff_sim <- list()
  
  # extract model runoff and measured greenroof runoff for each calibration rain event (1-8)
  for (j in seq_along(cal_rain_events$Event)){
    
    runoff_sim[[j]] <- data.frame(matrix(
      data = NA,
      ncol = 1,
      nrow = length(seq.POSIXt(cal_rain_events$tBeg[j], (cal_rain_events$tEnd[j] + 300*144), by="5 min"))))
    
    colnames(runoff_sim[[j]])<-"sim" 
    
    runoff_sim[[j]]$DateTime <- seq.POSIXt(cal_rain_events$tBeg[j], (cal_rain_events$tEnd[j] + 300*144), by="5 min")
    
    runoff_sim[[j]]$sim <- (as.numeric(coredata(results$system_variable$total_runoff[which(index(results$system_variable$total_runoff) == cal_rain_events$tBeg[j]) : which(index(results$system_variable$total_runoff) == (cal_rain_events$tEnd[j] + 300*144))])))*300/101
    
    runoff_sim[[j]] <- semi_join(runoff_sim[[j]], runoff[which(runoff$DateTime == cal_rain_events$tBeg[j]) : which(runoff$DateTime == (cal_rain_events$tEnd[j] + 300*144)), ], by="DateTime")
    
    runoff_sim[[j]]$GD <- runoff$GD[which(runoff$DateTime == cal_rain_events$tBeg[j]) : which(runoff$DateTime == (cal_rain_events$tEnd[j] + 300*144))]
    
  }
  
  # calculate gof results for each event
  sim_p<-c(0,1,0)
  obs_p<-c(0,1,0)
  
  zero_gof<-gof(sim_p,obs_p)
  
  sim_results<-data.frame(matrix(
    data = NA,
    ncol = length(runoff_sim),
    nrow = nrow(zero_gof)))
  
  n_rows <- nrow(sim_results)
  
  for (k in seq_along(runoff_sim)){
    
    simulated <- runoff_sim[[k]]$sim
    observed <- runoff_sim[[k]]$GD
    
    if (sum(simulated) == 0 && sum(observed) == 0) {
      
      gof_result <- zero_gof
      
    } else if (sum(simulated) != 0 && sum(observed) == 0){
      
      gof_results <- gof(observed, simulated, na.rm = FALSE)
      
    } else {
      
      gof_result <- try(gof(simulated, observed, na.rm = FALSE))
      
    }
    
    if (inherits(gof_result, "try-error")) {
      
      kwb.utils::printIf(TRUE, simulated)
      kwb.utils::printIf(TRUE, observed)
      
      stop("Error in gof, see inputs above")
    }
    
    sim_results[, k] <- gof_result
    
  }
  
  # write mean gof results in data frame
  -(mean(as.numeric(sim_results[9, ])))
  
}

optim_both <- optim(par = parameters, fn = NSE)

parameters <- as.numeric(cal_results[72,2:13])
parameters <- best_run_7[,2:9]
parameters <- optim_PS_NM_1$par

input$lid_controls$Par1[3] <- parameters[1] # Soil Thickness
input$lid_controls$Par2[3] <- parameters[2] # Porosity
input$lid_controls$Par3[3] <- parameters[3] # Field Capacity
input$lid_controls$Par4[3] <- parameters[4] # Wilting Point
input$lid_controls$Par5[3] <- parameters[5] # Conductivity
input$lid_controls$Par6[3] <- parameters[6] # Conductivity Slope
#input$lid_controls$Par7[3] <- parameters[7] # Suction Head
input$lid_controls$Par1[4] <- parameters[7] # Storage Thickness
input$lid_controls$Par2[4] <- parameters[8] # Void Fraction
input$lid_controls$Par1[5] <- parameters[9] # Flow Coefficient
input$lid_controls$Par2[5] <- parameters[10] # Flow Exponent
input$lid_controls$Par3[5] <- parameters[11] # Offset Height
input$lid_usage$Width <- parameters[12] # Subcatchment Width

#input$evaporation$Parameters[1] <- paste(parameters[14], 
                                         #parameters[15],
                                         #parameters[16],
                                         #parameters[17],
                                         #parameters[18],
                                         #parameters[19],
                                         #parameters[20],
                                         #parameters[21],
                                         #parameters[22],
                                         #parameters[23],
                                         #parameters[24],
                                         #parameters[25]) 
#input$evaporation$Parameters[2] <- "YES"

write_inp(input,"Validation_greenroof3.inp") # save the changed input file
files <- run_swmm(inp = "Validation_greenroof3.inp") # run swimm with changed input file
results <- read_out(files$out, iType = 3, vIndex = c(4,14,13)) # read out results for itype 3 = system and vIndex 4 = runoff
indexTZ(results$system_variable$total_runoff)<-"UTC"
indexTZ(results$system_variable$actual_evaporation)<-"UTC"

runoff_sim<-list()
for (j in 1:length(cal_rain_events$Event)){
  runoff_sim[[j]]<-data.frame(matrix(data=NA, ncol = 1,nrow = length(seq.POSIXt(cal_rain_events$tBeg[j],(cal_rain_events$tEnd[j]+300*144),by="5 min"))))
  
  colnames(runoff_sim[[j]])<-"sim" 
  runoff_sim[[j]]$DateTime<-seq.POSIXt(cal_rain_events$tBeg[j],(cal_rain_events$tEnd[j]+300*144),by="5 min")
  runoff_sim[[j]]$sim<-(as.numeric(coredata(results$system_variable$total_runoff[which(index(results$system_variable$total_runoff)==cal_rain_events$tBeg[j]):which(index(results$system_variable$total_runoff)==(cal_rain_events$tEnd[j]+300*144))])))*300/101
  runoff_sim[[j]]$evap<-as.numeric(coredata(results$system_variable$actual_evaporation[which(index(results$system_variable$actual_evaporation)==cal_rain_events$tBeg[j]):which(index(results$system_variable$actual_evaporation)==(cal_rain_events$tEnd[j]+300*144))]))
  runoff_sim[[j]]<-semi_join(runoff_sim[[j]],runoff[which(runoff$DateTime==cal_rain_events$tBeg[j]):which(runoff$DateTime==(cal_rain_events$tEnd[j]+300*144)),],by="DateTime")
  runoff_sim[[j]]$evap<-runoff_sim[[j]]$evap/288
  runoff_sim[[j]]$GD<-runoff$GD[which(runoff$DateTime==cal_rain_events$tBeg[j]):which(runoff$DateTime==(cal_rain_events$tEnd[j]+300*144))]
}

# calculate gof results for each event
sim_p<-c(0,1,0)
obs_p<-c(0,1,0)

zero_gof<-gof(sim_p,obs_p)

sim_results<-data.frame(matrix(
  data = NA,
  ncol = length(runoff_sim),
  nrow = nrow(zero_gof)+3))

n_rows <- nrow(sim_results)

for (k in seq_along(runoff_sim)){
  
  simulated <- runoff_sim[[k]]$sim
  observed <- runoff_sim[[k]]$GD
  
  if (sum(simulated) == 0 && sum(observed) == 0) {
    
    gof_result <- zero_gof
    max_result <- 0
    
    
  } else if (sum(simulated) != 0 && sum(observed) == 0){
    
    gof_results <- gof(observed, simulated, na.rm = FALSE)
    max_result <- sqrt((max(simulated) - max(observed))^2)
    
    
  } else {
    
    gof_result <- try(gof(simulated, observed, na.rm = FALSE))
    max_result <- sqrt((max(simulated) - max(observed))^2)
    
    
  }
  
  if (inherits(gof_result, "try-error")) {
    
    kwb.utils::printIf(TRUE, simulated)
    kwb.utils::printIf(TRUE, observed)
    
    stop("Error in gof, see inputs above")
  }
  
  sim_results[1:nrow(zero_gof), k] <- gof_result
  sim_results[ n_rows-2, k] <- max_result
  sim_results[ n_rows-1, k] <- sum(simulated)
  sim_results[ n_rows, k] <- sum(observed)
}

((sum(sim_results[22,])-sum(sim_results[23,]))/sum(sim_results[23,]))*100

pdf_file <- preparePdf(file.path(tempdir(), "example64_preparePdf.pdf"))

# Plot something

for (i in 1:length(cal_rain_events$tBeg)) {
  plot(runoff_sim[[i]]$GD~runoff_sim[[i]]$DateTime,xaxt="n",
       type="l",
       col="blue",
       ylim=c(0,0.5),
       xlab="Zeit",ylab="Abfluss [l/5 min]/Niederschlag[mm/5min]",
       main="Validierung des Gründachabflusses") 
  lines(runoff_sim[[i]]$sim~runoff_sim[[i]]$DateTime,
        col="red")
  legend("topright",legend = c("Abflussmessung","Modell","Niederschlag"),col = c("blue","red","black"),lty = 1)
  axis.POSIXct(1,at=seq(head(runoff_sim[[i]]$DateTime,1),tail(runoff_sim[[i]]$DateTime,1),by="day"),format="%d.%m")
  mtext(text = paste("NSE =" , round(sim_results[9,1], 2) , " dMax =" , round(sim_results[21,i],2) , " dVol =" , round((((sim_results[22,i]-sim_results[23,i])/sim_results[23,i])*100),1) , sep = " "), 
        side = 3)
        
  #lines(runoff$NSB[which(runoff$DateTime==runoff_events$tBeg[i]):which(runoff$DateTime==runoff_events$tEnd[i])]~
  # runoff$DateTime[which(runoff$DateTime==runoff_events$tBeg[i]):which(runoff$DateTime==runoff_events$tEnd[i])],type="h",col="black")
  #ggof(sim = runoff$sim[which(runoff$DateTime==runoff_events$tBeg[i]):which(runoff$DateTime==runoff_events$tEnd[i])],
  #obs = runoff$GD[which(runoff$DateTime==runoff_events$tBeg[i]):which(runoff$DateTime==runoff_events$tEnd[i])],
  #dates = runoff$DateTime[which(runoff$DateTime==runoff_events$tBeg[i]):which(runoff$DateTime==runoff_events$tEnd[i])])
}



# Open PDF file in viewer
finishAndShowPdf(pdf_file)

