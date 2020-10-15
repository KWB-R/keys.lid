########################################################################################################
#CALIBRATION WITH RAIN EVENTS
########################################################################################################

# read swmm input file
input <- read_inp(x = "Validation_greenroof3.inp")
# length of the loop
l <- 1000

# data frame to write in parameter values and Goodness of fit (gof) results

#cal_names<-c("Run","Surf_Roughn","Slope","Soil_Thickness","Porosity","Field_Capacity",
#             "Wilting_Point","Conductivity","Conductivity_Slope","Suction_Head","Stor_Thick","Void_Fraction",
#             "FlowCoeff","FlowExp","Offset_Height","InitSat","Sub_Width","NSE","PBIAS","Total_Runoff","Total_Evaporation")


cal_names<-c("Run", "Soil_Thickness", "Porosity", "Field_Capacity", 
             "Wilting_Point", "Conductivity", "Conductivity_Slope", 
             "Suction_Head", "Stor_Thick", "Void_Fraction", "Roughness",
             
             # Abfluss ?ber alle 8 Events
             "Sum_R_ges", "Sum_R_w", "Sum_R_s", # Abflusssumme
             "Max_R_ges", "Max_R_w", "Max_R_s", # Abflussspitze
             "Dur_R_ges", "Dur_R_w", "Dur_R_s", # Abflussdauer
             "Sum_diff_ges", "Sum_diff_w", "Sum_diff_s", # Differenz der Abflusssumme Simulation und Messung 
             "Max_diff_ges", "Max_diff_w", "Max_diff_s", # Different der Abflussspitze Simulation und Messung
             "Dur_diff_ges", "Dur_diff_w", "Dur_diff_s", # Differenz der Abflussdauer Simulation und Messung
             
             # Event 1
             "E1_Sum_R", # Abflusssumme
             "E1_Max_R", # Abflussspitze
             "E1_Dur_R", # Abflussdauer
             "E1_Sum_diff", # Differenz der Abflusssumme Simulation und Messung 
             "E1_Max_diff", # Different der Abflussspitze Simulation und Messung
             "E1_Dur_diff", # Differenz der Abflussdauer Simulation und Messung  
             
             # Event 2
             "E2_Sum_R", # Abflusssumme
             "E2_Max_R", # Abflussspitze
             "E2_Dur_R", # Abflussdauer
             "E2_Sum_diff", # Differenz der Abflusssumme Simulation und Messung 
             "E2_Max_diff", # Different der Abflussspitze Simulation und Messung
             "E2_Dur_diff", # Differenz der Abflussdauer Simulation und Messung
             
             # Event 3
             "E3_Sum_R", # Abflusssumme
             "E3_Max_R", # Abflussspitze
             "E3_Dur_R", # Abflussdauer
             "E3_Sum_diff", # Differenz der Abflusssumme Simulation und Messung 
             "E3_Max_diff", # Different der Abflussspitze Simulation und Messung
             "E3_Dur_diff", # Differenz der Abflussdauer Simulation und Messung
             
             # Event 4
             "E4_Sum_R", # Abflusssumme
             "E4_Max_R", # Abflussspitze
             "E4_Dur_R", # Abflussdauer
             "E4_Sum_diff", # Differenz der Abflusssumme Simulation und Messung 
             "E4_Max_diff", # Different der Abflussspitze Simulation und Messung
             "E4_Dur_diff", # Differenz der Abflussdauer Simulation und Messung
             
             # Event 5
             "E5_Sum_R", # Abflusssumme
             "E5_Max_R", # Abflussspitze
             "E5_Dur_R", # Abflussdauer
             "E5_Sum_diff", # Differenz der Abflusssumme Simulation und Messung 
             "E5_Max_diff", # Different der Abflussspitze Simulation und Messung
             "E5_Dur_diff", # Differenz der Abflussdauer Simulation und Messung
             
             # Event 6
             "E6_Sum_R", # Abflusssumme
             "E6_Max_R", # Abflussspitze
             "E6_Dur_R", # Abflussdauer
             "E6_Sum_diff", # Differenz der Abflusssumme Simulation und Messung 
             "E6_Max_diff", # Different der Abflussspitze Simulation und Messung
             "E6_Dur_diff", # Differenz der Abflussdauer Simulation und Messung
             
             # Event 7
             "E7_Sum_R", # Abflusssumme
             "E7_Max_R", # Abflussspitze
             "E7_Dur_R", # Abflussdauer
             "E7_Sum_diff", # Differenz der Abflusssumme Simulation und Messung 
             "E7_Max_diff", # Different der Abflussspitze Simulation und Messung
             "E7_Dur_diff", # Differenz der Abflussdauer Simulation und Messung
             
             # Event 8
             "E8_Sum_R", # Abflusssumme
             "E8_Max_R", # Abflussspitze
             "E8_Dur_R", # Abflussdauer
             "E8_Sum_diff", # Differenz der Abflusssumme Simulation und Messung 
             "E8_Max_diff", # Different der Abflussspitze Simulation und Messung
             "E8_Dur_diff" # Differenz der Abflussdauer Simulation und Messung
             )

cal_results <- data.frame(matrix(
  data = NA,
  ncol = length(cal_names),
  nrow = l))

cal_col <- colnames(cal_results)
cal_results <- gdata::rename.vars(cal_results,cal_col, cal_names)

# time sequence for simulation results
seq <- seq.POSIXt(ISOdate(2014,9,12,14,30,tz="UTC"),
                  ISOdate(2016,1,31,23,55,tz="UTC"),
                  by="5 min")

# BWSTI Beijing events period
seq <- seq.POSIXt(ISOdate(2014,6,12,14,30,tz="UTC"),
                  ISOdate(2016,8,18,23,55,tz="UTC"),
                  by="5 min")

# runoff results of the green roof
runoff <- rainData_r[which(rainData_r$DateTime == head(seq,1)) : which(rainData_r$DateTime == tail(seq,1)), c(1, 3, 6, 7)]

for (i in 1:l){
  
  # set parameters for simulation
  input$lid_controls$Par1[3] <- runif(n = 1, min = 70, max = 110) # Soil Thickness
  input$lid_controls$Par2[3] <- runif(n = 1, min = 0.4, max = 0.8) # Porosity
  input$lid_controls$Par3[3] <- runif(n = 1, min = 0.2, max = 0.39) # Field Capacity
  input$lid_controls$Par4[3] <- runif(n = 1, min = 0.01, max = 0.19) # Wilting Point
  input$lid_controls$Par5[3] <- runif(n = 1, min = 50, max = 300) # Conductivity
  input$lid_controls$Par6[3] <- runif(n = 1, min = 5, max = 30) # Conductivity Slope
  input$lid_controls$Par7[3] <- runif(n = 1, min = 1.9, max = 60) # Suction Head
  input$lid_controls$Par1[4] <- runif(n = 1, min = 5, max = 20) # Storage Thickness
  input$lid_controls$Par2[4] <- runif(n = 1, min = 0.4, max = 0.8) # Void Fraction
  input$lid_controls$Par3[4] <- runif(n = 1, min = 0.5, max = 4.5) # Roughness
  
  # write values in the dataframe
  cal_results$Run[i] <- i
  
  cal_results$Soil_Thickness[i] <- input$lid_controls$Par1[3]
  cal_results$Porosity[i] <- input$lid_controls$Par2[3]
  cal_results$Field_Capacity[i] <- input$lid_controls$Par3[3] 
  cal_results$Wilting_Point[i] <- input$lid_controls$Par4[3]
  cal_results$Conductivity[i] <- input$lid_controls$Par5[3]
  cal_results$Conductivity_Slope[i] <- input$lid_controls$Par6[3]
  cal_results$Suction_Head[i] <- input$lid_controls$Par7[3]
  cal_results$Stor_Thick[i] <- input$lid_controls$Par1[4]
  cal_results$Void_Fraction[i] <- input$lid_controls$Par2[4]
  cal_results$Roughness[i] <- input$lid_controls$Par3[4]
  
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
  
  ### calculate sum, max and duration results for all events
  
  sim_results<-data.frame(matrix(
    data = NA,
    ncol = length(runoff_sim),
    nrow = nrow(6)))
  
  for (k in seq_along(runoff_sim)){
    
    simulated <- runoff_sim[[k]]$sim
    observed <- runoff_sim[[k]]$GD
  
  sim_results[1, k] <- sum(simulated) 
  sim_results[2, k] <- sum(observed)
  
  sim_results[3, k] <- max(simulated)
  sim_results[4, k] <- max(observed)
  
  sim_results[5, k] <- length(simulated[simulated > 0.03])
  sim_results[6, k] <- length(observed[observed > 0.03])
  
  }
 
  # Results for all Events
  sum_sim <- sum(sim_results[1, ]) 
  sum_obs <- sum(sim_results[2, ])
  max_sim <- mean(sim_results[3, ])
  max_obs <- mean(sim_results[4, ])
  dur_sim <- sum(sim_results[5, ])
  dur_obs <- sum(sim_results[6, ])
  
  # Results for winter Events
  wsum_sim <- sum(sim_results[1, c(1,7,8)])
  wsum_obs <- sum(sim_results[2, c(1,7,8)])
  wmax_sim <- mean(sim_results[3, c(1,7,8)])
  wmax_obs <- mean(sim_results[4, c(1,7,8)])
  wdur_sim <- sum(sim_results[5, c(1,7,8)])
  wdur_obs <- sum(sim_results[6, c(1,7,8)])
  
  # Results for summer Events
  ssum_sim <- sum(sim_results[1, c(2,3,4,5,6)])
  ssum_obs <- sum(sim_results[2, c(2,3,4,5,6)])
  smax_sim <- mean(sim_results[3, c(2,3,4,5,6)])
  smax_obs <- mean(sim_results[4, c(2,3,4,5,6)])
  sdur_sim <- sum(sim_results[5, c(2,3,4,5,6)])
  sdur_obs <- sum(sim_results[6, c(2,3,4,5,6)])
  
  ### write mean gof results in data frame
  
  # Results for all, winter, summer Events only Simulation
  cal_results$Sum_R_ges[i] <- sum_sim
  cal_results$Sum_R_w[i] <- wsum_sim
  cal_results$Sum_R_s[i] <- ssum_sim
  cal_results$Max_R_ges[i] <- max_sim
  cal_results$Max_R_w[i] <- wmax_sim
  cal_results$Max_R_s[i] <- smax_sim
  cal_results$Dur_R_ges[i] <- dur_sim  
  cal_results$Dur_R_w[i] <- wdur_sim
  cal_results$Dur_R_s[i] <- sdur_sim
  
  # Results for all, winter, summer Events Simulation - Measured
  cal_results$Sum_diff_ges[i] <- sum_sim - sum_obs
  cal_results$Sum_diff_w[i] <- wsum_sim - wsum_obs
  cal_results$Sum_diff_s[i] <- ssum_sim - ssum_obs
  cal_results$Max_diff_ges[i] <- max_sim - max_obs
  cal_results$Max_diff_w[i] <- wmax_sim - wmax_obs 
  cal_results$Max_diff_s[i] <- smax_sim - smax_obs
  cal_results$Dur_diff_ges[i] <- dur_sim - dur_obs
  cal_results$Dur_diff_w[i] <- wdur_sim - wdur_obs
  cal_results$Dur_diff_s[i] <- sdur_sim - sdur_obs
  
  # Results for Event 1
  cal_results$E1_Sum_R <- sum(runoff_sim[[1]]$sim)
  cal_results$E1_Max_R <- max(runoff_sim[[1]]$sim) 
  cal_results$E1_Dur_R <- length(runoff_sim[[1]]$sim[runoff_sim[[1]]$sim > 0.03])
  cal_results$E1_Sum_diff <- sum(runoff_sim[[1]]$sim) - sum(runoff_sim[[1]]$obs)
  cal_results$E1_Max_diff <- max(runoff_sim[[1]]$sim) - max(runoff_sim[[1]]$obs)
  cal_results$E1_Dur_diff <- length(runoff_sim[[1]]$sim[runoff_sim[[1]]$sim > 0.03]) - length(runoff_sim[[1]]$obs[runoff_sim[[1]]$obs > 0.03])
  
  # Results for Event 2
  cal_results$E2_Sum_R <- sum(runoff_sim[[2]]$sim)
  cal_results$E2_Max_R <- max(runoff_sim[[2]]$sim) 
  cal_results$E2_Dur_R <- length(runoff_sim[[2]]$sim[runoff_sim[[2]]$sim > 0.03])
  cal_results$E2_Sum_diff <- sum(runoff_sim[[2]]$sim) - sum(runoff_sim[[2]]$obs)
  cal_results$E2_Max_diff <- max(runoff_sim[[2]]$sim) - max(runoff_sim[[2]]$obs)
  cal_results$E2_Dur_diff <- length(runoff_sim[[2]]$sim[runoff_sim[[2]]$sim > 0.03]) - length(runoff_sim[[2]]$obs[runoff_sim[[2]]$obs > 0.03])
  
  # Results for Event 3
  cal_results$E3_Sum_R <- sum(runoff_sim[[3]]$sim)
  cal_results$E3_Max_R <- max(runoff_sim[[3]]$sim) 
  cal_results$E3_Dur_R <- length(runoff_sim[[3]]$sim[runoff_sim[[3]]$sim > 0.03])
  cal_results$E3_Sum_diff <- sum(runoff_sim[[3]]$sim) - sum(runoff_sim[[3]]$obs)
  cal_results$E3_Max_diff <- max(runoff_sim[[3]]$sim) - max(runoff_sim[[3]]$obs)
  cal_results$E3_Dur_diff <- length(runoff_sim[[3]]$sim[runoff_sim[[3]]$sim > 0.03]) - length(runoff_sim[[3]]$obs[runoff_sim[[3]]$obs > 0.03])
  
  # Results for Event 4
  cal_results$E4_Sum_R <- sum(runoff_sim[[4]]$sim)
  cal_results$E4_Max_R <- max(runoff_sim[[4]]$sim) 
  cal_results$E4_Dur_R <- length(runoff_sim[[4]]$sim[runoff_sim[[4]]$sim > 0.03])
  cal_results$E4_Sum_diff <- sum(runoff_sim[[4]]$sim) - sum(runoff_sim[[4]]$obs)
  cal_results$E4_Max_diff <- max(runoff_sim[[4]]$sim) - max(runoff_sim[[4]]$obs)
  cal_results$E4_Dur_diff <- length(runoff_sim[[4]]$sim[runoff_sim[[4]]$sim > 0.03]) - length(runoff_sim[[4]]$obs[runoff_sim[[4]]$obs > 0.03])
  
  # Results for Event 5
  cal_results$E5_Sum_R <- sum(runoff_sim[[5]]$sim)
  cal_results$E5_Max_R <- max(runoff_sim[[5]]$sim) 
  cal_results$E5_Dur_R <- length(runoff_sim[[5]]$sim[runoff_sim[[5]]$sim > 0.03])
  cal_results$E5_Sum_diff <- sum(runoff_sim[[5]]$sim) - sum(runoff_sim[[5]]$obs)
  cal_results$E5_Max_diff <- max(runoff_sim[[5]]$sim) - max(runoff_sim[[5]]$obs)
  cal_results$E5_Dur_diff <- length(runoff_sim[[5]]$sim[runoff_sim[[5]]$sim > 0.03]) - length(runoff_sim[[5]]$obs[runoff_sim[[5]]$obs > 0.03])
  
  # Results for Event 6
  cal_results$E6_Sum_R <- sum(runoff_sim[[6]]$sim)
  cal_results$E6_Max_R <- max(runoff_sim[[6]]$sim) 
  cal_results$E6_Dur_R <- length(runoff_sim[[6]]$sim[runoff_sim[[6]]$sim > 0.03])
  cal_results$E6_Sum_diff <- sum(runoff_sim[[6]]$sim) - sum(runoff_sim[[6]]$obs)
  cal_results$E6_Max_diff <- max(runoff_sim[[6]]$sim) - max(runoff_sim[[6]]$obs)
  cal_results$E6_Dur_diff <- length(runoff_sim[[6]]$sim[runoff_sim[[6]]$sim > 0.03]) - length(runoff_sim[[6]]$obs[runoff_sim[[6]]$obs > 0.03])
  
  # Results for Event 7
  cal_results$E7_Sum_R <- sum(runoff_sim[[7]]$sim)
  cal_results$E7_Max_R <- max(runoff_sim[[7]]$sim) 
  cal_results$E7_Dur_R <- length(runoff_sim[[7]]$sim[runoff_sim[[7]]$sim > 0.03])
  cal_results$E7_Sum_diff <- sum(runoff_sim[[7]]$sim) - sum(runoff_sim[[7]]$obs)
  cal_results$E7_Max_diff <- max(runoff_sim[[7]]$sim) - max(runoff_sim[[7]]$obs)
  cal_results$E7_Dur_diff <- length(runoff_sim[[7]]$sim[runoff_sim[[7]]$sim > 0.03]) - length(runoff_sim[[7]]$obs[runoff_sim[[7]]$obs > 0.03])
  
  # Results for Event 8
  cal_results$E8_Sum_R <- sum(runoff_sim[[8]]$sim)
  cal_results$E8_Max_R <- max(runoff_sim[[8]]$sim) 
  cal_results$E8_Dur_R <- length(runoff_sim[[8]]$sim[runoff_sim[[8]]$sim > 0.03])
  cal_results$E8_Sum_diff <- sum(runoff_sim[[8]]$sim) - sum(runoff_sim[[8]]$obs)
  cal_results$E8_Max_diff <- max(runoff_sim[[8]]$sim) - max(runoff_sim[[8]]$obs)
  cal_results$E8_Dur_diff <- length(runoff_sim[[8]]$sim[runoff_sim[[8]]$sim > 0.03]) - length(runoff_sim[[8]]$obs[runoff_sim[[8]]$obs > 0.03])
  
    
  print(paste("Run",i,"of",l,"finished"),sep=" ")
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

{
########################################################################################################
#SENSETIVITY PLOT LID PARAMETERS ~ SUM RUNOFF
########################################################################################################

png(filename = "Sensitivity_Sum_Runoff.png",width=4200,height=2400,res=200)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, fill="blue", color="blue", ...) +
    geom_hline(yintercept = sum_obs, color = "red", size = 2)
  p
}

p1 <- ggpairs(cal_results,columns = c(2,12), lower = list(continuous = my_fn),title = "Soil Thickness ~ Sum Runoff")
p2 <- ggpairs(cal_results,columns = c(3,12), lower = list(continuous = my_fn),title = "Soil Porosity ~ Sum Runoff")
p3 <- ggpairs(cal_results,columns = c(4,12), lower = list(continuous = my_fn),title = "Soil Field Capacity ~ Sum Runoff")
p4 <- ggpairs(cal_results,columns = c(5,12), lower = list(continuous = my_fn),title = "Soil Wilting Point ~ Sum Runoff")
p5 <- ggpairs(cal_results,columns = c(6,12), lower = list(continuous = my_fn),title = "Soil Conductivity ~ Sum Runoff")
p6 <- ggpairs(cal_results,columns = c(7,12), lower = list(continuous = my_fn),title = "Soil Conductivity Slope ~ Sum Runoff")
p7 <- ggpairs(cal_results,columns = c(8,12), lower = list(continuous = my_fn),title = "Soil Suction Head ~ Sum Runoff")
p8 <- ggpairs(cal_results,columns = c(9,12), lower = list(continuous = my_fn),title = "Storage Thickness ~ Sum Runoff")
p9 <- ggpairs(cal_results,columns = c(10,12), lower = list(continuous = my_fn),title = "Storage Void Fraction ~ Sum Runoff")
p10 <- ggpairs(cal_results,columns = c(11,12), lower = list(continuous = my_fn),title = "Storage Roughness ~ Sum Runoff")



multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, cols=5)

dev.off()

########################################################################################################
#SENSETIVITY PLOT LID PARAMETERS ~ SUM WINTER RUNOFF
########################################################################################################

png(filename = "Sensitivity_Sum_Winter_Runoff.png",width=4200,height=2400,res=200)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, fill="blue", color="blue", ...) +
    geom_hline(yintercept = wsum_obs, color = "red", size = 2)
  p
}

p1 <- ggpairs(cal_results,columns = c(2,13), lower = list(continuous = my_fn),title = "Soil Thickness ~ Sum Winter Runoff")
p2 <- ggpairs(cal_results,columns = c(3,13), lower = list(continuous = my_fn),title = "Soil Porosity ~ Sum Winter Runoff")
p3 <- ggpairs(cal_results,columns = c(4,13), lower = list(continuous = my_fn),title = "Soil Field Capacity ~ Sum Winter Runoff")
p4 <- ggpairs(cal_results,columns = c(5,13), lower = list(continuous = my_fn),title = "Soil Wilting Point ~ Sum Winter Runoff")
p5 <- ggpairs(cal_results,columns = c(6,13), lower = list(continuous = my_fn),title = "Soil Conductivity ~ Sum Winter Runoff")
p6 <- ggpairs(cal_results,columns = c(7,13), lower = list(continuous = my_fn),title = "Soil Conductivity Slope ~ Sum Winter Runoff")
p7 <- ggpairs(cal_results,columns = c(8,13), lower = list(continuous = my_fn),title = "Soil Suction Head ~ Sum Winter Runoff")
p8 <- ggpairs(cal_results,columns = c(9,13), lower = list(continuous = my_fn),title = "Storage Thickness ~ Sum Winter Runoff")
p9 <- ggpairs(cal_results,columns = c(10,13), lower = list(continuous = my_fn),title = "Storage Void Fraction ~ Sum Winter Runoff")
p10 <- ggpairs(cal_results,columns = c(11,13), lower = list(continuous = my_fn),title = "Storage Roughness ~ Sum Winter Runoff")



multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, cols=5)

dev.off()

########################################################################################################
#SENSETIVITY PLOT LID PARAMETERS ~ SUM SUMMER RUNOFF
########################################################################################################

png(filename = "Sensitivity_Sum_Summer_Runoff.png",width=4200,height=2400,res=200)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, fill="blue", color="blue", ...) +
    geom_hline(yintercept = ssum_obs, color = "red", size = 2)
  p
}

p1 <- ggpairs(cal_results,columns = c(2,14), lower = list(continuous = my_fn),title = "Soil Thickness ~ Sum Summer Runoff")
p2 <- ggpairs(cal_results,columns = c(3,14), lower = list(continuous = my_fn),title = "Soil Porosity ~ Sum Summer Runoff")
p3 <- ggpairs(cal_results,columns = c(4,14), lower = list(continuous = my_fn),title = "Soil Field Capacity ~ Sum Summer Runoff")
p4 <- ggpairs(cal_results,columns = c(5,14), lower = list(continuous = my_fn),title = "Soil Wilting Point ~ Sum Summer Runoff")
p5 <- ggpairs(cal_results,columns = c(6,14), lower = list(continuous = my_fn),title = "Soil Conductivity ~ Sum Summer Runoff")
p6 <- ggpairs(cal_results,columns = c(7,14), lower = list(continuous = my_fn),title = "Soil Conductivity Slope ~ Sum Summer Runoff")
p7 <- ggpairs(cal_results,columns = c(8,14), lower = list(continuous = my_fn),title = "Soil Suction Head ~ Sum Summer Runoff")
p8 <- ggpairs(cal_results,columns = c(9,14), lower = list(continuous = my_fn),title = "Storage Thickness ~ Sum Summer Runoff")
p9 <- ggpairs(cal_results,columns = c(10,14), lower = list(continuous = my_fn),title = "Storage Void Fraction ~ Sum Summer Runoff")
p10 <- ggpairs(cal_results,columns = c(11,14), lower = list(continuous = my_fn),title = "Storage Roughness ~ Sum Summer Runoff")



multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, cols=5)

dev.off()

}

{
########################################################################################################
#SENSETIVITY PLOT LID PARAMETERS ~ MEAN PEAK RUNOFF
########################################################################################################

png(filename = "Sensitivity_Peak_Runoff.png",width=4200,height=2400,res=200)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, fill="blue", color="blue", ...) +
    geom_hline(yintercept = max_obs, color = "red", size = 2)
  p
}

p1 <- ggpairs(cal_results,columns = c(2,15), lower = list(continuous = my_fn),title = "Soil Thickness ~ Peak Runoff")
p2 <- ggpairs(cal_results,columns = c(3,15), lower = list(continuous = my_fn),title = "Soil Porosity ~ Peak Runoff")
p3 <- ggpairs(cal_results,columns = c(4,15), lower = list(continuous = my_fn),title = "Soil Field Capacity ~ Peak Runoff")
p4 <- ggpairs(cal_results,columns = c(5,15), lower = list(continuous = my_fn),title = "Soil Wilting Point ~ Peak Runoff")
p5 <- ggpairs(cal_results,columns = c(6,15), lower = list(continuous = my_fn),title = "Soil Conductivity ~ Peak Runoff")
p6 <- ggpairs(cal_results,columns = c(7,15), lower = list(continuous = my_fn),title = "Soil Conductivity Slope ~ Peak Runoff")
p7 <- ggpairs(cal_results,columns = c(8,15), lower = list(continuous = my_fn),title = "Soil Suction Head ~ Peak Runoff")
p8 <- ggpairs(cal_results,columns = c(9,15), lower = list(continuous = my_fn),title = "Storage Thickness ~ Peak Runoff")
p9 <- ggpairs(cal_results,columns = c(10,15), lower = list(continuous = my_fn),title = "Storage Void Fraction ~ Peak Runoff")
p10 <- ggpairs(cal_results,columns = c(11,15), lower = list(continuous = my_fn),title = "Storage Roughness ~ Peak Runoff")



multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, cols=5)

dev.off()

########################################################################################################
#SENSETIVITY PLOT LID PARAMETERS ~ MEAN PEAK WINTER RUNOFF
########################################################################################################

png(filename = "Sensitivity_Peak_Winter_Runoff.png",width=4200,height=2400,res=200)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, fill="blue", color="blue", ...) +
    geom_hline(yintercept = wmax_obs, color = "red", size = 2)
  p
}

p1 <- ggpairs(cal_results,columns = c(2,16), lower = list(continuous = my_fn),title = "Soil Thickness ~ Peak Winter Runoff")
p2 <- ggpairs(cal_results,columns = c(3,16), lower = list(continuous = my_fn),title = "Soil Porosity ~ Peak Winter Runoff")
p3 <- ggpairs(cal_results,columns = c(4,16), lower = list(continuous = my_fn),title = "Soil Field Capacity ~ Peak Winter Runoff")
p4 <- ggpairs(cal_results,columns = c(5,16), lower = list(continuous = my_fn),title = "Soil Wilting Point ~ Peak Winter Runoff")
p5 <- ggpairs(cal_results,columns = c(6,16), lower = list(continuous = my_fn),title = "Soil Conductivity ~ Peak Winter Runoff")
p6 <- ggpairs(cal_results,columns = c(7,16), lower = list(continuous = my_fn),title = "Soil Conductivity Slope ~ Peak Winter Runoff")
p7 <- ggpairs(cal_results,columns = c(8,16), lower = list(continuous = my_fn),title = "Soil Suction Head ~ Peak Winter Runoff")
p8 <- ggpairs(cal_results,columns = c(9,16), lower = list(continuous = my_fn),title = "Storage Thickness ~ Peak Winter Runoff")
p9 <- ggpairs(cal_results,columns = c(10,16), lower = list(continuous = my_fn),title = "Storage Void Fraction ~ Peak Winter Runoff")
p10 <- ggpairs(cal_results,columns = c(11,16), lower = list(continuous = my_fn),title = "Storage Roughness ~ Peak Winter Runoff")



multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, cols=5)

dev.off()

########################################################################################################
#SENSETIVITY PLOT LID PARAMETERS ~ MEAN PEAK SUMMER RUNOFF
########################################################################################################

png(filename = "Sensitivity_Peak_Summer_Runoff.png",width=4200,height=2400,res=200)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, fill="blue", color="blue", ...) +
    geom_hline(yintercept = smax_obs, color = "red", size = 2)
  p
}

p1 <- ggpairs(cal_results,columns = c(2,17), lower = list(continuous = my_fn),title = "Soil Thickness ~ Peak Summer Runoff")
p2 <- ggpairs(cal_results,columns = c(3,17), lower = list(continuous = my_fn),title = "Soil Porosity ~ Peak Summer Runoff")
p3 <- ggpairs(cal_results,columns = c(4,17), lower = list(continuous = my_fn),title = "Soil Field Capacity ~ Peak Summer Runoff")
p4 <- ggpairs(cal_results,columns = c(5,17), lower = list(continuous = my_fn),title = "Soil Wilting Point ~ Peak Summer Runoff")
p5 <- ggpairs(cal_results,columns = c(6,17), lower = list(continuous = my_fn),title = "Soil Conductivity ~ Peak Summer Runoff")
p6 <- ggpairs(cal_results,columns = c(7,17), lower = list(continuous = my_fn),title = "Soil Conductivity Slope ~ Peak Summer Runoff")
p7 <- ggpairs(cal_results,columns = c(8,17), lower = list(continuous = my_fn),title = "Soil Suction Head ~ Peak Summer Runoff")
p8 <- ggpairs(cal_results,columns = c(9,17), lower = list(continuous = my_fn),title = "Storage Thickness ~ Peak Summer Runoff")
p9 <- ggpairs(cal_results,columns = c(10,17), lower = list(continuous = my_fn),title = "Storage Void Fraction ~ Peak Summer Runoff")
p10 <- ggpairs(cal_results,columns = c(11,17), lower = list(continuous = my_fn),title = "Storage Roughness ~ Peak Summer Runoff")



multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, cols=5)

dev.off()
}

{
########################################################################################################
#SENSETIVITY PLOT LID PARAMETERS ~ MEAN RUNOFF DURATION
########################################################################################################

png(filename = "Sensitivity_Runoff_Duration.png",width=4200,height=2400,res=200)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, fill="blue", color="blue", ...) +
    geom_hline(yintercept = dur_obs, color = "red", size = 2)
  p
}

p1 <- ggpairs(cal_results,columns = c(2,18), lower = list(continuous = my_fn),title = "Soil Thickness ~ Runoff Duration")
p2 <- ggpairs(cal_results,columns = c(3,18), lower = list(continuous = my_fn),title = "Soil Porosity ~ Runoff Duration")
p3 <- ggpairs(cal_results,columns = c(4,18), lower = list(continuous = my_fn),title = "Soil Field Capacity ~ Runoff Duration")
p4 <- ggpairs(cal_results,columns = c(5,18), lower = list(continuous = my_fn),title = "Soil Wilting Point ~ Runoff Duration")
p5 <- ggpairs(cal_results,columns = c(6,18), lower = list(continuous = my_fn),title = "Soil Conductivity ~ Runoff Duration")
p6 <- ggpairs(cal_results,columns = c(7,18), lower = list(continuous = my_fn),title = "Soil Conductivity Slope ~ Runoff Duration")
p7 <- ggpairs(cal_results,columns = c(8,18), lower = list(continuous = my_fn),title = "Soil Suction Head ~ Runoff Duration")
p8 <- ggpairs(cal_results,columns = c(9,18), lower = list(continuous = my_fn),title = "Storage Thickness ~ Runoff Duration")
p9 <- ggpairs(cal_results,columns = c(10,18), lower = list(continuous = my_fn),title = "Storage Void Fraction ~ Runoff Duration")
p10 <- ggpairs(cal_results,columns = c(11,18), lower = list(continuous = my_fn),title = "Storage Roughness ~ Runoff Duration")



multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, cols=5)

dev.off()

########################################################################################################
#SENSETIVITY PLOT LID PARAMETERS ~ MEAN WINTER RUNOFF DURATION
########################################################################################################

png(filename = "Sensitivity_Winter_Runoff_Duration.png",width=4200,height=2400,res=200)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, fill="blue", color="blue", ...) +
    geom_hline(yintercept = wdur_obs, color = "red", size = 2)
  p
}

p1 <- ggpairs(cal_results,columns = c(2,19), lower = list(continuous = my_fn),title = "Soil Thickness ~ Winter Runoff Duration")
p2 <- ggpairs(cal_results,columns = c(3,19), lower = list(continuous = my_fn),title = "Soil Porosity ~ Winter Runoff Duration")
p3 <- ggpairs(cal_results,columns = c(4,19), lower = list(continuous = my_fn),title = "Soil Field Capacity ~ Winter Runoff Duration")
p4 <- ggpairs(cal_results,columns = c(5,19), lower = list(continuous = my_fn),title = "Soil Wilting Point ~ Winter Runoff Duration")
p5 <- ggpairs(cal_results,columns = c(6,19), lower = list(continuous = my_fn),title = "Soil Conductivity ~ Winter Runoff Duration")
p6 <- ggpairs(cal_results,columns = c(7,19), lower = list(continuous = my_fn),title = "Soil Conductivity Slope ~ Winter Runoff Duration")
p7 <- ggpairs(cal_results,columns = c(8,19), lower = list(continuous = my_fn),title = "Soil Suction Head ~ Winter Runoff Duration")
p8 <- ggpairs(cal_results,columns = c(9,19), lower = list(continuous = my_fn),title = "Storage Thickness ~ Winter Runoff Duration")
p9 <- ggpairs(cal_results,columns = c(10,19), lower = list(continuous = my_fn),title = "Storage Void Fraction ~ Winter Runoff Duration")
p10 <- ggpairs(cal_results,columns = c(11,19), lower = list(continuous = my_fn),title = "Storage Roughness ~ Winter Runoff Duration")



multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, cols=5)

dev.off()

########################################################################################################
#SENSETIVITY PLOT LID PARAMETERS ~ MEAN SUMMER RUNOFF DURATION
########################################################################################################

png(filename = "Sensitivity_Summer_Runoff_Duration.png",width=4200,height=2400,res=200)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, fill="blue", color="blue", ...) +
    geom_hline(yintercept = sdur_obs, color = "red", size = 2)
  p
}

p1 <- ggpairs(cal_results,columns = c(2,20), lower = list(continuous = my_fn),title = "Soil Thickness ~ Summer Runoff Duration")
p2 <- ggpairs(cal_results,columns = c(3,20), lower = list(continuous = my_fn),title = "Soil Porosity ~ Summer Runoff Duration")
p3 <- ggpairs(cal_results,columns = c(4,20), lower = list(continuous = my_fn),title = "Soil Field Capacity ~ Summer Runoff Duration")
p4 <- ggpairs(cal_results,columns = c(5,20), lower = list(continuous = my_fn),title = "Soil Wilting Point ~ Summer Runoff Duration")
p5 <- ggpairs(cal_results,columns = c(6,20), lower = list(continuous = my_fn),title = "Soil Conductivity ~ Summer Runoff Duration")
p6 <- ggpairs(cal_results,columns = c(7,20), lower = list(continuous = my_fn),title = "Soil Conductivity Slope ~ Summer Runoff Duration")
p7 <- ggpairs(cal_results,columns = c(8,20), lower = list(continuous = my_fn),title = "Soil Suction Head ~ Summer Runoff Duration")
p8 <- ggpairs(cal_results,columns = c(9,20), lower = list(continuous = my_fn),title = "Storage Thickness ~ Summer Runoff Duration")
p9 <- ggpairs(cal_results,columns = c(10,20), lower = list(continuous = my_fn),title = "Storage Void Fraction ~ Summer Runoff Duration")
p10 <- ggpairs(cal_results,columns = c(11,20), lower = list(continuous = my_fn),title = "Storage Roughness ~ Summer Runoff Duration")



multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, cols=5)

dev.off()

########################################################################################################
#SENSETIVITY PLOT LID PARAMETERS ~ MEAN SUMMER RUNOFF DURATION
########################################################################################################

png(filename = "Sensitivity_Sum_Runoff_Difference.png",width=4200,height=2400,res=200)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, fill="blue", color="blue", ...) +
    geom_hline(yintercept = 0, color = "red", size = 2)
  p
}

p1 <- ggpairs(cal_results,columns = c(2,21), lower = list(continuous = my_fn),title = "Soil Thickness ~ Summer Runoff Duration")
p2 <- ggpairs(cal_results,columns = c(3,21), lower = list(continuous = my_fn),title = "Soil Porosity ~ Summer Runoff Duration")
p3 <- ggpairs(cal_results,columns = c(4,21), lower = list(continuous = my_fn),title = "Soil Field Capacity ~ Summer Runoff Duration")
p4 <- ggpairs(cal_results,columns = c(5,21), lower = list(continuous = my_fn),title = "Soil Wilting Point ~ Summer Runoff Duration")
p5 <- ggpairs(cal_results,columns = c(6,21), lower = list(continuous = my_fn),title = "Soil Conductivity ~ Summer Runoff Duration")
p6 <- ggpairs(cal_results,columns = c(7,21), lower = list(continuous = my_fn),title = "Soil Conductivity Slope ~ Summer Runoff Duration")
p7 <- ggpairs(cal_results,columns = c(8,21), lower = list(continuous = my_fn),title = "Soil Suction Head ~ Summer Runoff Duration")
p8 <- ggpairs(cal_results,columns = c(9,21), lower = list(continuous = my_fn),title = "Storage Thickness ~ Summer Runoff Duration")
p9 <- ggpairs(cal_results,columns = c(10,21), lower = list(continuous = my_fn),title = "Storage Void Fraction ~ Summer Runoff Duration")
p10 <- ggpairs(cal_results,columns = c(11,21), lower = list(continuous = my_fn),title = "Storage Roughness ~ Summer Runoff Duration")



multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, cols=5)

dev.off()
}

### ranking calibration results ###

# changing the difference from + and - values to + values
cal_results$Sum_diff_gesr <- sqrt((cal_results$Sum_diff_ges)^2)
cal_results$Sum_diff_wr <- sqrt((cal_results$Sum_diff_w)^2)
cal_results$Sum_diff_sr <- sqrt((cal_results$Sum_diff_s)^2)

cal_results$Max_diff_gesr <- sqrt((cal_results$Max_diff_ges)^2)
cal_results$Max_diff_wr <- sqrt((cal_results$Max_diff_w)^2)
cal_results$Max_diff_sr <- sqrt((cal_results$Max_diff_s)^2)

cal_results$RankSum <- rank(cal_results$Sum_diff_gesr)
cal_results$RankSumw <- rank(cal_results$Sum_diff_wr)
cal_results$RankSums <- rank(cal_results$Sum_diff_sr)

cal_results$RankMax <- rank(cal_results$Max_diff_gesr)
cal_results$RankMaxw <- rank(cal_results$Max_diff_wr)
cal_results$RankMaxs <- rank(cal_results$Max_diff_sr)

cal_results$Rankall <- rowMeans(cal_results[ ,c("RankSum", "RankSumw", "RankSums", "RankMax", "RankMaxw", "RankMaxs")])
best_results <- head(cal_results[order(cal_results$Rankall),],5)
best_rankall_results <- best_results
write.csv2(x = best_rankall_results,file = "best_rankall_results.csv")

cal_results$Rankw <- rowMeans(cal_results[ ,c("RankSumw", "RankMaxw")])
best_results <- head(cal_results[order(cal_results$Rankw),],5)
best_rankw_results <- best_results
write.csv2(x = best_rankw_results,file = "best_rankw_results.csv")

cal_results$Ranks <- rowMeans(cal_results[ ,c("RankSums", "RankMaxs")])
best_results <- head(cal_results[order(cal_results$Ranks),],5)
best_ranks_results <- best_results
write.csv2(x = best_ranks_results,file = "best_ranks_results.csv")

### Finding the best results

best_results <- cal_results[cal_results$Sum_diff_ges <= 10 & cal_results$Sum_diff_ges >= -10, ]
best_results <- best_results[best_results$Sum_diff_s <= 2 & best_results$Sum_diff_s >= -2, ]
bestsum_results <- best_results
write.csv2(x = bestsum_results,file = "bestsum_results.csv")

best_results <- cal_results[cal_results$Max_diff_w <= 0.02 & cal_results$Max_diff_w>= -0.02, ]
best_results <- best_results[best_results$Max_diff_s <= 0.01 & best_results$Max_diff_s >= -0.01, ]
bestpeak_results <- best_results
write.csv2(x = bestpeak_results,file = "bestpeak_results.csv")

best_results <- cal_results[cal_results$Dur_diff_w <= 10 & cal_results$Dur_diff_w>= -10, ]
best_results <- best_results[best_results$Dur_diff_s <= 10 & best_results$Dur_diff_s>= -10, ]
bestdur_results <- best_results
write.csv2(x = bestdur_results,file = "bestdur_results.csv")

##########################################
### Plotting best results for overview ###
##########################################

for (i in seq_along(best_results[,1])) {

  input$lid_controls$Par1[3] <- best_results$Soil_Thickness[i] # Soil Thickness
  input$lid_controls$Par2[3] <- best_results$Porosity[i] # Porosity
  input$lid_controls$Par3[3] <- best_results$Field_Capacity[i] # Field Capacity
  input$lid_controls$Par4[3] <- best_results$Wilting_Point[i] # Wilting Point
  input$lid_controls$Par5[3] <- best_results$Conductivity[i] # Conductivity
  input$lid_controls$Par6[3] <- best_results$Conductivity_Slope[i] # Conductivity Slope
  input$lid_controls$Par7[3] <- best_results$Suction_Head[i] # Suction Head
  input$lid_controls$Par1[4] <- best_results$Stor_Thick[i] # Storage Thickness
  input$lid_controls$Par2[4] <- best_results$Void_Fraction[i] # Void Fraction
  input$lid_controls$Par3[4] <- best_results$Roughness[i] # Roughness
    
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
  for (j in seq_along(cal_rain_events$Event)) {
    
    runoff_sim[[j]] <- data.frame(matrix(
      data = NA,
      ncol = 1,
      nrow = length(seq.POSIXt(cal_rain_events$tBeg[j], (cal_rain_events$tEnd[j] + 300*144), by="5 min"))))
    
    colnames(runoff_sim[[j]])<-"sim" 
    
    runoff_sim[[j]]$DateTime <- seq.POSIXt(cal_rain_events$tBeg[j], (cal_rain_events$tEnd[j] + 300*144), by="5 min")
    
    runoff_sim[[j]]$sim <- (as.numeric(coredata(results$system_variable$total_runoff[which(index(results$system_variable$total_runoff) == cal_rain_events$tBeg[j]) : which(index(results$system_variable$total_runoff) == (cal_rain_events$tEnd[j] + 300*144))])))*300/101
    
    runoff_sim[[j]] <- semi_join(runoff_sim[[j]], runoff[which(runoff$DateTime == cal_rain_events$tBeg[j]) : which(runoff$DateTime == (cal_rain_events$tEnd[j] + 300*144)), ], by="DateTime")
    
    runoff_sim[[j]]$GD <- runoff$GD[which(runoff$DateTime == cal_rain_events$tBeg[j]) : which(runoff$DateTime == (cal_rain_events$tEnd[j] + 300*144))]
    
    runoff_sim[[j]]$rain <- runoff$NSB[which(runoff$DateTime == cal_rain_events$tBeg[j]) : which(runoff$DateTime == (cal_rain_events$tEnd[j] + 300*144))]
  
    }

pdf_file <- preparePdf(file.path(tempdir(), paste("best_ranks",i,".pdf",sep = "")))

# Plot something

for (l in seq_along(cal_rain_events$tBeg)) {
 
  ### GGPLOT ###

test_1 <- gather(data = runoff_sim[[l]], key = "attribute" , value = "value", c(-DateTime, -rain ))  
  
g1 <- ggplot(runoff_sim[[l]],aes(x=DateTime)) +
  geom_bar(aes(y=rain), stat = "identity",fill = "dodgerblue",color = "blue") +
  ylim(c(10,0)) +
  ylab("Niederschlag [mm/5min.]") +
  ggtitle(paste("Simulationsergebnis Event ", l, sep = "")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
  
  
g2 <- ggplot(data = test_1, aes(DateTime, value, color = attribute)) +
  geom_line(size = 0.7, alpha = 0.7) +
  xlab("Zeit") +
  ylab("Abfluss [mm/5min.]") +
  ylim(c(0,0.5)) +
  theme(legend.position = "bottom") +
  scale_color_manual(name = "", 
    labels = c("Messung", "Modell" ),
                       values = c("black","red")) +
  annotate("text",
    x = (tail(runoff_sim[[l]]$DateTime,1))-length(runoff_sim[[l]]$DateTime)*300/8, y = 0.40, label = paste(
      "Event ", l,
      "\n Beginn: ", head(runoff_sim[[l]]$DateTime,1), 
      "\n Ende: ", tail(runoff_sim[[l]]$DateTime,1),
      "\n Eventdauer: ", round(length(seq_along(head(runoff_sim[[l]]$DateTime,1):tail(runoff_sim[[l]]$DateTime, 1)))/3600,0), "h",
      "\n Niederschlagssumme: ",round(sum(runoff_sim[[l]]$rain),digits = 1), "mm",
      "\n Gemessene Abflusssumme: ", round(sum(runoff_sim[[l]]$GD),digits = 1),"mm",
      "\n Simulierte Abflusssumme: ", round(sum(runoff_sim[[l]]$sim),digits = 1),"mm",
      "\n Gemessene Abflussspitze: ", round(max(runoff_sim[[l]]$GD),digits = 2),"mm/5min.",
      "\n Simulierte Abflussspitze: ", round(max(runoff_sim[[l]]$sim),digits = 2),"mm/5min.",
      "\n Gemessene Abflussdauer: ", round(length(runoff_sim[[l]]$GD[runoff_sim[[l]]$GD > 0.03])/12,digits = 1),"h",
      "\n Simulierte Abflussdauer: ", round(length(runoff_sim[[l]]$sim[runoff_sim[[l]]$sim > 0.03])/12,digits = 1),"h"),
      color = "black", cex = 3)
  
grid.arrange(g1,g2,nrow = 2, heights = c(1,3))

}

# Open PDF file in viewer
finishAndShowPdf(pdf_file)

}

####################################################
### Plotting best results validation rain events ###
####################################################

best_results <- best_rankall_results[2, ]
  
  input$lid_controls$Par1[3] <- best_results$Soil_Thickness # Soil Thickness
  input$lid_controls$Par2[3] <- best_results$Porosity # Porosity
  input$lid_controls$Par3[3] <- best_results$Field_Capacity # Field Capacity
  input$lid_controls$Par4[3] <- best_results$Wilting_Point # Wilting Point
  input$lid_controls$Par5[3] <- best_results$Conductivity # Conductivity
  input$lid_controls$Par6[3] <- best_results$Conductivity_Slope # Conductivity Slope
  input$lid_controls$Par7[3] <- best_results$Suction_Head # Suction Head
  input$lid_controls$Par1[4] <- best_results$Stor_Thick # Storage Thickness
  input$lid_controls$Par2[4] <- best_results$Void_Fraction # Void Fraction
  input$lid_controls$Par3[4] <- best_results$Roughness # Roughness
  
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
  for (j in seq_along(val_rain_events$Event)) {
    
    runoff_sim[[j]] <- data.frame(matrix(
      data = NA,
      ncol = 1,
      nrow = length(seq.POSIXt(val_rain_events$tBeg[j], (val_rain_events$tEnd[j] + 300*144), by="5 min"))))
    
    colnames(runoff_sim[[j]])<-"sim" 
    
    runoff_sim[[j]]$DateTime <- seq.POSIXt(val_rain_events$tBeg[j], (val_rain_events$tEnd[j] + 300*144), by="5 min")
    
    runoff_sim[[j]]$sim <- (as.numeric(coredata(results$system_variable$total_runoff[which(index(results$system_variable$total_runoff) == val_rain_events$tBeg[j]) : which(index(results$system_variable$total_runoff) == (val_rain_events$tEnd[j] + 300*144))])))*300/101
    
    runoff_sim[[j]] <- semi_join(runoff_sim[[j]], runoff[which(runoff$DateTime == val_rain_events$tBeg[j]) : which(runoff$DateTime == (val_rain_events$tEnd[j] + 300*144)), ], by="DateTime")
    
    runoff_sim[[j]]$GD <- runoff$GD[which(runoff$DateTime == val_rain_events$tBeg[j]) : which(runoff$DateTime == (val_rain_events$tEnd[j] + 300*144))]
    
    runoff_sim[[j]]$rain <- runoff$NSB[which(runoff$DateTime == val_rain_events$tBeg[j]) : which(runoff$DateTime == (val_rain_events$tEnd[j] + 300*144))]
    
  } 
  
  pdf_file <- preparePdf(file.path(tempdir(), "validation_result.pdf"))
  
  # Plot something
  
  for (l in seq_along(val_rain_events$tBeg)) {
    
    ### GGPLOT ###
    
    test_1 <- gather(data = runoff_sim[[l]], key = "attribute" , value = "value", c(-DateTime, -rain ))  
    
    g1 <- ggplot(runoff_sim[[l]],aes(x=DateTime)) +
      geom_bar(aes(y=rain), stat = "identity",fill = "dodgerblue",color = "blue") +
      ylim(c(10,0)) +
      ylab("Niederschlag [mm/5min.]") +
      ggtitle(paste("Simulationsergebnis Event ", l, sep = "")) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    
    
    g2 <- ggplot(data = test_1, aes(DateTime, value, color = attribute)) +
      geom_line(size = 0.7, alpha = 0.7) +
      xlab("Zeit") +
      ylab("Abfluss [mm/5min.]") +
      ylim(c(0,0.5)) +
      theme(legend.position = "bottom") +
      scale_color_manual(name = "", 
                         labels = c("Messung", "Modell" ),
                         values = c("black","red")) +
      annotate("text",
               x = (tail(runoff_sim[[l]]$DateTime,1))-length(runoff_sim[[l]]$DateTime)*300/8, y = 0.40, label = paste(
                 "Event ", l,
                 "\n Beginn: ", head(runoff_sim[[l]]$DateTime,1), 
                 "\n Ende: ", tail(runoff_sim[[l]]$DateTime,1),
                 "\n Eventdauer: ", round(length(seq_along(head(runoff_sim[[l]]$DateTime,1):tail(runoff_sim[[l]]$DateTime, 1)))/3600,0), "h",
                 "\n Niederschlagssumme: ",round(sum(runoff_sim[[l]]$rain),digits = 1), "mm",
                 "\n Gemessene Abflusssumme: ", round(sum(runoff_sim[[l]]$GD),digits = 1),"mm",
                 "\n Simulierte Abflusssumme: ", round(sum(runoff_sim[[l]]$sim),digits = 1),"mm",
                 "\n Gemessene Abflussspitze: ", round(max(runoff_sim[[l]]$GD),digits = 2),"mm/5min.",
                 "\n Simulierte Abflussspitze: ", round(max(runoff_sim[[l]]$sim),digits = 2),"mm/5min.",
                 "\n Gemessene Abflussdauer: ", round(length(runoff_sim[[l]]$GD[runoff_sim[[l]]$GD > 0.03])/12,digits = 1),"h",
                 "\n Simulierte Abflussdauer: ", round(length(runoff_sim[[l]]$sim[runoff_sim[[l]]$sim > 0.03])/12,digits = 1),"h"),
               color = "black", cex = 3)
    
    grid.arrange(g1,g2,nrow = 2, heights = c(1,3))
    
  }
  
  # Open PDF file in viewer
  finishAndShowPdf(pdf_file)
  

