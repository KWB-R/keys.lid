library(swmmr)
library(gdata)
library(xts)
library(ggplot2)
library(GGally)
library(ggpubr)
setwd("Y:/WWT_Department/Projects/KEYS/Data-Work packages/WP1_sponge_city_elements/_DataAnalysis/LIDmodels/greenRoof/BWSTI_Beijing")

# read swmm input file
input <- read_inp(x = "beijing2014_2016.inp")
summary(input)

# length of the loop
l <- 1000

# data frame to write in parameter values 
cal_names<-c("Run", "Soil_Thickness", "Porosity", "Field_Capacity", "Wilting_Point", "Conductivity", "Conductivity_Slope", "Suction_Head", # Soil Parameters
             "Drain_Thickness", "Void_Fraction", "Roughness", # Drainage Mat Parameters 
             "Sum_R"# Total Runoff
             )

cal_results <- data.frame(matrix(
  data = NA,
  ncol = length(cal_names),
  nrow = l))

cal_col <- colnames(cal_results)

cal_results <- gdata::rename.vars(cal_results,cal_col, cal_names)


# BWSTI Beijing events period
seq <- seq.POSIXt(ISOdate(2014,6,10,00,10,tz="UTC"),
                  ISOdate(2016,8,18,11,30,tz="UTC"),
                  by="5 min")

# runoff results of the green roof
for (i in 1:l){
  
  # set parameters for simulation
  input$lid_controls$Par1[3] <- runif(n = 1, min = 80, max = 120) # Soil Thickness
  input$lid_controls$Par2[3] <- runif(n = 1, min = 0.45, max = 0.65) # Porosity
  input$lid_controls$Par3[3] <- runif(n = 1, min = 0.35, max = 0.55) # Field Capacity
  input$lid_controls$Par4[3] <- runif(n = 1, min = 0.05, max = 0.20) # Wilting Point
  input$lid_controls$Par5[3] <- runif(n = 1, min = 50, max = 350) # Conductivity
  input$lid_controls$Par6[3] <- runif(n = 1, min = 30, max = 55) # Conductivity Slope
  input$lid_controls$Par7[3] <- runif(n = 1, min = 50, max = 100) # Suction Head
  input$lid_controls$Par1[4] <- runif(n = 1, min = 10, max = 50) # Storage Thickness
  input$lid_controls$Par2[4] <- runif(n = 1, min = 0.3, max = 0.5) # Void Fraction
  input$lid_controls$Par3[4] <- runif(n = 1, min = 0.01, max = 0.03) # Roughness
  
  # write values in the dataframe
  cal_results$Run[i] <- i
  
  cal_results$Soil_Thickness[i] <- input$lid_controls$Par1[3]
  cal_results$Porosity[i] <- input$lid_controls$Par2[3]
  cal_results$Field_Capacity[i] <- input$lid_controls$Par3[3] 
  cal_results$Wilting_Point[i] <- input$lid_controls$Par4[3]
  cal_results$Conductivity[i] <- input$lid_controls$Par5[3]
  cal_results$Conductivity_Slope[i] <- input$lid_controls$Par6[3]
  cal_results$Suction_Head[i] <- input$lid_controls$Par7[3]
  cal_results$Drain_Thickness[i] <- input$lid_controls$Par1[4]
  cal_results$Void_Fraction[i] <- input$lid_controls$Par2[4]
  cal_results$Roughness[i] <- input$lid_controls$Par3[4]
  
  # save the changed input file
  write_inp(input,"Validation_Beijing.inp") 
  
  # run swimm with changed input file
  files <- run_swmm(inp = "Validation_Beijing.inp") 
  
  # read out results for itype 3 = system and vIndex 4 = runoff 
  results <- read_out(files$out, iType = 3, vIndex = c(4)) 
  
  # change timezone to UTC
  tzone(results$system_variable$total_runoff) <- "UTC"
  
  # write model runoff in data frame
  runoff_sim <- list()

  # extract model runoff of entire simulation period
  runoff_sim <- data.frame(matrix(
    data = NA,
    ncol = 1,
    nrow = length(seq)))
  
  colnames(runoff_sim)<-"sim" 
  
  runoff_sim$DateTime <- seq
  
  runoff_sim$sim <- (as.numeric(coredata(results$system_variable$total_runoff)))*300/65
  
  ### calculate sum of Runoff

  sum_sim <- sum(runoff_sim$sim) 
  
  cal_results$Sum_R[i] <- sum_sim
 
  
  print(paste("Run",i,"of",l,"finished"),sep=" ")
}


# Sensitivity Analysis Plot Green Roof Parameters ~ Total Runoff Volume
p1 <- ggplot(cal_results, aes(Soil_Thickness, Sum_R)) + geom_point(alpha=0.8, color='#0078d2') + ggtitle("Substrate Thickness") + theme_pubr() +
  geom_smooth(method=lm, fill="red", color="red", alpha=0.3, size = 1.5) + xlab("Thickness [mm]") + ylab(expression(paste("Total Runoff [  ",L/m^2," ]"))) + 
  stat_cor(label.y = 600) + stat_regline_equation(label.y = 620) + theme(plot.title = element_text(hjust=0.5, size=13))

p2 <- ggplot(cal_results, aes(Porosity, Sum_R)) + geom_point(alpha=0.8, color='#0078d2') + ggtitle("Substrate Porosity") + theme_pubr() +
  geom_smooth(method=lm, fill="red", color="red", alpha=0.3, size = 1.5) + xlab("Porosity") + ylab(expression(paste("Total Runoff [  ",L/m^2," ]"))) + 
  stat_cor(label.y = 600) + stat_regline_equation(label.y = 620) + theme(plot.title = element_text(hjust=0.5, size=13))

p3 <- ggplot(cal_results, aes(Field_Capacity, Sum_R)) + geom_point(alpha=0.8, color='#0078d2') + ggtitle("Substrate Field Capacity") + theme_pubr() +
  geom_smooth(method=lm, fill="red", color="red", alpha=0.3, size = 1.5)  + xlab("Field Capacity") + ylab(expression(paste("Total Runoff [  ",L/m^2," ]"))) + 
  stat_cor(label.y = 600) + stat_regline_equation(label.y = 620) + theme(plot.title = element_text(hjust=0.5, size=13))

p4 <- ggplot(cal_results, aes(Wilting_Point, Sum_R)) + geom_point(alpha=0.8, color='#0078d2') + ggtitle("Substrate Wilting Point") + theme_pubr() +
  geom_smooth(method=lm, fill="red", color="red", alpha=0.3, size = 1.5)  + xlab("Wilting Point") + ylab(expression(paste("Total Runoff [  ",L/m^2," ]"))) + 
  stat_cor(label.y = 600) + stat_regline_equation(label.y = 620) + theme(plot.title = element_text(hjust=0.5, size=13))

p5 <- ggplot(cal_results, aes(Conductivity, Sum_R)) + geom_point(alpha=0.8, color='#0078d2') + ggtitle("Substrate Conductivity") + theme_pubr() +
  geom_smooth(method=lm, fill="red", color="red", alpha=0.3, size = 1.5) + xlab("Conductivity [mm/h]") + ylab(expression(paste("Total Runoff [  ",L/m^2," ]"))) + 
  stat_cor(label.y = 600) + stat_regline_equation(label.y = 620) + theme(plot.title = element_text(hjust=0.5, size=13))

p6 <- ggplot(cal_results, aes(Conductivity_Slope, Sum_R)) + geom_point(alpha=0.8, color='#0078d2') + ggtitle("Substrate Conductivity Slope") + theme_pubr() +
  geom_smooth(method=lm, fill="red", color="red", alpha=0.3, size = 1.5)  + xlab("Conductivity Slope") + ylab(expression(paste("Total Runoff [  ",L/m^2," ]"))) + 
  stat_cor(label.y = 600) + stat_regline_equation(label.y = 620) + theme(plot.title = element_text(hjust=0.5, size=13))

p7 <- ggplot(cal_results, aes(Suction_Head, Sum_R)) + geom_point(alpha=0.8, color='#0078d2') + ggtitle("Substrate Suction Head") + theme_pubr() +
  geom_smooth(method=lm, fill="red", color="red", alpha=0.3, size = 1.5)  + xlab("Suction Head [mm]") + ylab(expression(paste("Total Runoff [  ",L/m^2," ]"))) + 
  stat_cor(label.y = 600) + stat_regline_equation(label.y = 620) + theme(plot.title = element_text(hjust=0.5, size=13))

p8 <- ggplot(cal_results, aes(Drain_Thickness, Sum_R)) + geom_point(alpha=0.8, color='#0078d2') + ggtitle("Drainage Thickness") + theme_pubr() +
  geom_smooth(method=lm, fill="red", color="red", alpha=0.3, size = 1.5)  + xlab("Thickness [mm]") + ylab(expression(paste("Total Runoff [  ",L/m^2," ]"))) + 
  stat_cor(label.y = 600) + stat_regline_equation(label.y = 620) + theme(plot.title = element_text(hjust=0.5, size=13))

p9 <- ggplot(cal_results, aes(Void_Fraction, Sum_R)) + geom_point(alpha=0.8, color='#0078d2') + ggtitle("Drainage Void Fraction") + theme_pubr() +
  geom_smooth(method=lm, fill="red", color="red", alpha=0.3, size = 1.5)  + xlab("Void fraction") + ylab(expression(paste("Total Runoff [  ",L/m^2," ]"))) + 
  stat_cor(label.y = 600) + stat_regline_equation(label.y = 620) + theme(plot.title = element_text(hjust=0.5, size=13))

p10 <- ggplot(cal_results, aes(Roughness, Sum_R)) + geom_point(alpha=0.8, color='#0078d2') + ggtitle("Drainage Roughness") + theme_pubr() +
  geom_smooth(method=lm, fill="red", color="red", alpha=0.3, size = 1.5) + xlab("Roughness (n)") + ylab(expression(paste("Total Runoff [  ", L/m^2," ]"))) + 
  stat_cor(label.y = 600) + stat_regline_equation(label.y = 620) + theme(plot.title = element_text(hjust=0.5, size=13))

fig1 <- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, ncol = 5, nrow = 2)
fig2 <- annotate_figure(fig1, top = text_grob("Sensitivity Analysis - Beijing (BWSTI, 2014-2016)", face = "bold", size = 15))
print(fig2)