library(swmmr)
library(gdata)
library(xts)
library(ggplot2)
library(GGally)
library(ggpubr)
setwd("C:/Users/jaeho/Downloads/wp1/_DataAnalysis/LIDmodels/greenRoof/BWSTI_Zone4")
# read swmm input file
input <- read_inp(x = "BWSTI_Zone4.inp")
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
seq <- seq.POSIXt(ISOdate(2014,4,6,00,05,tz="UTC"),
                  ISOdate(2019,10,15,21,00,tz="UTC"),
                  by="5 min")


# Initial simulated total Runoff of each zone 2014-2016(BWSTI)
Sum_Obs <- 681.6 # Zone 1
Sum_Obs <- 806.0 # Zone 2
Sum_Obs <- 288.4 # Zone 3
Sum_Obs <- 1433.4 # Zone 4
Sum_Obs <- 3864 # Zone 5

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
p11 <- ggplot(cal_results, aes(Soil_Thickness, Sum_R)) + geom_point(alpha=0.8, color='#0078d2') + ggtitle("Substrate Thickness") + theme_pubr() +
  geom_smooth(method=lm, fill="red", color="red", alpha=0.3, size = 1.5) + geom_hline(yintercept = Sum_Obs, color = "black", size = 1.4) + 
  xlab("Thickness [mm]") + ylab(expression(paste("Total Runoff [  ",L/m^2," ]"))) + theme(plot.title = element_text(hjust=0.5, size=13))

p12 <- ggplot(cal_results, aes(Porosity, Sum_R)) + geom_point(alpha=0.8, color='#0078d2') + ggtitle("Substrate Porosity") + theme_pubr() +
  geom_smooth(method=lm, fill="red", color="red", alpha=0.3) + geom_hline(yintercept = Sum_Obs, color = "black", size = 1.4) + 
  xlab("Porosity") + ylab(expression(paste("Total Runoff [  ",L/m^2," ]"))) + theme(plot.title = element_text(hjust=0.5, size=13))

p13 <- ggplot(cal_results, aes(Field_Capacity, Sum_R)) + geom_point(alpha=0.8, color='#0078d2') + ggtitle("Substrate Field Capacity") + theme_pubr() +
  geom_smooth(method=lm, fill="red", color="red", alpha=0.3, size = 1.5) + geom_hline(yintercept = Sum_Obs, color = "black", size =1.4) + 
  xlab("Field Capacity") + ylab(expression(paste("Total Runoff [  ",L/m^2," ]"))) + theme(plot.title = element_text(hjust=0.5, size=13))

p14 <- ggplot(cal_results, aes(Wilting_Point, Sum_R)) + geom_point(alpha=0.8, color='#0078d2') + ggtitle("Substrate Wilting Point") + theme_pubr() +
  geom_smooth(method=lm, fill="red", color="red", alpha=0.3, size = 1.5) + geom_hline(yintercept = Sum_Obs, color = "black", size =1.4) + 
  xlab("Wilting Point") + ylab(expression(paste("Total Runoff [  ",L/m^2," ]"))) + theme(plot.title = element_text(hjust=0.5, size=13))

p15 <- ggplot(cal_results, aes(Conductivity, Sum_R)) + geom_point(alpha=0.8, color='#0078d2') + ggtitle("Substrate Conductivity") + theme_pubr() +
  geom_smooth(method=lm, fill="red", color="red", alpha=0.3, size = 1.5) + geom_hline(yintercept = Sum_Obs, color = "black", size =1.4) + 
  xlab("Conductivity [mm/h]") + ylab(expression(paste("Total Runoff [  ",L/m^2," ]"))) + theme(plot.title = element_text(hjust=0.5, size=13))

p16 <- ggplot(cal_results, aes(Conductivity_Slope, Sum_R)) + geom_point(alpha=0.8, color='#0078d2') + ggtitle("Substrate Conductivity Slope") + theme_pubr() +
  geom_smooth(method=lm, fill="red", color="red", alpha=0.3, size = 1.5) + geom_hline(yintercept = Sum_Obs, color = "black", size =1.4) + 
  xlab("Conductivity Slope") + ylab(expression(paste("Total Runoff [  ",L/m^2," ]"))) + theme(plot.title = element_text(hjust=0.5, size=13))

p17 <- ggplot(cal_results, aes(Suction_Head, Sum_R)) + geom_point(alpha=0.8, color='#0078d2') + ggtitle("Substrate Suction Head") + theme_pubr() +
  geom_smooth(method=lm, fill="red", color="red", alpha=0.3, size = 1.5) + geom_hline(yintercept = Sum_Obs, color = "black", size =1.4) + 
  xlab("Suction Head [mm]") + ylab(expression(paste("Total Runoff [  ",L/m^2," ]"))) + theme(plot.title = element_text(hjust=0.5, size=13))

p18 <- ggplot(cal_results, aes(Drain_Thickness, Sum_R)) + geom_point(alpha=0.8, color='#0078d2') + ggtitle("Drainage Thickness") + theme_pubr() +
  geom_smooth(method=lm, fill="red", color="red", alpha=0.3, size = 1.5) + geom_hline(yintercept = Sum_Obs, color = "black", size =1.4) + 
  xlab("Thickness [mm]") + ylab(expression(paste("Total Runoff [  ",L/m^2," ]"))) + theme(plot.title = element_text(hjust=0.5, size=13))

p19 <- ggplot(cal_results, aes(Void_Fraction, Sum_R)) + geom_point(alpha=0.8, color='#0078d2') + ggtitle("Drainage Void Fraction") + theme_pubr() +
  geom_smooth(method=lm, fill="red", color="red", alpha=0.3, size = 1.5) + geom_hline(yintercept = Sum_Obs, color = "black", size =1.4) + 
  xlab("Void fraction") + ylab(expression(paste("Total Runoff [  ",L/m^2," ]"))) + theme(plot.title = element_text(hjust=0.5, size=13))

p20 <- ggplot(cal_results, aes(Roughness, Sum_R)) + geom_point(alpha=0.8, color='#0078d2') + ggtitle("Drainage Roughness") + theme_pubr() +
  geom_smooth(method=lm, fill="red", color="red", alpha=0.3, size = 1.5) + geom_hline(yintercept = Sum_Obs, color = "black", size =1.4) + 
  xlab("Roughness (n)") + ylab(expression(paste("Total Runoff [  ", L/m^2," ]"))) + theme(plot.title = element_text(hjust=0.5, size=13))


fig1 <- ggarrange(p11,p12,p13,p14,p15,p16,p17,p18,p19,p20, ncol = 5, nrow = 2)
fig2 <- annotate_figure(fig1, top = text_grob("Sensitivity Analysis - Zone 4 (Wuhan)", face = "bold", size = 15))
print(fig2)


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
  my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, fill="blue", color="blue", ...) +
    geom_hline(yintercept = Sum_Obs, color = "red", size = 2)
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
