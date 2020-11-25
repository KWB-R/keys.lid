### install package dependencies
cran_pkgs <- c("gdata", "remotes", "swmmr", "sessioninfo", "xts")
install.packages(pkgs = cran_pkgs, repos = "https://cran.rstudio.com")

remotes::install_github("kwb-r/kwb.utils")

### Download "EPA SWMM 5.1.015.7z" from KWB cloud 
### "https://cloud.kompetenz-wasser.de/index.php/f/182136"
### and extract to "C:/_UserProg/EPA SWMM 5.1.015/"

### define paths
paths_list <- list(
  root_data = ".",
  root_swmm = "C:/_UserProg",
  swmm_version = "5.1.015",
  swmm_exe = "<root_swmm>/EPA SWMM <swmm_version>/swmm5.exe",
  lid_models = "<root_data>/sensitivity_analysis_models"
)

paths <- kwb.utils::resolve(paths_list)


# create swmm model
# ... function to create *.inp automatically in the future. so far, it is done manually with the GUI
# ...

# read swmm model
swmm_file <- swmmr::read_inp(x = file.path(paths$lid_models, "model_greenroof_zone1.inp"))

# number of parameter combinations
l <- 1000

# initialize data.frame for holding parameter values
# these are specific to each lid
names <- c("run_number", 
           "surface_vegetated_volume", "surface_roughness", "surface_slope",
           "soil_thickness", "soil_porosity", "soil_field_Capacity", "soil_wilting_point", 
           "soil_conductivity", "soil_conductivity_slope", "soil_suction_head", 
           "drainmat_thickness", "drainmat_void_fraction", "drainmat_roughness", 
           "total_runoff")

sensitivity_results <- data.frame(matrix(
  data = NA,
  ncol = length(names),
  nrow = l,
  dimnames = list(NULL, names)))


# set ranges for parameter values


# run sensitivity analysis, input time series ideally should cover several years

# time period
seq <- seq.POSIXt(ISOdate(2008,4,30,00,05,tz="UTC"),
                  ISOdate(2019,10,15,23,00,tz="UTC"),
                  by="5 min")


pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in seq_len(l)){
  
  setTxtProgressBar(pb, i)
  
  # set parameters for simulation -> draw from uniform distribution with ranges
  # given above
  swmm_file$lid_controls$Par1[3] <- runif(n = 1, min = 80, max = 120) # Soil Thickness
  swmm_file$lid_controls$Par2[3] <- runif(n = 1, min = 0.45, max = 0.65) # Porosity
  swmm_file$lid_controls$Par3[3] <- runif(n = 1, min = 0.35, max = 0.55) # Field Capacity
  swmm_file$lid_controls$Par4[3] <- runif(n = 1, min = 0.05, max = 0.20) # Wilting Point
  swmm_file$lid_controls$Par5[3] <- runif(n = 1, min = 50, max = 350) # Conductivity
  swmm_file$lid_controls$Par6[3] <- runif(n = 1, min = 30, max = 55) # Conductivity Slope
  swmm_file$lid_controls$Par7[3] <- runif(n = 1, min = 50, max = 100) # Suction Head
  swmm_file$lid_controls$Par1[4] <- runif(n = 1, min = 10, max = 50) # Storage Thickness
  swmm_file$lid_controls$Par2[4] <- runif(n = 1, min = 0.3, max = 0.5) # Void Fraction
  swmm_file$lid_controls$Par3[4] <- runif(n = 1, min = 0.01, max = 0.03) # Roughness
  
  
  
  # write values in the dataframe
  cal_results$Run[i] <- i
  
  sensitivity_results$Soil_Thickness[i] <- input$lid_controls$Par1[3]
  sensitivity_results$Porosity[i] <- input$lid_controls$Par2[3]
  sensitivity_results$Field_Capacity[i] <- input$lid_controls$Par3[3] 
  sensitivity_results$Wilting_Point[i] <- input$lid_controls$Par4[3]
  sensitivity_results$Conductivity[i] <- input$lid_controls$Par5[3]
  sensitivity_results$Conductivity_Slope[i] <- input$lid_controls$Par6[3]
  sensitivity_results$Suction_Head[i] <- input$lid_controls$Par7[3]
  sensitivity_results$Drain_Thickness[i] <- input$lid_controls$Par1[4]
  sensitivity_results$Void_Fraction[i] <- input$lid_controls$Par2[4]
  sensitivity_results$Roughness[i] <- input$lid_controls$Par3[4]
  
  # save the changed input file, overwriting the original file to avoid writing thousands of copies 
  # (one per run)
  swmmr::write_inp(input,"Validation_Beijing.inp") 
  
  # run swimm with changed input file
  files <- swmmr::run_swmm(inp = "Validation_Beijing.inp",
                           exec = paths$swmm_exe) 
  
  # read out results for itype 3 = system and vIndex 4 = runoff 
  results <- swmmr::read_out(files$out, iType = 3, vIndex = c(4)) 
  
  # change timezone to UTC
  xts::tzone(results$system_variable$total_runoff) <- "UTC"
  
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
  
  
  
  # write out results
  
  
  # plot results
  
  
  # annual VRR vs weather vs. lid parameters
  
  
  
  
}




paths$swmm_exe
sessioninfo::session_info()

