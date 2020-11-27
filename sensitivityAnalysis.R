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
  root_swmm = 'c:/Program Files (x86)/',
  swmm_version = "5.1.013",
  swmm_exe = "<root_swmm>/EPA SWMM <swmm_version>/swmm5.exe",
  lid_models = "<root_data>/sensitivity_analysis_models"
)

paths <- kwb.utils::resolve(paths_list)


# function to create *.inp automatically in the future. 
# at the moment, it is done manually with the GUI
# ...

# read swmm model
swmm_file <- swmmr::read_inp(x = file.path(paths$lid_models, "model_greenroof.inp"))

# read user-defined tables for parameter min and max values
params_min <- read.table(file.path(paths$lid_models, 'params_greenroof_min.csv'),
                         sep = ';', 
                         header = TRUE)
params_max <- read.table(file.path(paths$lid_models, 'params_greenroof_max.csv'),
                         sep = ';', 
                         header = TRUE)

# number of parameter combinations
l <- 100

# from swmm_file, find which LID parameters are being used (these change based on 
# type of LID being modeled)
param_positions <- apply(
  X = params_min[, 2:ncol(params_min)],
  FUN  = function(x) which(!is.na(x)),
  MARGIN = 1)
active_rows <- which(sapply(X = param_positions, FUN = length) > 0)

# initialize output data.frame

# make column names
colnamessens <- vector(mode = 'character', length = 0)
for(row in active_rows){
  for(col in param_positions[[row]]){
    colnamessens <- c(colnamessens,
                      paste(params_min$layer[row],
                            colnames(swmm_file$lid_controls)[2 + col],
                            sep = '_'))
  }
}

# create data.frame
sensitivity_results <- data.frame(matrix(
  data = NA,
  ncol = length(colnamessens),
  nrow = l,
  dimnames = list(NULL, colnamessens)))

# run sensitivity analysis, input time series ideally should cover several years

# time period
simperiod <- seq.POSIXt(ISOdate(2008, 4, 30, 00, 05,tz="UTC"),
                        ISOdate(2019, 10, 15, 23, 00,tz="UTC"),
                        by="5 min")

pb <- txtProgressBar(min = 0, max = l, style = 3)
for(i in seq_len(l)){
  
  setTxtProgressBar(pb, i)
  
  # set parameters for simulation -> draw from uniform distribution with ranges
  # given by user in params_min and params_max
  for(row in active_rows){
    for(col in param_positions[[row]]){
      swmm_file$lid_controls[row, col+2] <-
        runif(n = 1, 
              min = params_min[[row, col + 1]], 
              max = params_max[[row, col + 1]])
    }
  }
  
  # if the random draw produced field capacity > porosity, set field capacity = 
  # 0.99*porosity
  if('SOIL' %in% swmm_file$lid_controls$`Type/Layer`){
    porosity <- 
      swmm_file$lid_controls$Par2[swmm_file$lid_controls$`Type/Layer` == 'SOIL']
    
    field_capacity <- 
      swmm_file$lid_controls$Par3[swmm_file$lid_controls$`Type/Layer` == 'SOIL']
    
    if(field_capacity > porosity){
      swmm_file$lid_controls$Par3[swmm_file$lid_controls$`Type/Layer` == 'SOIL'] <-
        0.99*swmm_file$lid_controls$Par2[swmm_file$lid_controls$`Type/Layer` == 'SOIL']
    }
  }
  
  # write parameter values in the output data.frame
  for(row in active_rows){
    for(col in param_positions[[row]]){
      
      currcol <- paste(params_min$layer[row],
                       colnames(swmm_file$lid_controls)[2 + col],
                       sep = '_')
      
      sensitivity_results[i, currcol] <- swmm_file$lid_controls[row, col+2]
    }
  }
  
  # save the changed input file, overwriting the original file to avoid writing
  # thousands of copies (one per run)
  swmmr::write_inp(swmm_file, file.path(paths$lid_models, 'tmp.inp'))
  
  # run swimm with changed input file
  files <- swmmr::run_swmm(inp = file.path(paths$lid_models, 'tmp.inp'),
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
    nrow = length(simperiod)))
  
  colnames(runoff_sim) <- "sim" 
  
  runoff_sim$DateTime <- simperiod
  
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

