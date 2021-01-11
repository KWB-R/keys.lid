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
  swmm_version = "5.1.013",
  swmm_exe = "<root_swmm>/EPA SWMM <swmm_version>/swmm5.exe",
  lid_models = "<root_data>/sensitivity_analysis_models",
  weather_data = "<root_data>/data_weather_sponge_regions",
  sensitivity_results = "<root_data>/sensitivity_analysis_results"
)

paths <- kwb.utils::resolve(paths_list)


# go to SWMM GUI and create input file (*.inp)
# input rainfall and temperature data for the desired sponge city climate zone
# are selected there. these files are in paths$weather_data

# read swmm model
swmm_file <- swmmr::read_inp(x = file.path(paths$lid_models, 
                                           'model_greenroof_zone1.inp'))

# read user-defined tables for parameter min and max values
params_min <- read.table(file.path(paths$lid_models, 'params_greenroof_min.csv'),
                         sep = ';', 
                         header = TRUE)
params_max <- read.table(file.path(paths$lid_models, 'params_greenroof_max.csv'),
                         sep = ';', 
                         header = TRUE)

# number of parameter combinations
l <- 10

# from swmm_file, find which LID parameters are being used (these change based on 
# type of LID being modeled)
param_positions <- apply(
  X = params_min[, 2:ncol(params_min)],
  FUN  = function(x) which(!is.na(x)),
  MARGIN = 1)
active_rows <- which(sapply(X = param_positions, FUN = length) > 0)

# initialize output data.frame

# years of analysis
years <- c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)

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
  ncol = length(colnamessens) + length(years),
  nrow = l,
  dimnames = list(NULL, c(colnamessens,
                          paste('VRR', years, sep='_')))))

# run sensitivity analysis, input time series ideally should cover several years
for(i in seq_len(l)){
  
  cat('\nmodel run no.', i, '\n')
  
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
  results <- swmmr::run_swmm(inp = file.path(paths$lid_models, 'tmp.inp'),
                             exec = paths$swmm_exe)
  swmm_run_has_errors <- !file.exists(file.path(paths$lid_models, 'tmp.out'))
  
  if(swmm_run_has_errors) {
      warning(sprintf("SWMM run %d did not complete successfully without errors.",
                      i))
    
  } else {
  # read out results for itype 3 (= system) and vIndex 4 (= runoff) and 1(= rainfall)
  results_runoff <- swmmr::read_out(results$out, iType = 3, vIndex = 4)
  results_rainfall_rate <- swmmr::read_out(results$out, iType = 3, vIndex = 1)
  
  # store results in data frame
  results <- data.frame(
    dateTime = zoo::index(results_runoff$system_variable$total_runoff),
    rainfall_rate = zoo::coredata(results_rainfall_rate$system_variable$total_rainfall),
    runoff = zoo::coredata(results_runoff$system_variable$total_runoff),
    years = format(zoo::index(results_runoff$system_variable$total_runoff), 
                   format = '%Y'))
  
  # convert runoff from l/s to mm/s
  flow_units <- swmm_file$options[
    swmm_file$options$Option == 'FLOW_UNITS', 'Value'][[1]]
  if(flow_units == 'LPS'){
    lidarea <- swmm_file$subcatchments$Area
    results$runoff <- results$runoff/(1e4*lidarea)
  }
  
  # compute rainfall depth [mm] based on rainfall rate [mm/hour] and time 
  # step of data [hours]
  
  # get time interval in hours
  dt <- as.numeric(strsplit(x = swmm_file$raingages$Interval, 
                 split = ':')[[1]])
  dt <- dt[1] + dt[2]/60
  
  # rainfall depth = rainfall rate * time
  results$rainfall_depth <- results$rainfall_rate*dt
  
  # compute annual VRR (volume rainfall retention) for all analysis years
  # and add it to output data.frame
  years <- unique(results$years)
  vrr <- vector(mode = 'numeric', length = length(years))
  names(vrr) <- years
  for(j in seq_along(years)){
    yearj <- results[results$years == years[j], ]
    runoff_volume <- computeVol(data = yearj, 
                                timeColumn = 'dateTime', 
                                Qcolumn = 'runoff')
    rainfall_volume <- sum(yearj$rainfall_depth, na.rm = TRUE)
    
    vrr[j] <- runoff_volume/rainfall_volume
  }
  
  sensitivity_results[i, 
                      (ncol(sensitivity_results) - 
                         length(vrr) + 1):ncol(sensitivity_results)] <- vrr
  
  rm(results_rainfall_rate)
  fs::file_delete(file.path(paths$lid_models, 'tmp.out'))
  }
  
  rm(results)
  fs::file_delete(file.path(paths$lid_models, 'tmp.inp'))
  fs::file_delete(file.path(paths$lid_models, 'tmp.rpt'))
}

# write out results
write.table(sensitivity_results,
            file.path(paths$sensitivity_results, 'greenroof_zone1.txt'),
            sep = ';',
            row.names = FALSE,
            quote = FALSE)


# annual VRR vs weather vs. lid parameters
y <- apply(X = sensitivity_results[, (ncol(sensitivity_results) - 
                                        12 + 1):ncol(sensitivity_results)],
           MARGIN = 1,
           FUN = mean)

X <- sensitivity_results[, 1:length(colnamessens)]
data_aov <- cbind(y, X)
summary(aov(y ~ ., data = data))

# paths$swmm_exe
# sessioninfo::session_info()

# functions ---------------------------------------------------------------------------

# compute runoff volume for runoff in mm/s
computeVol <- function(data, timeColumn, Qcolumn){

  AA <- data[-1, Qcolumn]
  aa <- data[-nrow(data), Qcolumn]
  hh <- as.numeric(data[-1, timeColumn]) -
    as.numeric(data[-nrow(data), timeColumn])
  
  Vtot <- sum((AA + aa)/2*hh)
  
  return(Vtot)
}
