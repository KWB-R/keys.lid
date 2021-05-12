if(FALSE)  {

library(keys.lid)

model_dir <- keys.lid::extdata_file("scenarios/models/zone1")

base_model <- list.files(path = model_dir, pattern = "\\.inp$", full.names = TRUE)

swmm_inp <- swmmr::read_inp(base_model[1])
swmm_inp$lid_controls
swmm_inp$lid_usage

scenarios <- keys.lid::read_scenarios()

unique(scenarios$lid_name_tidy)

lid <- "permeable_pavement"

lid_selected <- scenarios %>%  dplyr::filter(.data$lid_name_tidy == lid)


pp_0.00 <- get_vrr(lid_selected,
                   lid_area_fraction = 0.00)

pp_0.01 <- get_vrr(lid_selected,
                   lid_area_fraction = 0.01)

pp_0.05 <- get_vrr(lid_selected,
                   lid_area_fraction = 0.05)

pp_0.1 <- get_vrr(lid_selected,
                   lid_area_fraction = 0.1)

pp_0.5 <- get_vrr(lid_selected,
                  lid_area_fraction = 0.5)

pp_1.0 <- get_vrr(lid_selected,
                  lid_area_fraction = 1.0)


pp <- dplyr::bind_rows(pp_0.00, pp_0.01) %>%
  dplyr::bind_rows(pp_0.1) %>%
  dplyr::bind_rows(pp_0.5) %>%
  dplyr::bind_rows(pp_1.0) %>%
  tidyr::pivot_wider(names_from = "lid_area_fraction",
                   names_prefix = "lidarea.fraction_",
                   values_from = "vrr")

get_vrr <- function(
        lid_selected,
        lid_area_fraction = 0.1,
        catchment_area_m2 = 1000,
        lid_area_m2 = lid_area_fraction * catchment_area_m2)
{
scenario_names <- unique(lid_selected$scenario_name)
vrr_list <- lapply(scenario_names, function(selected_scenario) {
lid_selected_scenario <- lid_selected %>%
  dplyr::filter(.data$scenario_name == selected_scenario)

lid_controls <- keys.lid::lidconfig_to_swmm(lid_selected_scenario)


subcatchment <- tibble::tibble(Name = "S1",
                               `Rain Gage` = "RainGage",
                               Outlet = "Out1",
                               Area = keys.lid::squaremeter_to_hectar(catchment_area_m2),
                               Perc_Imperv = 100,
                               Width = 6,
                               Perc_Slope = 0.5,
                               Curb_Len = 0,
                               Snowpack = "snowPack1"
                               )

swmm_inp$subcatchments <- subcatchment


lid_usage <- tibble::tibble("Subcatchment" = "S1",
                    "LID Process" = lid_controls$Name[1],
                    "Number" = 1,
                    "Area" = lid_area_m2,
                    "Width" = 6,
                    "InitSat" = 0,
                    "FromImp" = 0,
                    "ToPerv" = 0,
                    "RptFile" = "*",
                    "DrainTo" = "*                0"
                    )

path_inp_file <- paste0(sprintf("%s/%s",
                                model_dir, lid_controls$Name[1]),
                                # stringr::str_replace_all(lid_controls$Name[1],
                                #                      pattern = "\\.",
                                #                      replacement = "__")),
                        ".inp")
path_rpt_file <- stringr::str_replace(path_inp_file, "\\.inp", "\\.rpt")
path_out_file <- stringr::str_replace(path_inp_file, "\\.inp", "\\.out")


swmm_inp$lid_controls <- lid_controls
swmm_inp$lid_usage  <- lid_usage # lid_controls$Name[1]

#Temperature file
#swmm_inp$temperature$Values[swmm_inp$temperature$`Data Element` == "FILE"]

#Rain file
#swmm_inp$raingages$Source


swmmr::write_inp(swmm_inp, file = path_inp_file)
swmmr::run_swmm(inp = path_inp_file,
                rpt = path_rpt_file,
                out = path_out_file)

# read out results for itype 3 (= system) and vIndex 4 (= runoff) and 1(= rainfall)
results_runoff <- swmmr::read_out(path_out_file, iType = 3, vIndex = 4)
results_rainfall_rate <- swmmr::read_out(path_out_file, iType = 3, vIndex = 1)

# store results in data frame
results <- data.frame(
  dateTime = zoo::index(results_runoff$system_variable$total_runoff),
  rainfall_rate = zoo::coredata(results_rainfall_rate$system_variable$total_rainfall),
  runoff = zoo::coredata(results_runoff$system_variable$total_runoff),
  years = format(zoo::index(results_runoff$system_variable$total_runoff),
                 format = '%Y'))


# convert runoff from l/s to mm/s
flow_units <- swmm_inp$options[
  swmm_inp$options$Option == 'FLOW_UNITS', 'Value'][[1]]
if(flow_units == 'LPS'){
  lidarea <- swmm_inp$subcatchments$Area
  results$runoff <- results$runoff/(1e4*lidarea)
}

# compute rainfall depth [mm] based on rainfall rate [mm/hour] and time
# step of data [hours]

# get time interval in hours
dt <- as.numeric(strsplit(x = swmm_inp$raingages$Interval,
                          split = ':')[[1]])
dt <- dt[1] + dt[2]/60

# rainfall depth = rainfall rate * time
results$rainfall_depth <- results$rainfall_rate*dt

# compute annual VRR (volume rainfall retention) for all analysis years
# and add it to output data.frame
res_vrr <- tibble::tibble(name = lid_controls$Name[1],
                          years = unique(results$years),
                          vrr = NA_real_,
                          lid_area_fraction = lid_area_fraction)


for(j in seq_len(nrow(res_vrr))) {
  yearj <- results[results$years == res_vrr$years[j], ]
  runoff_volume <- keys.lid::computeVol(data = yearj,
                                        timeColumn = 'dateTime',
                                        Qcolumn = 'runoff')
  rainfall_volume <- sum(yearj$rainfall_depth, na.rm = TRUE)

  #### add more output parameters
  #### -> reduction of maximum runoff (per event ?) -> mean reduction runoff
  #### -> reduction of events time ? -> mean reduction of events time
  #### > rain: runoff amount / peak (-> 90 percent percentile ?! )


  res_vrr$vrr[j] <- 1 - runoff_volume/rainfall_volume
}

res_vrr

})

data.table::rbindlist(vrr_list)
}

}

