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


pp_0.00 <- calculate_performance(lid_selected,
                                 lid_area_fraction = 0.00)

pp_0.01 <- calculate_performance(lid_selected,
                   lid_area_fraction = 0.01)

pp_0.05 <- calculate_performance(lid_selected,
                   lid_area_fraction = 0.05)

pp_0.1 <- calculate_performance(lid_selected,
                   lid_area_fraction = 0.1)

pp_0.5 <- calculate_performance(lid_selected,
                  lid_area_fraction = 0.5)

pp_1.0 <- calculate_performance(lid_selected,
                  lid_area_fraction = 1.0)


pp <- dplyr::bind_rows(pp_0.00, pp_0.01) %>%
  dplyr::bind_rows(pp_0.1) %>%
  dplyr::bind_rows(pp_0.5) %>%
  dplyr::bind_rows(pp_1.0) %>%
  tidyr::pivot_wider(names_from = "lid_area_fraction",
                   names_prefix = "lidarea.fraction_",
                   values_from = "vrr")

calculate_performance <- function(
        lid_selected,
        lid_area_fraction = 0.1,
        catchment_area_m2 = 1000,
        col_eventsep = "total_rainfall") {

lid_area_m2 <- lid_area_fraction * catchment_area_m2

scenario_names <- unique(lid_selected$scenario_name)
performance_list <- lapply(scenario_names, function(selected_scenario) {
  # selected_scenario <- scenario_names[1]
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



results_system <- kwb.swmm::get_results(path_out = path_out_file,
                                        vIndex = c(1,4))

results_vrr <-  results_system %>%
  dplyr::mutate(year = lubridate::year(.data$datetime)) %>%
  dplyr::group_by(.data$year) %>%
  dplyr::summarise(vrr = 1 - (sum(.data$total_runoff) / sum(.data$total_rainfall)))


rainevent_stats_sum <- kwb.swmm::calculate_rainevent_stats(results_system,
                           col_eventsep = col_eventsep,
                           aggregation_function = "sum") %>%
  dplyr::arrange(dplyr::desc(.data$sum_total_rainfall))

rainevent_stats_max <- kwb.swmm::calculate_rainevent_stats(results_system,
                                                           col_eventsep = col_eventsep,
                                                           aggregation_function = "max") %>%
  dplyr::arrange(dplyr::desc(.data$max_total_rainfall))


list(vrr = results_vrr,
     events_sum = rainevent_stats_sum,
     events_max = rainevent_stats_max)

})

performance_list
}

