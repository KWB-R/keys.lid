#' Simulate Performance of LID
#'
#' @param lid_selected tibble with a selected LID as retrieved by \code{\link{read_scenarios}}
#' @param lid_area_fraction fraction of LID in subcatchment (default: 0)
#' @param catchment_area_m2 catchment area (default: 1000 m2)
#' @param swmm_base_inp path to SWMM model to be used as template for modification
#' (default: keys.lid::extdata_file("scenarios/models/model_template.inp"))
#' @param swmm_climate_dir directory with climate data
#' (default: keys.lid::extdata_file("rawdata/weather_sponge_regions")
#' @param swmm_exe Name and path to swmm5 executable. If not manually set,
#' the following paths are looked up: linux: "/usr/bin/swmm5" darwin:
#' "/Applications/swmm5" windows: "C:/Program Files (x86)/EPA SWMM 5.1/swmm5.exe",
#' (default: NULL)
#' @param model_dir default:  keys.lid::extdata_file("scenarios/models")
#' @param zone_ids climate zone ids to be used for simulation (default: 1L:5L)

#' @return tibble with nested lists containing all scenario performance
#' @importFrom dplyr arrange desc
#' @importFrom rlang .data
#' @importFrom lubridate year
#' @importFrom tibble tibble
#' @importFrom stringr str_replace
#' @importFrom swmmr read_inp run_swmm write_inp
#' @importFrom kwb.swmm calculate_rainevent_stats get_results
#' @importFrom kwb.utils resolve catAndRun
#' @export
#' @examples
#' \dontrun{
#' scenarios <- keys.lid::read_scenarios()
#' unique(scenarios$lid_name_tidy)
#' lid <- "permeable_pavement"
#' lid_selected <- scenarios %>%  dplyr::filter(.data$lid_name_tidy == lid)
#' pp_0.00 <- keys.lid::simulate_performance(lid_selected,
#'                                           lid_area_fraction = 0.00)
#' pp_1.0 <- keys.lid::simulate_performance(lid_selected,
#'                                          lid_area_fraction = 1.0)
#' pp <- dplyr::bind_rows(pp_0.00, pp_1.0)
#' }
simulate_performance <- function(
  lid_selected,
  lid_area_fraction = 0,
  catchment_area_m2 = 1000,
  swmm_base_inp = keys.lid::extdata_file("scenarios/models/model_template.inp"),
  swmm_climate_dir = keys.lid::extdata_file("rawdata/weather_sponge_regions"),
  swmm_exe = NULL,
  model_dir = keys.lid::extdata_file("scenarios/models"),
  zone_ids = 1L:5L
) {

  swmm_inp <- swmmr::read_inp(swmm_base_inp)

  flow_unit <- swmm_inp$options$Value[swmm_inp$options$Option == "FLOW_UNITS"]
  stopifnot(flow_unit == "LPS")

  lid_area_m2 <- lid_area_fraction * catchment_area_m2

  lapply(zone_ids, function(zone_id) {
  scenario_names <- unique(lid_selected$scenario_name)
  msg_txt <- sprintf("Simulating LID '%s' with %s scenarios for climate zone %d (remaining zones: %d)",
                     unique(lid_selected$lid_name_tidy),
                     length(scenario_names),
                     zone_id, length(zone_ids)-zone_id)
  kwb.utils::catAndRun(messageText = msg_txt, expr = {
  lapply(scenario_names, function(selected_scenario) {
    lid_selected_scenario <- lid_selected %>%
      dplyr::filter(.data$scenario_name == selected_scenario)

    lid_controls <- lidconfig_to_swmm(lid_selected_scenario)


    subcatchment <- tibble::tibble(Name = "S1",
                                   `Rain Gage` = "RainGage",
                                   Outlet = "Out1",
                                   Area = kwb.swmm::squaremeter_to_hectar(catchment_area_m2),
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


    path_inp_file <- paste0(sprintf("%s/zone-%d_%s_lidshare-%1.2f",
                                    model_dir,
                                    zone_id,
                                    lid_controls$Name[1],
                                    lid_area_fraction),
                            ".inp")
    path_rpt_file <- stringr::str_replace(path_inp_file, "\\.inp", "\\.rpt")
    path_out_file <- stringr::str_replace(path_inp_file, "\\.inp", "\\.out")


    swmm_inp$lid_controls <- lid_controls
    swmm_inp$lid_usage  <- lid_usage # lid_controls$Name[1]


    paths_list <- list(temp = "<climate_dir>/swmm_climeng_zone<zone_id>_temp.txt",
                       rain = "<climate_dir>/swmm_bwsti_zone<zone_id>_rain_hourly.txt"
                       )

    paths <- kwb.utils::resolve(paths_list,
                                climate_dir = swmm_climate_dir,
                                zone_id = zone_id)

    stopifnot(all(file.exists(unlist(paths)
                              )
                  )
              )


  swmm_inp$raingages$Source <- sprintf('FILE       \"%s\" BWSTI      MM',
                                         normalizePath(paths$rain))
  swmm_inp$temperature[swmm_inp$temperature$`Data Element`=="FILE", "Values"] <- sprintf('\"%s\"',
                                                                                         normalizePath(paths$temp))

  swmmr::write_inp(swmm_inp, file = path_inp_file)
  swmmr::run_swmm(inp = path_inp_file,
                  rpt = path_rpt_file,
                  out = path_out_file,
                  exec = swmm_exe
                  )


   lps_to_mmPerHour <- function(values) {
     values * 3.6
   }

    results_system <- kwb.swmm::get_results(path_out = path_out_file,
                                            vIndex = c(1,4)) %>%
      dplyr::rename(total_rainfall_mmPerHour = .data$total_rainfall,
                    total_runoff_litrePerSecond = .data$total_runoff) %>%
      dplyr::mutate(total_runoff_mmPerHour = lps_to_mmPerHour(.data$total_runoff_litrePerSecond)) %>%
      dplyr::select(- .data$total_runoff_litrePerSecond)

    results_vrr <-  results_system %>%
      dplyr::mutate(year = lubridate::year(.data$datetime)) %>%
      dplyr::group_by(.data$year) %>%
      dplyr::summarise(vrr = 1 - (sum(.data$total_runoff_mmPerHour) / sum(.data$total_rainfall_mmPerHour)))

    col_eventsep <- "total_rainfall_mmPerHour"

    rainevent_stats_mean <- kwb.swmm::calculate_rainevent_stats(results_system,
                                                               col_eventsep = col_eventsep,
                                                               aggregation_function = "mean") %>%
      dplyr::mutate(rainfall_cbm = .data$dur * .data$mean_total_rainfall_mmPerHour/3600/1000,
                    runoff_cbm = .data$dur * .data$mean_total_runoff_mmPerHour/3600/1000,
                    vrr = 1 - runoff_cbm / rainfall_cbm) %>%
      dplyr::arrange(dplyr::desc(.data$mean_total_rainfall_mmPerHour))

    rainevent_stats_max <- kwb.swmm::calculate_rainevent_stats(results_system,
                                                               col_eventsep = col_eventsep,
                                                               aggregation_function = "max") %>%
      dplyr::arrange(dplyr::desc(.data$max_total_rainfall_mmPerHour))


    tibble::tibble(lid_name_tidy = unique(lid_selected$lid_name_tidy),
                   scenario_name = selected_scenario,
                   catchment_area_m2 = catchment_area_m2,
                   lid_area_fraction = lid_area_fraction,
                   lid_area_m2 = lid_area_m2,
                   lid_usage = list(lid_usage),
                   lid_controls = list(lid_controls),
                   subcatchment = list(subcatchment),
                   annual = list(results_vrr),
                   events_sum = list(rainevent_stats_mean),
                   events_max = list(rainevent_stats_max),
                   col_eventsep = col_eventsep,
                   model_inp = path_inp_file,
                   model_rpt = path_rpt_file,
                   model_out = path_out_file)

  }) %>%
    dplyr::bind_rows()

})}) %>% dplyr::bind_rows(.id = "zone_id")


}
