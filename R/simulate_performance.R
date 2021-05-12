#' Simulate Performance of LID
#'
#' @param lid_selected tibble with a selected LID as retrieved by \code{\link{read_scenarios}}
#' @param lid_area_fraction fraction of LID in subcatchment (default: 0)
#' @param catchment_area_m2 catchment area (default: 1000 m2)
#' @param col_eventsep SWMM output column used for event separation (default:
#' "total_rainfall")
#' @param swmm_base_inp path to SWMM model to be used as template for modification
#' (default: keys.lid::extdata_file("scenarios/models/zone1/model_greenroof_zone1.inp"))
#' @param swmm_climate_dir directory with climate data
#' (default: keys.lid::extdata_file("rawdata/weather_sponge_regions")
#' @param model_dir default:  keys.lid::extdata_file("scenarios/models/zone1")

#' @return tibble with nested lists containing all scenario performance
#' @export
#' @importFrom dplyr arrange desc
#' @importFrom rlang .data
#' @importFrom lubridate year
#' @importFrom tibble tibble
#' @importFrom stringr str_replace
#' @importFrom swmmr read_inp run_swmm write_inp
#' @importFrom kwb.swmm calculate_rainevent_stats get_results
#' @importFrom withr with_dir
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
  col_eventsep = "total_rainfall",
  swmm_base_inp = keys.lid::extdata_file("scenarios/models/zone1/model_greenroof_zone1.inp"),
  swmm_climate_dir = keys.lid::extdata_file("rawdata/weather_sponge_regions"),
  model_dir = keys.lid::extdata_file("scenarios/models/zone1")
) {

  swmm_inp <- swmmr::read_inp(swmm_base_inp)

  lid_area_m2 <- lid_area_fraction * catchment_area_m2

  scenario_names <- unique(lid_selected$scenario_name)

  lapply(scenario_names, function(selected_scenario) {
    lid_selected_scenario <- lid_selected %>%
      dplyr::filter(.data$scenario_name == selected_scenario)

    lid_controls <- lidconfig_to_swmm(lid_selected_scenario)


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
                            ".inp")
    path_rpt_file <- stringr::str_replace(path_inp_file, "\\.inp", "\\.rpt")
    path_out_file <- stringr::str_replace(path_inp_file, "\\.inp", "\\.out")


    swmm_inp$lid_controls <- lid_controls
    swmm_inp$lid_usage  <- lid_usage # lid_controls$Name[1]

    swmmr::write_inp(swmm_inp, file = path_inp_file)

    withr::with_dir(new = swmm_climate_dir,
                    code = {
    swmmr::run_swmm(inp = path_inp_file,
                    rpt = path_rpt_file,
                    out = path_out_file)
                      })



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


    tibble::tibble(lid_name_tidy = unique(lid_selected$lid_name_tidy),
                   scenario_name = selected_scenario,
                   catchment_area_m2 = catchment_area_m2,
                   lid_area_fraction = lid_area_fraction,
                   lid_area_m2 = lid_area_m2,
                   lid_usage = list(lid_usage),
                   lid_controls = list(lid_controls),
                   subcatchment = list(subcatchment),
                   annual = list(results_vrr),
                   events_min = list(rainevent_stats_sum),
                   events_max = list(rainevent_stats_max),
                   col_eventsep = col_eventsep)

  }) %>%
    dplyr::bind_rows()

}
