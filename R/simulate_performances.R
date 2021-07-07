#' Simulate Performances of LID
#'
#' @param lid_area_fractions fractions of LID in subcatchment (default: c(0,1)
#' @inheritParams simulate_performance
#' @return tibble with nested lists containing all scenario performances for
#' varying lid_area_fractions
#' @export
#' @importFrom dplyr arrange desc
#' @importFrom rlang .data
#' @importFrom lubridate year
#' @importFrom tibble tibble
#' @importFrom stringr str_replace
#' @importFrom swmmr read_inp run_swmm write_inp
#' @importFrom kwb.swmm calculate_rainevent_stats get_results
#' @importFrom stats setNames
#' @examples
#' \dontrun{
#' scenarios <- keys.lid::read_scenarios()
#' unique(scenarios$lid_name_tidy)
#' lid <- "permeable_pavement"
#' lid_selected <- scenarios %>%  dplyr::filter(.data$lid_name_tidy == lid)
#' pp <- keys.lid::simulate_performances(lid_selected,
#'                                       lid_area_fractions = c(0,1)
#'                                       )
#' }
simulate_performances <- function(
  lid_selected,
  lid_area_fractions = c(0,1),
  catchment_area_m2 = 1000,
  col_eventsep = "total_rainfall",
  swmm_base_inp = keys.lid::extdata_file("scenarios/models/model_template.inp"),
  swmm_climate_dir = keys.lid::extdata_file("rawdata/weather_sponge_regions"),
  swmm_exe = NULL,
  model_dir = keys.lid::extdata_file("scenarios/models"),
  zone_ids = 1L:5L
) {
    lid <- unique(lid_selected$lid_name_tidy)
    label <- sprintf("%s_%0.2f", lid, lid_area_fractions)
    stats::setNames(
      lapply(lid_area_fractions, function(lid_area_fraction) {
      simulate_performance(lid_selected,
                           lid_area_fraction,
                           catchment_area_m2,
                           col_eventsep,
                           swmm_base_inp,
                           swmm_climate_dir,
                           swmm_exe,
                           model_dir,
                           zone_ids)
    }), nm = label) %>%
      dplyr::bind_rows()
}


