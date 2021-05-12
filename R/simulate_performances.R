#' Simulate Performances of LID
#'
#' @param lid_selected tibble with a selected LID as retrieved by \code{\link{read_scenarios}}
#' @param lid_area_fractions fractions of LID in subcatchment (default: c(0,1)
#' @param catchment_area_m2 catchment area (default: 1000 m2)
#' @param col_eventsep SWMM output column used for event separation (default:
#' "total_rainfall")
#' @param swmm_base_inp path to SWMM model to be used as template for modification
#' (default: keys.lid::extdata_file("scenarios/models/zone1/model_greenroof_zone1.inp"))
#' @param swmm_climate_dir directory with climate data
#' (default: keys.lid::extdata_file("rawdata/weather_sponge_regions")
#' @param model_dir default:  keys.lid::extdata_file("scenarios/models/zone1")

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
#' @importFrom withr with_dir
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
  swmm_base_inp = keys.lid::extdata_file("scenarios/models/zone1/model_greenroof_zone1.inp"),
  swmm_climate_dir = keys.lid::extdata_file("rawdata/weather_sponge_regions"),
  model_dir = keys.lid::extdata_file("scenarios/models/zone1")
) {
    lid <- unique(lid_selected$lid_name_tidy)
    label <- sprintf("%s_%0.2f", lid, lid_area_fractions)
    stats::setNames(
      lapply(lid_area_fractions, function(lid_area_fraction) {
      simulate_performance(lid_selected,
                           lid_area_fractions,
                           catchment_area_m2,
                           col_eventsep,
                           swmm_base_inp,
                           swmm_climate_dir,
                           model_dir)
    }), nm = label) %>%
      dplyr::bind_rows()
}


