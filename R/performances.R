#' Performance results for LIDs
#'
#' A dataset containing the performance of LIDs for different climate conditions
#' created with R script in /data-raw/performances.R
#'
#' @format A nested tibble with 290 rows and 16 variables:
#' \describe{
#'   \item{zone_id}{climate zone id}
#'   \item{lid_name_tidy}{tidy LID name}
#'   \item{scenario_name}{name of LID scenario}
#'   \item{catchment_area_m2}{catchment area in squaremeters}
#'   \item{lid_area_fraction}{fraction of LID compared to total catchment}
#'   \item{lid_area_m2}{total LID area}
#'   \item{lid_usage}{tibble with LID usage parameterisation}
#'   \item{lid_controls}{tibble with LID controls parameterisation}
#'   \item{subcatchment}{tibble with subcatchment parameterisation}
#'   \item{annual}{tibble with two columns "year" and "vrr" (volume rainfall retended for each year}
#'   \item{events_max}{tibble with maximum values for each rainfall event}
#'   \item{events_sum}{tibble with sum values for each rainfall event}
#'   \item{col_eventsep}{name of SWMM results used for event separation}
#'   \item{model_inp}{path to SWMM model input file}
#'   \item{model_rpt}{path to SWMM model report file}
#'   \item{model_out}{path to SWMM model output file}
#' }
#'
"performances"
