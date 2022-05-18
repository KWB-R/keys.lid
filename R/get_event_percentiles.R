#' Get Percentiles for Events
#'
#' @param performances nested tibble (default: \code{\link{performances}})
#' @return list with percentiles for "event_sum" and "event_max"
#' @export
#' @importFrom tidyselect all_of
#' @importFrom tidyr unnest
#' @importFrom dplyr select group_by mutate summarise
get_event_percentiles <- function(performances = keys.lid::performances) {

volume <- performances %>%
  tidyr::unnest(.data$events_sum)

sel_cols <- c("zone_id",
              "lid_name_tidy",
              "scenario_name",
              "lid_area_fraction",
              "runoff_cbm",
              "tBeg",
              "tEnd")

volume_stats <- volume %>%
  dplyr::select(tidyselect::all_of(sel_cols)) %>%
  dplyr::group_by(.data$zone_id,
                   .data$lid_name_tidy,
                   .data$scenario_name,
                   .data$lid_area_fraction) %>%
  dplyr::mutate(runoff_LitrePerSqm = 1000 * .data$runoff_cbm) %>%
  dplyr::summarise(datetime_min = min(.data$tBeg),
                datetime_max = max(.data$tEnd),
                timeperiod_days = as.numeric(diff(c(datetime_min, datetime_max))),
                timeperiod_years = timeperiod_days/365,
                number_of_events = dplyr::n(),
                events_per_year = number_of_events / timeperiod_years,
                runoff_LitrePerSqm_q00 = quantile(.data$runoff_LitrePerSqm, probs = 0),
                runoff_LitrePerSqm_q01 = quantile(.data$runoff_LitrePerSqm, probs = 0.01),
                runoff_LitrePerSqm_q05 = quantile(.data$runoff_LitrePerSqm, probs = 0.05),
                runoff_LitrePerSqm_q10 = quantile(.data$runoff_LitrePerSqm, probs = 0.10),
                runoff_LitrePerSqm_q25 = quantile(.data$runoff_LitrePerSqm, probs = 0.25),
                runoff_LitrePerSqm_q50 = quantile(.data$runoff_LitrePerSqm, probs = 0.5),
                runoff_LitrePerSqm_q75 = quantile(.data$runoff_LitrePerSqm, probs = 0.75),
                runoff_LitrePerSqm_q90 = quantile(.data$runoff_LitrePerSqm, probs = 0.9),
                runoff_LitrePerSqm_q95 = quantile(.data$runoff_LitrePerSqm, probs = 0.95),
                runoff_LitrePerSqm_q99 = quantile(.data$runoff_LitrePerSqm, probs = 0.99),
                runoff_LitrePerSqm_q100 = quantile(.data$runoff_LitrePerSqm, probs = 1))


peak <- performances %>%
  tidyr::unnest(.data$events_max)

sel_cols <- c("zone_id",
              "lid_name_tidy",
              "scenario_name",
              "lid_area_fraction",
              "max_total_runoff_mmPerHour",
              "tBeg",
              "tEnd")

peak_stats <- peak %>%
  dplyr::select(tidyselect::all_of(sel_cols)) %>%
  dplyr::group_by(.data$zone_id,
                  .data$lid_name_tidy,
                  .data$scenario_name,
                  .data$lid_area_fraction) %>%
  dplyr::summarise(datetime_min = min(.data$tBeg),
                datetime_max = max(.data$tEnd),
                timeperiod_days = as.numeric(diff(c(datetime_min, datetime_max))),
                timeperiod_years = timeperiod_days/365,
                number_of_events = dplyr::n(),
                events_per_year = number_of_events / timeperiod_years,
                runoff_max_mmPerHour_q00 = quantile(.data$max_total_runoff_mmPerHour, probs = 0),
                runoff_max_mmPerHour_q01 = quantile(.data$max_total_runoff_mmPerHour, probs = 0.01),
                runoff_max_mmPerHour_q05 = quantile(.data$max_total_runoff_mmPerHour, probs = 0.05),
                runoff_max_mmPerHour_q10 = quantile(.data$max_total_runoff_mmPerHour, probs = 0.10),
                runoff_max_mmPerHour_q25 = quantile(.data$max_total_runoff_mmPerHour, probs = 0.25),
                runoff_max_mmPerHour_q50 = quantile(.data$max_total_runoff_mmPerHour, probs = 0.5),
                runoff_max_mmPerHour_q75 = quantile(.data$max_total_runoff_mmPerHour, probs = 0.75),
                runoff_max_mmPerHour_q90 = quantile(.data$max_total_runoff_mmPerHour, probs = 0.9),
                runoff_max_mmPerHour_q95 = quantile(.data$max_total_runoff_mmPerHour, probs = 0.95),
                runoff_max_mmPerHour_q99 = quantile(.data$max_total_runoff_mmPerHour, probs = 0.99),
                runoff_max_mmPerHour_q100 = quantile(.data$max_total_runoff_mmPerHour, probs = 1))

list(event_max_percentiles = peak_stats,
     event_sum_percentiles = volume_stats
     )
}
