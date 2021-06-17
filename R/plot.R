#' Plot Median VRR
#'
#' @param lid tidy name of LID
#' @param performances nested tibble (default: \code{\link{performances}})
#' @return interactive plot of performance results
#' @export
#' @importFrom tidyr unnest
#' @importFrom dplyr group_by summarise
#' @importFrom plotly ggplotly layout
#' @import ggplot2
#' @importFrom stats median
#' @examples
#' \dontrun{
#' lids <- unique(keys.lid::performances$lid_name_tidy)
#' sapply(lids, function(lid) print(keys.lid::plot_vrr_median(lid)))
#' }
plot_vrr_median <- function(lid = "bioretention_cell",
                            performances = keys.lid::performances) {

  perf_selected <- performances %>%
    dplyr::filter(.data$lid_name_tidy == lid)

  catchment_area_m2 <- unique(perf_selected$catchment_area_m2)

  g <- perf_selected %>%
    dplyr::mutate(label = sprintf("%s (%d m2)", .data$scenario_name, .data$lid_area_m2)) %>%
    tidyr::unnest(.data$annual) %>%
    # dplyr::filter(.data$vrr > 0) %>%
    dplyr::group_by(.data$zone_id,
                    .data$lid_name_tidy,
                    .data$scenario_name,
                    .data$lid_area_fraction) %>%
    dplyr::summarise(vrr_median = stats::median(.data$vrr)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes_string(x = "lid_area_fraction",
                                        y = "vrr_median",
                                        color = "scenario_name")) +
    ggplot2::facet_wrap(~ zone_id, ncol = 1) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(title = sprintf("%s (catchment area: %d m2)",
                                  lid,
                                  catchment_area_m2)) +#,
                  #y = "Median Volume Rainfall Retended per Year (%)") +
    ggplot2::coord_cartesian(ylim = c(0,1)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  plotly::ggplotly(g) %>%
    plotly::layout(legend = list(orientation = "h", x = 0, y = -0.1 ))#,
                   #ylab = list(orientation = "v", x = -0.5, y = 0 ))
}

#' Boxplot Volume Rainfall Retended per Year
#'
#' @param lid tidy name of LID (default: "bioretention_cell")
#' @param zone_id climate zone id to plot (default: 1)
#' @param performances nested tibble (default: \code{\link{performances}})
#' @return interactive plot of performance results
#' @export
#' @importFrom tidyr unnest
#' @importFrom dplyr group_by summarise
#' @importFrom plotly plot_ly layout
#' @examples
#' \dontrun{
#' boxplot_vrr(lid = "bioretention_cell", zone_id = 1)
#' }
boxplot_vrr <- function(lid = "bioretention_cell",
                        zone_id = 1,
                        performances = keys.lid::performances) {

  sel_zone <- as.integer(zone_id)

  perf_selected <- performances %>%
    dplyr::mutate(zone_id = as.integer(.data$zone_id)) %>%
    dplyr::filter(.data$lid_name_tidy == lid,
                  .data$zone_id == sel_zone)

  catchment_area_m2 <- unique(perf_selected$catchment_area_m2)

  perf_selected %>%
    dplyr::mutate(lid_area_fraction = as.factor(lid_area_fraction),
                  scenario_name = as.factor(.data$scenario_name),
                  label = sprintf("%s (%d m2)", .data$scenario_name, .data$lid_area_m2)) %>%
    tidyr::unnest(.data$annual) %>%
    dplyr::group_by(.data$zone_id,
                    .data$lid_name_tidy,
                    .data$scenario_name,
                    .data$lid_area_fraction) %>%
    plotly::plot_ly(x = ~lid_area_fraction,
                    y = ~vrr*100,
                    color = ~scenario_name,
                    type = "box") %>%
    plotly::layout(boxmode = "group",
                   title = sprintf("zone %d: %s (catchment area: %s m2)",
                                   zone_id,
                                   lid,
                                   catchment_area_m2),
                   xaxis = list(title='LID area fraction'),
                   yaxis = list(title='Volume Rainfall Retended (%)',
                                range = c(0, 100)),
                   legend = list(orientation = "h", x = 0, y = -0.1 ))


}


#' Boxplot Runoff Maximum per Event
#'
#' @param lid tidy name of LID (default: "bioretention_cell")
#' @param zone_id climate zone id to plot (default: 1)
#' @param performances nested tibble (default: \code{\link{performances}})
#' @return interactive plot of performance results
#' @export
#' @importFrom tidyr unnest
#' @importFrom dplyr group_by summarise
#' @importFrom plotly plot_ly layout
#' @examples
#' \dontrun{
#' boxplot_runoff_max(lid = "bioretention_cell", zone_id = 1)
#' }
boxplot_runoff_max <- function(lid = "bioretention_cell",
                               zone_id = 1,
                               performances = keys.lid::performances) {

  sel_zone <- as.integer(zone_id)

  perf_selected <- performances %>%
    dplyr::mutate(zone_id = as.integer(.data$zone_id)) %>%
    dplyr::filter(.data$lid_name_tidy == lid,
                  .data$zone_id == sel_zone)

  catchment_area_m2 <- unique(perf_selected$catchment_area_m2)

  perf_selected %>%
    dplyr::mutate(lid_area_fraction = as.factor(lid_area_fraction),
                  scenario_name = as.factor(.data$scenario_name),
                  label = sprintf("%s (%d m2)", .data$scenario_name, .data$lid_area_m2)) %>%
    tidyr::unnest(.data$events_max) %>%
    dplyr::group_by(.data$zone_id,
                    .data$lid_name_tidy,
                    .data$scenario_name,
                    .data$lid_area_fraction) %>%
    plotly::plot_ly(x = ~lid_area_fraction,
                    y = ~max_total_runoff_mmPerHour,
                    color = ~scenario_name,
                    type = "box") %>%
    plotly::layout(boxmode = "group",
                   title = sprintf("zone %d: %s (catchment area: %s m2)",
                                   zone_id,
                                   lid,
                                   catchment_area_m2),
                   xaxis = list(title='LID area fraction'),
                   yaxis = list(title='Maximum total runoff (mm/h per event)'),
                   legend = list(orientation = "h", x = 0, y = -0.1 ))


}

#' Boxplot Runoff Volume per Event
#'
#' @param lid tidy name of LID (default: "bioretention_cell")
#' @param zone_id climate zone id to plot (default: 1)
#' @param performances nested tibble (default: \code{\link{performances}})
#' @return interactive plot of performance results
#' @export
#' @importFrom tidyr unnest
#' @importFrom dplyr group_by summarise
#' @importFrom plotly plot_ly layout
#' @examples
#' \dontrun{
#' boxplot_runoff_volume(lid = "bioretention_cell", zone_id = 1)
#' }
boxplot_runoff_volume <- function(lid = "bioretention_cell",
                               zone_id = 1,
                               performances = keys.lid::performances) {

  sel_zone <- as.integer(zone_id)

  perf_selected <- performances %>%
    dplyr::mutate(zone_id = as.integer(.data$zone_id)) %>%
    dplyr::filter(.data$lid_name_tidy == lid,
                  .data$zone_id == sel_zone)

  catchment_area_m2 <- unique(perf_selected$catchment_area_m2)

  perf_selected %>%
    dplyr::mutate(lid_area_fraction = as.factor(lid_area_fraction),
                  scenario_name = as.factor(.data$scenario_name),
                  label = sprintf("%s (%d m2)", .data$scenario_name, .data$lid_area_m2)) %>%
    tidyr::unnest(.data$events_sum) %>%
    dplyr::group_by(.data$zone_id,
                    .data$lid_name_tidy,
                    .data$scenario_name,
                    .data$lid_area_fraction) %>%
    dplyr::mutate(sum_total_runoff_cbm = .data$dur * .data$sum_total_runoff / 1000 / catchment_area_m2) %>%
    plotly::plot_ly(x = ~lid_area_fraction,
                    y = ~sum_total_runoff_cbm,
                    color = ~scenario_name,
                    type = "box") %>%
    plotly::layout(boxmode = "group",
                   title = sprintf("zone %d: %s (catchment area: %s m2)",
                                   zone_id,
                                   lid,
                                   catchment_area_m2),
                   xaxis = list(title='LID area fraction'),
                   yaxis = list(title='Total Runoff Volume (m3 per m2 per event)'),
                   legend = list(orientation = "h", x = 0, y = -0.1 ))


}
