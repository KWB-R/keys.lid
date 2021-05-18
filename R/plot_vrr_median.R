
#' Plot Median VRR
#'
#' @param lid tidy name of LID
#' @param performances nested tibble (default: \link{performance})
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
                                catchment_area_m2)) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")

  plotly::ggplotly(g) %>%
  plotly::layout(legend = list(orientation = "h", x = 0, y = -0.1 ))
}

