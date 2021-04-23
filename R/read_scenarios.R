
#' read_scenarios
#'
#' @param scenarios_xlsx path to LID scenarios Excel file (default:
#' \code{\link{extdata_file}} with: "scenarios/swmm_lid-parameterisation.xlsx"
#' @return tidy scenarios data frame
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom readxl excel_sheets read_xlsx
#' @importFrom stats setNames
#' @importFrom tidyr pivot_longer
#'
read_scenarios <- function(
  scenarios_xlsx = extdata_file("scenarios/swmm_lid-parameterisation.xlsx")
) {
lids <- readxl::excel_sheets(scenarios_xlsx)

scenarios <- dplyr::bind_rows(stats::setNames(
  lapply(lids, function(lid) {
  lid <- readxl::read_xlsx(scenarios_xlsx, sheet = lid)
  cols <- names(lid)[!names(lid) %in% c("lid_name_tidy",
                                                    "type",
                                                    "parameter_unit",
                                                    "comment",
                                                    "reference")]
  tidyr::pivot_longer(lid,
                      cols = cols,
                      names_to = "scenario_name",
                      values_to = "value")
}), nm = sheets),
.id = "lid_name_tidy")


}
