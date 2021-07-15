#' Title
#'
#' @param export_dir default: tempdir()
#'
#' @return write "performances" to "swmm_lid-performances.xlsx" in directory
#' "export_dir" and return path to fike
#' @export
#'
#' @importFrom stats setNames
#' @importFrom tidyselect all_of
#' @importFrom openxlsx write.xlsx
#' @importFrom dplyr select
#' @importFrom tidyr nest
export_performances <- function(export_dir = tempdir()) {

  path <- file.path(export_dir, "swmm_lid-performances.xlsx")

  list_elements <- names(performances)[sapply(performances, is.list)]


  unnest_list_col <- function(list_element) {
  list_elements_to_remove <- list_elements[! list_elements %in% list_element]

  performances %>%
    dplyr::select(!tidyselect::all_of(list_elements_to_remove)) %>%
    tidyr::unnest(tidyselect::all_of(list_element))
  }

  export <- stats::setNames(lapply(list_elements, function(list_element) {
    unnest_list_col(list_element)}),
                            list_elements)

  openxlsx::write.xlsx(x = export, file = path)
  path
}
