#' Convert LID config to SWMM LID controls
#' @param df data frame for a single scenario of a LID (as returned by
#' \code{\link{read_scenarios}})
#' @return data frame with SWMM LID controls
#' @export
#' @importFrom readr read_csv
#' @importFrom kwb.swmm extdata_file
#' @importFrom dplyr any_vars bind_rows filter_at left_join mutate select vars
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @importFrom tidyselect all_of starts_with
#' @importFrom stringr str_to_upper
#' @examples
#' scenarios <- keys.lid::read_scenarios()
#' unique(scenarios$lid_name_tidy)
#' lid <- "permeable_pavement"
#' lid_selected <- scenarios %>%  dplyr::filter(.data$lid_name_tidy == lid)
#' scenario_names <- unique(lid_selected$scenario_name)
#' scenario_name <- scenario_names[1]
#' scenario_name
#' lid_selected_scenario <- lid_selected[lid_selected$scenario_name == scenario_name,]
#' lid_controls <- lidconfig_to_swmm(lid_selected_scenario)
#' str(lid_controls)
lidconfig_to_swmm <- function(df) {

  lid_para <- readr::read_csv(kwb.swmm::extdata_file("lid/required_parameteristion.csv"))

  lid_parametersation <- df %>%
    dplyr::filter(!is.na(.data$id_type_parameter)) %>%
    dplyr::select(tidyselect::all_of(c("lid_name_tidy", "type", "id_type_parameter", "scenario_name", "value"))) %>%
    dplyr::left_join(lid_para  %>%
                       dplyr::select(.data$lid_id, .data$lid_name_tidy), by = "lid_name_tidy") %>%
    dplyr::mutate("Name" = sprintf("%s.%s", .data$lid_name_tidy, .data$scenario_name),
                  "Type/Layer" =  stringr::str_to_upper(.data$type),
    ) %>%
    dplyr::select(tidyselect::all_of(c("Name", "Type/Layer", "id_type_parameter", "value"))) %>%
    tidyr::pivot_wider(names_from = "id_type_parameter",
                       names_prefix = "Par",
                       values_from = "value") %>%
    dplyr::filter_at(dplyr::vars(tidyselect::starts_with("Par")), dplyr::any_vars(!is.na(.)))

  ## dont know why 5 is needed by SWMM (but generated in SWMM GUI)
  lid_parametersation[lid_parametersation$`Type/Layer` == "SURFACE", "Par5"] <- 5

  lid_id <- lid_para$lid_id[lid_para$lid_name_tidy == unique(df$lid_name_tidy)]

  lid_header <-   lid_parametersation[1,]
  lid_header[1,3:ncol(lid_header)] <- NA_real_
  lid_header$`Type/Layer` <- lid_id

  dplyr::bind_rows(lid_header, lid_parametersation)

}
