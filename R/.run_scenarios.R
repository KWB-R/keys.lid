if(FALSE)  {

library(keys.lid)

model_dir <- keys.lid::extdata_file("scenarios/models/zone1")

base_model <- list.files(path = model_dir, pattern = "\\.inp$", full.names = TRUE)

swmm_inp <- swmmr::read_inp(base_model[1])
swmm_inp$lid_controls
swmm_inp$lid_usage

cols <- c("Name", "Type/Layer")

lid_scenarios <- keys.lid::read_scenarios()

scenarios <- keys.lid::read_scenarios()


lid <- "permeable_pavement"

lid_selected <- scenarios %>%  dplyr::filter(.data$lid_name_tidy == lid)

scenario_names <- unique(lid_selected$scenario_name)

selected_scenario <- scenario_names[1]

lid_selected_scenario <- lid_selected %>%  dplyr::filter(.data$scenario_name == selected_scenario)

lid_controls <- lidconfig_to_swmm(lid_selected_scenario)

lid_area_fraction <- 0.1
catchment_area_m2 <- 1000
lid_area_m2 <- lid_area_fraction * catchment_area_m2

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
                                # stringr::str_replace_all(lid_controls$Name[1],
                                #                      pattern = "\\.",
                                #                      replacement = "__")),
                        ".inp")
swmm_inp$lid_controls <- lid_controls
swmm_inp$lid_usage  <- lid_usage # lid_controls$Name[1]

#Temperature file
swmm_inp$temperature$Values[swmm_inp$temperature$`Data Element` == "FILE"]

#Rain file
swmm_inp$raingages$Source


swmmr::write_inp(swmm_inp, file = path_inp_file)
swmmr::run_swmm(path_inp_file)

}

lidconfig_to_swmm <- function(df) {

  lid_para <- readr::read_csv(kwb.swmm::extdata_file("lid/required_parameteristion.csv"))

  lid_parametersation <- df %>%
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


