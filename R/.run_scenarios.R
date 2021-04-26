library(keys.lid)

lid_para <- readr::read_csv(kwb.swmm::extdata_file("lid/required_parameteristion.csv"))
lid_para

model_dir <- keys.lid::extdata_file("scenarios/models/zone1")

base_model <- list.files(path = model_dir, pattern = "\\.inp$", full.names = TRUE)

swmm_inp <- swmmr::read_inp(base_model)
swmm_inp$lid_controls

cols <- c("Name", "Type/Layer")

lid_scenarios <- keys.lid::read_scenarios()

scenarios <- keys.lid::read_scenarios()

scenarios %>%  dplyr::filter(lid_name_tidy == "permeable_pavement")

lid <- "green_roof"
scenario_name <

scenarios[scenarios$lid_name_tidy == lid,]

