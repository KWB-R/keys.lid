## code to prepare `DATASET` dataset goes here
library(keys.lid)

scenarios <- keys.lid::read_scenarios()

### takes about 2.5h for all four LIDs to simulate

### Bioretention Cell
br <- keys.lid::simulate_performances(
  lid_selected = scenarios[scenarios$lid_name_tidy == "bioretention_cell",],
  lid_area_fractions = c(0, 0.05, 0.1, 0.2),
  catchment_area_m2 = 1000
)

### Green Roof
gr <- keys.lid::simulate_performances(
  lid_selected = scenarios[scenarios$lid_name_tidy == "green_roof",],
  lid_area_fractions = c(0,1),
  catchment_area_m2 = 1000,
)

### Permeable Pavement
pp <- keys.lid::simulate_performances(
  lid_selected = scenarios[scenarios$lid_name_tidy == "permeable_pavement",],
  lid_area_fractions = c(0, 1),
  catchment_area_m2 = 1000
)

### Rain Barrel
rb <- keys.lid::simulate_performances(
  lid_selected = scenarios[scenarios$lid_name_tidy == "rain_barrel",],
  lid_area_fractions = c(0, 0.1, 0.2, 0.4),
  catchment_area_m2 = 1000
)


performances <- br %>%
  dplyr::bind_rows(gr) %>%
  dplyr::bind_rows(pp) %>%
  dplyr::bind_rows(rb)

#saveRDS(object = performances, file = "performances.rds")
#readRDS(file = "performances.rds")

usethis::use_data(performances, overwrite = TRUE)
