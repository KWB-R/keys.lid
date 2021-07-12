## code to prepare `DATASET` dataset goes here
library(keys.lid)

scenarios <- keys.lid::read_scenarios()

paths_list <- list(
  swmm_exe = "C:/Program Files (x86)/EPA SWMM 5.1.015/swmm5.exe"
  )

paths <- kwb.utils::resolve(paths_list)

### takes about 2.5h for all four LIDs to simulate

### Bioretention Cell
br <- keys.lid::simulate_performances(
  lid_selected = scenarios[scenarios$lid_name_tidy == "bioretention_cell",],
  lid_area_fractions = c(0,1),
  catchment_area_m2 = 1000,
  swmm_exe = paths$swmm_exe
)

### Green Roof
gr <- keys.lid::simulate_performances(
  lid_selected = scenarios[scenarios$lid_name_tidy == "green_roof",],
  lid_area_fractions = c(0,1),
  catchment_area_m2 = 1000,
  swmm_exe = paths$swmm_exe
)

### Permeable Pavement
pp <- keys.lid::simulate_performances(
  lid_selected = scenarios[scenarios$lid_name_tidy == "permeable_pavement",],
  lid_area_fractions = c(0, 1),
  catchment_area_m2 = 1000,
  swmm_exe = paths$swmm_exe
)

### Rain Barrel
rb <- keys.lid::simulate_performances(
  lid_selected = scenarios[scenarios$lid_name_tidy == "rain_barrel",],
  lid_area_fractions = c(0, 0.1, 0.2, 0.4),
  catchment_area_m2 = 1000,
  swmm_exe = paths$swmm_exe
)


performances <- br %>%
  dplyr::bind_rows(gr) %>%
  dplyr::bind_rows(pp) %>%
  dplyr::bind_rows(rb)

#saveRDS(object = performances, file = "performances.rds")
#readRDS(file = "performances.rds")

### Check different compression formats (as recommended by Rcmdcheck):
### https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Data-in-packages

#usethis::use_data(performances, compress = "gzip", overwrite = TRUE) #44.6MB
#usethis::use_data(performances, compress = "bzip2", overwrite = TRUE) #18.9MB
usethis::use_data(performances, compress = "xz", overwrite = TRUE) #0.6MB
