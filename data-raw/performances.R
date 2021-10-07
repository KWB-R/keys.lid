## code to prepare `DATASET` dataset goes here
library(keys.lid)

scenarios <- keys.lid::read_scenarios()

paths_list <- list(
  swmm_exe = "C:/Program Files (x86)/EPA SWMM 5.1.015/swmm5.exe"
  )

paths <- kwb.utils::resolve(paths_list)

### takes about 2.5h for all four LIDs to simulate
catchment_area_m2 <- 1000
lid_area_fractions <- c(0,0.25,0.5,0.75,1)

### Bioretention Cell
br <- keys.lid::simulate_performances(
  lid_selected = scenarios[scenarios$lid_name_tidy == "bioretention_cell",],
  lid_area_fractions = lid_area_fractions,
  catchment_area_m2 = catchment_area_m2,
  swmm_exe = paths$swmm_exe
)

### Green Roof
gr <- keys.lid::simulate_performances(
  lid_selected = scenarios[scenarios$lid_name_tidy == "green_roof",],
  lid_area_fractions = lid_area_fractions,
  catchment_area_m2 = catchment_area_m2,
  swmm_exe = paths$swmm_exe
)

### Permeable Pavement
pp <- keys.lid::simulate_performances(
  lid_selected = scenarios[scenarios$lid_name_tidy == "permeable_pavement",],
  lid_area_fractions = lid_area_fractions,
  catchment_area_m2 = catchment_area_m2,
  swmm_exe = paths$swmm_exe
)

performances <- br %>%
  dplyr::bind_rows(gr) %>%
  dplyr::bind_rows(pp)

# scenarios <- keys.lid::performances
#
# performances <- scenarios[scenarios$lid_name_tidy == "bioretention_cell",] %>%
#   dplyr::bind_rows(gr) %>%
#   scenarios[scenarios$lid_name_tidy == "permeable_pavement",]
# performances <- keys.lid::performances

performances_event_percentiles <- keys.lid::get_event_percentiles(performances)
export_dir <- "."
keys.lid::export_performances(export_dir = export_dir)
path_wb <- file.path(export_dir, "swmm_lid-performances.xlsx")
wb <- openxlsx::loadWorkbook(file = path_wb)
openxlsx::addWorksheet(wb = wb,
                       sheetName = "event_max_percentiles")
openxlsx::writeData(wb,
                    sheet = "event_max_percentiles",
                    x = performances_event_percentiles$event_max_percentiles)
openxlsx::addWorksheet(wb = wb,
                       sheetName = "event_sum_percentiles")
openxlsx::writeData(wb,
                    sheet = "event_sum_percentiles",
                    x = performances_event_percentiles$event_sum_percentiles)
openxlsx::saveWorkbook(wb = wb,
                       file = path_wb,
                       overwrite = TRUE)
#saveRDS(object = performances, file = "performances.rds")
#readRDS(file = "performances.rds")

### Check different compression formats (as recommended by Rcmdcheck):
### https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Data-in-packages

#usethis::use_data(performances, compress = "gzip", overwrite = TRUE) #44.6MB
#usethis::use_data(performances, compress = "bzip2", overwrite = TRUE) #18.9MB
usethis::use_data(performances, compress = "xz", overwrite = TRUE) #0.6MB
