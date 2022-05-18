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

## Quick fix for renaming "German" scenario names to "English"
performances <- keys.lid::performances %>%
  dplyr::mutate(scenario_name = kwb.utils::multiSubstitute(strings = .data$scenario_name,
                                                           replacements = list("mulde_rigole" = "through-trench",
                                                                               "mulde" = "through")))

performances <- keys.lid::performances %>%
  dplyr::mutate(scenario_name = kwb.utils::multiSubstitute(strings = .data$scenario_name,
                                                           replacements = list("through-trench" = "mulde_rigole",
                                                                               "through" = "mulde",
                                                                               "with-berm" = "mit-berme",
                                                                               "no-berm" = "ohne-berme",
                                                                               "no-drainmat" = "keine-drainagematte",
                                                                               "with-drainmat" = "mit-drainagematte",
                                                                               "no-drainage" = "keine-drainage",
                                                                               "with-drainage" = "mit-drainage",
                                                                               "per.hour" = "pro.Stunde",
                                                                               "extensive" = "extensiv",
                                                                               "intensive" = "intensiv"))) %>%
                  dplyr::group_by(.data$lid_name_tidy,
                                  .data$scenario_name) %>%
                  dplyr::mutate(scenario_id = dplyr::cur_group_id())

performances_without_lids <- performances %>%
  dplyr::filter(.data$lid_name_tidy == "bioretention_cell",
                .data$lid_area_fraction == 0,
                .data$scenario_id == 1) %>%
  dplyr::mutate(lid_name_tidy = "Referenz",
                scenario_name = "Regenr\u00FCckhalt ohne LID")


mycolors <- c(rev(RColorBrewer::brewer.pal(name="Reds", n = 6)),
              RColorBrewer::brewer.pal(name="Blues", n = 6))

performances_without_lids %>%
  tidyr::unnest(.data$annual) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = .data$zone_id,
                                         y = 100*.data$vrr)) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  ggplot2::geom_boxplot() +
  ggplot2::geom_jitter(mapping = ggplot2::aes(col = factor(.data$year)),
                       size=2, alpha=1) +
  ggplot2::scale_color_manual(values = mycolors) +
  ggplot2::labs(x = "Klimazone",
                y = "J\u00E4hrlicher Regenr\u00FCckhalt ohne LIDs",
                col = "Jahr") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position="top")

#usethis::use_data(performances, compress = "gzip", overwrite = TRUE) #44.6MB
#usethis::use_data(performances, compress = "bzip2", overwrite = TRUE) #18.9MB
usethis::use_data(performances, compress = "xz", overwrite = TRUE) #0.6MB
