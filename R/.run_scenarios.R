library(keys.lid)

scenarios <- keys.lid::read_scenarios()

gr_tbl <- scenarios[scenarios$lid_name_tidy == "green_roof",]
gr_tbl <- gr[gr$scenario_name == unique(gr$scenario_name)[1],]

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

### "permeable_pavement"
pp <- keys.lid::simulate_performances(
  lid_selected = scenarios[scenarios$lid_name_tidy == "permeable_pavement",],
  lid_area_fractions = c(0, 1),
  catchment_area_m2 = 1000
  )


performances <- br %>%
  dplyr::bind_rows(gr) %>%
  dplyr::bind_rows(pp)
