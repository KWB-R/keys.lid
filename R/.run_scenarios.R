library(keys.lid)

scenarios <- keys.lid::read_scenarios()

unique(scenarios$lid_name_tidy)

lid <- "permeable_pavement"

lid_selected <- scenarios %>%  dplyr::filter(.data$lid_name_tidy == lid)

pp_0.00 <- keys.lid::simulate_performance(lid_selected,
                                 lid_area_fraction = 0.00)

pp_0.01 <- keys.lid::simulate_performance(lid_selected,
                   lid_area_fraction = 0.01)

pp_0.05 <- keys.lid::simulate_performance(lid_selected,
                   lid_area_fraction = 0.05)

pp_0.1 <- keys.lid::simulate_performance(lid_selected,
                   lid_area_fraction = 0.1)

pp_0.5 <- keys.lid::simulate_performance(lid_selected,
                  lid_area_fraction = 0.5)

pp_1.0 <- keys.lid::simulate_performance(lid_selected,
                                         lid_area_fraction = 1.0)


pp <- dplyr::bind_rows(pp_0.00, pp_0.01) %>%
  dplyr::bind_rows(pp_0.1) %>%
  dplyr::bind_rows(pp_0.5) %>%
  dplyr::bind_rows(pp_1.0)
