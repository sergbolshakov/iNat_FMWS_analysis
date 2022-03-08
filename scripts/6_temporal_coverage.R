library(magrittr)
source("scripts/set_options.R")
simpleCache::loadCaches("inat")

# Summarize data by years ------------------------------------------------------

inat %>% 
  dplyr::filter(!is.na(eventDate)) %>% 
  tsibble::as_tsibble(index = eventDate,
                      key = occurrenceID) %>% 
  tsibble::index_by(year = ~ lubridate::year(.)) %>%
  dplyr::summarise(
    observations_all = dplyr::n_distinct(occurrenceID),
    observations_rg = dplyr::n_distinct(occurrenceID[qualityGrade == "research"])
  ) %>% 
  flextable::flextable() %>%
  flextable::colformat_date() %>% 
  flextable::autofit()

# Summarize and plot data by months and quality grade --------------------------

inat_month <- 
  inat %>% 
  dplyr::filter(!is.na(eventDate),
                eventDate > "2018-01-01" & eventDate < "2022-01-01") %>% 
  tsibble::as_tsibble(index = eventDate,
                      key = occurrenceID) %>% 
  tsibble::index_by(year_month = ~ tsibble::yearmonth(.)) %>%
  dplyr::summarise(
    observations_all = dplyr::n_distinct(occurrenceID),
    observations_rg = dplyr::n_distinct(occurrenceID[qualityGrade == "research"])
  ) %>% 
  tidyr::pivot_longer(cols = c(observations_all,
                               observations_rg),
                      names_to = "qualityGrade",
                      values_to = "observations")

# Simple stacked column chart

inat_month %>% 
  ggplot2::ggplot(ggplot2::aes(lubridate::ym(year_month), observations,
                               color = qualityGrade,
                               fill = qualityGrade)
                  ) +
  ggplot2::geom_col() +
  ggplot2::xlab('Month of year') +
  ggplot2::ylab('Number of observations') +
  ggplot2::coord_flip() +
  ggplot2::scale_colour_manual(values = c("red", "darkblue"),
                               labels = c("All observations",
                                          "Research Grade observations")) +
  ggplot2::scale_fill_manual(values = c("white", "lightblue"),
                             labels = c("All observations",
                                        "Research Grade observations")) + 
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom",
                 legend.title = ggplot2::element_blank())

# Seasonal-trend decomposition

inat_month %>%  
  tsibble::fill_gaps() %>% 
  dplyr::mutate(observations = forecast::na.interp(observations)) %>% 
  fabletools::model(feasts::STL(
    observations ~ trend(window = 30) + season(period = "year"))
    ) %>%
  fabletools::components() %>%
  feasts::autoplot() +
  ggplot2::theme_minimal() +
  ggplot2::scale_colour_manual(values = c("red", "darkblue"),
                               labels = c("All observations",
                                          "Research Grade observations")) +
  ggplot2::theme(legend.position = "bottom",
                 legend.direction = "vertical",
                 legend.title = ggplot2::element_blank())


# Seasonal subseries plots -----------------------------------------------------

inat %>% 
  dplyr::filter(!is.na(eventDate),
                eventDate > "2018-01-01" & eventDate < "2022-01-01") %>% 
  tsibble::as_tsibble(index = eventDate,
                      key = occurrenceID) %>% 
  tsibble::index_by(month = tsibble::yearmonth(eventDate)) %>% 
  dplyr::summarise(observations = dplyr::n_distinct(occurrenceID)) %>% 
  tsibble::fill_gaps() %>% 
  dplyr::mutate(observations = forecast::na.interp(observations)) %>% 
  feasts::gg_subseries() + 
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle = 90))

# Seasonal plot ----------------------------------------------------------------

inat %>% 
  dplyr::filter(!is.na(eventDate),
                eventDate > "2018-01-01" & eventDate < "2022-01-01") %>% 
  tsibble::as_tsibble(index = eventDate,
                      key = occurrenceID) %>% 
  tsibble::index_by(week = tsibble::yearweek(eventDate)) %>% 
  dplyr::summarise(observations = dplyr::n_distinct(occurrenceID)) %>% 
  tsibble::fill_gaps() %>% 
  dplyr::mutate(observations = forecast::na.interp(observations)) %>% 
  feasts::gg_season() + 
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")
