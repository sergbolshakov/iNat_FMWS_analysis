library(magrittr)

# Load data from cache --------------------------------------------------------- 

simpleCache::loadCaches("inat", cacheDir = "cache")

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
  dplyr::mutate(
    observations_all_cumsum = cumsum(observations_all),
    observations_all_cumprc = round((observations_all_cumsum / length(inat$occurrenceID) * 100),
                                    digits = 2),
    .after = observations_all) %>% 
  flextable::flextable() %>%
  flextable::autofit()

# Summarize data by months and quality grade -----------------------------------

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

# Plot the time series ---------------------------------------------------------

inat_month %>% 
  ggplot2::ggplot(ggplot2::aes(lubridate::ym(year_month), observations,
                               color = qualityGrade)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(x = "", y = "Number of observations") +
  ggplot2::ylim(0, 2300) +
  tsibble::scale_x_yearmonth(
    limits = c(lubridate::ym("2017 Dec"), lubridate::ym("2022 Jan")),
    breaks = "4 months") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom",
                 legend.title = ggplot2::element_blank()) +
  ggplot2::scale_colour_manual(values = c("darkblue", "red"),
                               labels = c("All observations",
                                          "Research Grade observations")) +
  ggplot2::geom_text(
    data = inat_month %>% 
      dplyr::filter(qualityGrade == "observations_all") %>% 
      tsibble::filter_index("2018 Aug", "2019 Aug", "2020 Apr",
                            "2020 Sep", "2021 May", "2021 Sep"),
    ggplot2::aes(label = paste(observations)),
    check_overlap = TRUE, vjust = -0.7,
    show.legend = FALSE)

ggplot2::ggsave("output/time_series.jpg",
                dpi = 1200,
                bg = "white")

# Seasonal subseries plots for Agaricomycetes only -----------------------------

inat %>% 
  dplyr::filter(class == "Agaricomycetes",
                !is.na(eventDate),
                eventDate > "2018-01-01" & eventDate < "2021-12-31") %>% 
  tsibble::as_tsibble(index = eventDate,
                      key = occurrenceID) %>% 
  tsibble::index_by(month = tsibble::yearmonth(eventDate)) %>% 
  dplyr::summarise(observations = dplyr::n_distinct(occurrenceID)) %>% 
  tsibble::fill_gaps() %>% 
  dplyr::mutate(observations = forecast::na.interp(observations)) %>% 
  feasts::gg_subseries() + 
  ggplot2::geom_point() +
  ggplot2::labs(x = "", y = "Number of observations") +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle = 90))

ggplot2::ggsave("output/seasonal_subseries.jpg",
                dpi = 1200,
                bg = "white")

# Seasonal plot for Agaricomycetes only ----------------------------------------

# inat %>% 
#   dplyr::filter(class == "Agaricomycetes",
#                 !is.na(eventDate),
#                 eventDate > "2018-01-01" & eventDate < "2022-01-01") %>% 
#   tsibble::as_tsibble(index = eventDate,
#                       key = occurrenceID) %>% 
#   tsibble::index_by(week = tsibble::yearweek(eventDate)) %>% 
#   dplyr::summarise(observations = dplyr::n_distinct(occurrenceID)) %>% 
#   tsibble::fill_gaps() %>% 
#   dplyr::mutate(observations = forecast::na.interp(observations)) %>% 
#   feasts::gg_season() + 
#   ggplot2::theme_minimal() +
#   ggplot2::theme(legend.position = "bottom")

# Seasonal-trend decomposition -------------------------------------------------

# inat_month %>%  
#   tsibble::fill_gaps() %>% 
#   dplyr::mutate(observations = forecast::na.interp(observations)) %>% 
#   fabletools::model(feasts::STL(
#     observations ~ trend(window = 30) + season(period = "year"))
#   ) %>%
#   fabletools::components() %>%
#   feasts::autoplot() +
#   ggplot2::theme_minimal() +
#   ggplot2::scale_colour_manual(values = c("darkblue", "red"),
#                                labels = c("All observations",
#                                           "Research Grade observations")) +
#   ggplot2::theme(legend.position = "bottom",
#                  legend.direction = "vertical",
#                  legend.title = ggplot2::element_blank())
