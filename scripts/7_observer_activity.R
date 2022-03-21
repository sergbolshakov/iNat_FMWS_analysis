library(magrittr)

# Load data from cache --------------------------------------------------------- 

simpleCache::loadCaches("inat", cacheDir = "cache")

# Total observers --------------------------------------------------------------

dplyr::n_distinct(inat$recordedBy)

# Summarize observations by observers ------------------------------------------

inat_observers <- 
  inat %>% 
  dplyr::group_by(recordedBy) %>% 
  dplyr::summarize(
    observations = dplyr::n_distinct(occurrenceID),
    percentage = round((observations / length(inat$occurrenceID) * 100), digits = 2)
    ) %>% 
  dplyr::ungroup() %>% 
  dplyr::bind_rows(
    dplyr::filter(., percentage <= 1.00) %>%
      dplyr::summarize(observations = sum(observations),
                       percentage = sum(percentage)) %>%
      dplyr::mutate(recordedBy = "Others")
  ) %>%
  dplyr::filter(percentage > 1.00)
  
# Show distribution of the observers' effort

ggplot2::ggplot(inat_observers,
                ggplot2::aes(x = observations, y = recordedBy)) +
  ggplot2::geom_point(size = 3, color = "darkblue") +
  ggplot2::geom_segment(ggplot2::aes(x = 0, xend = observations,
                                     y = recordedBy, yend = recordedBy),
                        colour = "darkblue") +
  ggplot2::scale_y_discrete(limits = c(
    inat_observers %>% 
      dplyr::filter(recordedBy == "Others") %>% 
      dplyr::pull(recordedBy),
    inat_observers %>% 
      dplyr::filter(recordedBy != "Others") %>% 
      dplyr::arrange(observations) %>% 
      dplyr::pull(recordedBy)
  )) +
  ggplot2::labs(x = "Number of observations", y = "") +
  ggplot2::geom_text(ggplot2::aes(label = paste0(percentage, "%"), hjust = -0.2),
                     family = "Times New Roman") +
  ggplot2::xlim(0, 4900) +
  ggplot2::theme_minimal() +
  ggplot2::theme(text = ggplot2::element_text(family = "Times New Roman",
                                              face = "bold", size = 12))

ggplot2::ggsave("output/observers_effort.jpg",
                dpi = 1200,
                bg = "white")

# Show distribution among observation and species numbers classes

inat %>% 
  dplyr::group_by(recordedBy) %>% 
  dplyr::summarize(observations = dplyr::n_distinct(occurrenceID),
                   species = dplyr::n_distinct(species)) %>% 
  tidyr::pivot_longer(cols = c(observations, species),
                      names_to = "variables",
                      values_to = "values") %>%
  ggplot2::ggplot(ggplot2::aes(x = values,
                               color = variables,
                               fill = variables)) +
  ggplot2::geom_histogram(position = "dodge") +
  ggplot2::labs(x = "Number of observations / species",
                y = "Number of observers") +
  ggplot2::scale_x_log10() +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom",
                 legend.title = ggplot2::element_blank()) +
  ggplot2::scale_colour_manual(values = c("darkblue", "red")) +
  ggplot2::scale_fill_manual(values = c("lightblue", "white")) +
  ggplot2::theme(text = ggplot2::element_text(family = "Times New Roman",
                                              face = "bold", size = 12))

ggplot2::ggsave("output/observers_histogram.jpg",
                dpi = 1200,
                bg = "white")
  