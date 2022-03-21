library(magrittr)

# Load data from cache --------------------------------------------------------- 

simpleCache::loadCaches(c("inat_idents", "inat"), cacheDir = "cache")

# iNaturalist Data Quality Assessment ------------------------------------------
# https://www.inaturalist.org/pages/help#quality

inat_idents %>% 
  dplyr::group_by(quality_grade) %>% 
  dplyr::summarize(
    observations = dplyr::n_distinct(id),
    percentage = round(observations / dplyr::n_distinct(inat_idents$id) * 100,
                       digits = 2))

# Total identifiers ------------------------------------------------------------

dplyr::n_distinct(inat_idents$identifier)

# Summarize identifications by identifiers -------------------------------------

inat_identifiers <- 
  inat_idents %>% 
  dplyr::group_by(identifier) %>% 
  dplyr::summarise(
    id_all = dplyr::n(),
    id_cur = dplyr::n_distinct(id[current == "TRUE"]),
    id_prc = round((id_cur / length(inat_idents$id) * 100), digits = 2),
    id_imp = dplyr::n_distinct(id[current == "TRUE" & category == "improving"])
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::bind_rows(
    dplyr::filter(., id_cur <= 200) %>%
      dplyr::summarize(id_all = sum(id_all),
                       id_cur = sum(id_cur),
                       id_prc = sum(id_prc),
                       id_imp = sum(id_imp)) %>%
      dplyr::mutate(identifier = "Others")
  ) %>%
  dplyr::filter(id_cur > 200)

# Show distribution of the identifiers' effort ---------------------------------

# Custom breaks for x-axis
base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

# Plot
ggplot2::ggplot(inat_identifiers) +
  ggplot2::geom_segment(ggplot2::aes(x = id_imp, xend = id_cur,
                                     y = identifier, yend = identifier),
                        colour = "darkblue") +
  ggplot2::geom_point(ggplot2::aes(x = id_cur, y = identifier,
                                   colour = "darkblue"),
                      size = 3) +
  ggplot2::geom_point(ggplot2::aes(x = id_imp, y = identifier,
                                   colour = "red"),
                      size = 3) +
  ggplot2::scale_y_discrete(
    name = "",
    limits = c(inat_identifiers %>%
                 dplyr::filter(identifier == "Others") %>%
                 dplyr::pull(identifier),
               inat_identifiers %>%
                 dplyr::filter(identifier != "Others") %>%
                 dplyr::arrange(id_imp) %>%
                 dplyr::pull(identifier)
    )) +
  ggplot2::geom_text(ggplot2::aes(x = id_cur, y = identifier,
                                  label = paste0(id_prc, "%"), hjust = -0.2),
                     family = "Times New Roman",
                     size = 3) +
  ggplot2::scale_x_continuous(name = "Number of identifications",
                              trans = "log10",
                              expand = c(0.1, 0),
                              breaks = base_breaks()) +
  ggplot2::scale_colour_manual(values = c("darkblue", "red"),
                               labels = c("All current identifications",
                                          "Improving current identification")) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom",
                 legend.title = ggplot2::element_blank(),
                 text = ggplot2::element_text(family = "Times New Roman",
                                              face = "bold", size = 12))

ggplot2::ggsave("output/identifiers_effort.jpg",
                dpi = 1200,
                bg = "white")

# Show distributions of the identifications counts -----------------------------

inat_idents %>% 
  dplyr::group_by(identifications_count) %>% 
  dplyr::summarise(observations = dplyr::n_distinct(id)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = observations,
                               y = factor(identifications_count))) +
  ggplot2::geom_segment(ggplot2::aes(x = 0, xend = observations,
                                     y = factor(identifications_count),
                                     yend = factor(identifications_count)),
                        colour = "darkblue") +
  ggplot2::geom_point(size = 3, colour = "darkblue") +
  ggplot2::scale_y_discrete(name = "Count of identifications") +
  ggplot2::scale_x_continuous(name = "Number of observations",
                              trans = "log10",
                              limits = c(1, 15000)) +
  ggplot2::geom_text(ggplot2::aes(label = observations, hjust = -0.5),
                     family = "Times New Roman") +
  ggplot2::theme_minimal() +
  ggplot2::theme(text = ggplot2::element_text(family = "Times New Roman",
                                              face = "bold", size = 12))

ggplot2::ggsave("output/identifications_counts.jpg",
                dpi = 1200,
                bg = "white")

# Show genera identifiability --------------------------------------------------

inat_idfbl <- 
  inat %>% 
  dplyr::group_by(genus) %>% 
  dplyr::summarize(
    obs_all = dplyr::n_distinct(occurrenceID),
    obs_rg = dplyr::n_distinct(occurrenceID[qualityGrade == "research" & !is.na(species)]),
    id_per = round((obs_rg / obs_all), digits = 2)) %>% 
  dplyr::filter(!is.na(genus),
                obs_all > 100)

ggplot2::ggplot(inat_idfbl,
                ggplot2::aes(x = id_per, y = genus)) +
  ggplot2::geom_segment(ggplot2::aes(x = 0, xend = id_per,
                                     y = genus, yend = genus),
                        colour = "darkblue") +
  ggplot2::geom_point(size = 3, colour = "darkblue") +
  ggplot2::scale_y_discrete(limits = c(
    inat_idfbl %>% dplyr::arrange(desc(id_per)) %>% dplyr::pull(genus))
  ) +
  ggplot2::scale_x_continuous(labels = scales::label_percent()) +
  ggplot2::labs(x = "Percentage of Research Grade observations identified to species",
                y = "") +
  ggplot2::theme_minimal() +
  ggplot2::theme(text = ggplot2::element_text(family = "Times New Roman",
                                              face = "bold", size = 12))

ggplot2::ggsave("output/identifiability_genera.jpg",
                dpi = 1200,
                bg = "white")
