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
    id_imp = dplyr::n_distinct(id[category == "improving"]),
    ) %>%
  dplyr::ungroup() %>% 
  dplyr::bind_rows(
    dplyr::filter(., id_prc <= 1.00) %>%
      dplyr::summarize(id_all = sum(id_all),
                       id_cur = sum(id_cur),
                       id_prc = sum(id_prc),
                       id_imp = sum(id_imp)) %>%
      dplyr::mutate(identifier = "Others")
  ) %>%
  dplyr::filter(id_prc > 1.00)
  
# Show distribution of the identifiers' effort ---------------------------------

ggplot2::ggplot(inat_identifiers,
                ggplot2::aes(x = id_cur, y = identifier)) +
  ggplot2::geom_segment(ggplot2::aes(x = 0, xend = id_cur,
                                     y = identifier, yend = identifier),
                        colour = "darkblue") +
  ggplot2::geom_point(size = 3, colour = "darkblue") +
  ggplot2::scale_y_discrete(
    name = "",
    limits = c(inat_identifiers %>%
                 dplyr::filter(identifier == "Others") %>%
                 dplyr::pull(identifier),
               inat_identifiers %>%
                 dplyr::filter(identifier != "Others") %>%
                 dplyr::arrange(id_cur) %>%
                 dplyr::pull(identifier)
               )) +
  ggplot2::geom_text(ggplot2::aes(label = paste0(id_prc, "%"), hjust = -0.2)) +
  ggplot2::scale_x_continuous(name = "Number of identifications",
                              n.breaks = 10, limits = c(0, 14000)) +
  ggplot2::theme_minimal()

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
  ggplot2::geom_text(ggplot2::aes(label = observations, hjust = -0.5)) +
  ggplot2::theme_minimal()

ggplot2::ggsave("output/identifications_counts.jpg",
                dpi = 1200,
                bg = "white")

# Show genera identifiability --------------------------------------------------

inat %>% 
  dplyr::group_by(genus) %>% 
  dplyr::summarize(obs_all = dplyr::n_distinct(occurrenceID),
                   obs_rg = dplyr::n_distinct(occurrenceID[qualityGrade == "research"])) %>% 
  dplyr::filter(!is.na(genus),
                obs_all > 100) %>% 
  tidyr::pivot_longer(cols = c(obs_all, obs_rg),
                      names_to = "variables",
                      values_to = "values") %>% 
  ggplot2::ggplot(ggplot2::aes(x = values, y = genus,
                               fill = variables)) +
  ggplot2::geom_bar(position = ggplot2::position_fill(), stat = "identity") +
  ggplot2::scale_x_continuous(labels = scales::percent_format())
