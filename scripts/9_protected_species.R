library(magrittr)

# Load data from cache --------------------------------------------------------- 

simpleCache::loadCaches(c("inat",
                          "redbooks_nws_gtax", "redlist_iucn_gtax"),
                        cacheDir = "cache")

# Select observations of red-listed species ------------------------------------

# Select observations of species in IUCN Red List Version 2021-3

inat_iucn <- 
  inat %>% 
  dplyr::filter(qualityGrade == "research",
                acceptedNameUsage %in% redlist_iucn_gtax$acceptedNameUsage) %>%
  dplyr::left_join(redlist_iucn_gtax,
                   by = c("acceptedNameUsage" = "acceptedNameUsage")) %>% 
  dplyr::group_by(acceptedNameUsage, stateProvince) %>% 
  dplyr::mutate(list = "IUCN") %>% 
  dplyr::select(acceptedNameUsage,
                stateProvince,
                list,
                status = redlistCategory,
                decimalLatitude,
                decimalLongitude,
                eventDate,
                recordedBy,
                associatedReferences) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(acceptedNameUsage)

# Select observations of species in Red Books of the Russian Federation (2008)

redbook_rf <- 
  redbooks_nws_gtax %>% 
  dplyr::filter(region == "_Russian Federation") %>% 
  dplyr::pull(acceptedNameUsage)

inat_redbook_rf <- 
  inat %>% 
  dplyr::filter(qualityGrade == "research",
                acceptedNameUsage %in% redbook_rf) %>%
  dplyr::left_join(redbooks_nws_gtax %>% 
                     dplyr::filter(region == "_Russian Federation"),
                   by = c("acceptedNameUsage" = "acceptedNameUsage")) %>% 
  dplyr::group_by(acceptedNameUsage, stateProvince) %>% 
  dplyr::mutate(list = "RB RF") %>%
  dplyr::select(acceptedNameUsage,
                stateProvince,
                list,
                status,
                decimalLatitude,
                decimalLongitude,
                eventDate,
                recordedBy,
                associatedReferences) %>% 
  dplyr::mutate(status = dplyr::case_when(
    is.na(status) ~ "monitored",
    TRUE ~ status
  )) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(acceptedNameUsage)

# Select observations of species in the regional Russian Red Books

redbook_ynao <- 
  redbooks_nws_gtax %>% 
  dplyr::filter(region == "Yamalo-Nenets Autonomous Okrug") %>% 
  dplyr::pull(acceptedNameUsage)

redbook_kmao <- 
  redbooks_nws_gtax %>% 
  dplyr::filter(region == "Khanty-Mansi Autonomous Okrug") %>% 
  dplyr::pull(acceptedNameUsage)

redbook_to <- 
  redbooks_nws_gtax %>% 
  dplyr::filter(region == "Tyumen Oblast") %>% 
  dplyr::pull(acceptedNameUsage)

inat_redbook_ynao <- 
  inat %>% 
  dplyr::filter(qualityGrade == "research",
                stateProvince == "Yamalo-Nenets Autonomous Okrug",
                acceptedNameUsage %in% redbook_ynao) %>%
  dplyr::group_by(acceptedNameUsage) %>% 
  dplyr::mutate(list = "RB YNAO") %>%
  dplyr::left_join(redbooks_nws_gtax %>% 
                     dplyr::filter(region == "Yamalo-Nenets Autonomous Okrug"),
                   by = c("acceptedNameUsage" = "acceptedNameUsage")) %>% 
  dplyr::select(acceptedNameUsage,
                stateProvince,
                list = list.x,
                status,
                decimalLatitude,
                decimalLongitude,
                eventDate,
                recordedBy,
                associatedReferences) %>% 
  dplyr::mutate(status = dplyr::case_when(
    is.na(status) ~ "monitored",
    TRUE ~ status
  )) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(acceptedNameUsage)

inat_redbook_kmao <- 
  inat %>% 
  dplyr::filter(qualityGrade == "research",
                stateProvince == "Khanty-Mansi Autonomous Okrug",
                acceptedNameUsage %in% redbook_kmao) %>%
  dplyr::group_by(acceptedNameUsage) %>% 
  dplyr::mutate(list = "RB KMAO") %>%
  dplyr::left_join(redbooks_nws_gtax %>% 
                     dplyr::filter(region == "Khanty-Mansi Autonomous Okrug"),
                   by = c("acceptedNameUsage" = "acceptedNameUsage")) %>% 
  dplyr::select(acceptedNameUsage,
                stateProvince,
                list = list.x,
                status,
                decimalLatitude,
                decimalLongitude,
                eventDate,
                recordedBy,
                associatedReferences) %>% 
  dplyr::mutate(status = dplyr::case_when(
    is.na(status) ~ "monitored",
    TRUE ~ status
  )) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(acceptedNameUsage)

inat_redbook_to <- 
  inat %>% 
  dplyr::filter(qualityGrade == "research",
                stateProvince == "Tyumen Region",
                acceptedNameUsage %in% redbook_to) %>%
  dplyr::group_by(acceptedNameUsage) %>% 
  dplyr::mutate(list = "RB TO") %>%
  dplyr::left_join(redbooks_nws_gtax %>% 
                     dplyr::filter(region == "Tyumen Oblast"),
                   by = c("acceptedNameUsage" = "acceptedNameUsage")) %>% 
  dplyr::select(acceptedNameUsage,
                stateProvince,
                list = list.x,
                status,
                decimalLatitude,
                decimalLongitude,
                eventDate,
                recordedBy,
                associatedReferences) %>% 
  dplyr::mutate(status = dplyr::case_when(
    is.na(status) ~ "monitored",
    TRUE ~ status
  )) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(acceptedNameUsage)

inat_protected_species <- 
  dplyr::bind_rows(inat_iucn,
                   inat_redbook_rf,
                   inat_redbook_ynao,
                   inat_redbook_kmao,
                   inat_redbook_to) %>% 
  dplyr::mutate(
    coordinates = stringr::str_c(format(decimalLatitude, nsmall = 5), "° N, ",
                                 format(decimalLongitude, nsmall = 5), "° E"),
    decimalLatitude = NULL,
    decimalLongitude = NULL,
    eventDate = format(eventDate, "%d.%m.%Y")
  ) %>% 
  dplyr::group_by(acceptedNameUsage, stateProvince) %>% 
  dplyr::mutate(
    obs_number = dplyr::n_distinct(associatedReferences),
    obs_info = dplyr::case_when(
      obs_number > 3 ~ stringr::str_c(obs_number, " observations"),
      TRUE ~ stringr::str_c(coordinates, eventDate, recordedBy, associatedReferences,
                            sep = ", ")
    )) %>% 
  dplyr::select(-(eventDate:obs_number)) %>% 
  dplyr::distinct() %>%
  dplyr::group_by(acceptedNameUsage, stateProvince, list, status) %>% 
  dplyr::summarize(obs_info = stringr::str_c(obs_info, collapse = "; ")) %>% 
  dplyr::arrange(acceptedNameUsage,
                 factor(list, levels = c("IUCN",
                                         "RB RF",
                                         "RB YNAO",
                                         "RB KMAO",
                                         "RB TO")
                 )) %>% 
  dplyr::group_by(acceptedNameUsage, stateProvince, obs_info) %>% 
  dplyr::summarize(lists = stringr::str_c(
    stringr::str_c(list, " (", status, ")"), collapse = "; ")) %>% 
  dplyr::group_by(acceptedNameUsage) %>% 
  dplyr::summarize(regions = stringr::str_c(
    acceptedNameUsage, " — ", lists, ": ", stateProvince, " (", obs_info, ").")
  ) %>% 
  dplyr::pull()

inat_protected_species_doc <- officer::read_docx(path = "data/output_style.docx")

for(i in seq_along(inat_protected_species)) {
  inat_new_species_doc <- officer::body_add_par(inat_protected_species_doc,
                                                inat_protected_species[[i]])
}

print(inat_protected_species_doc, target = "output/protected_species.docx")
