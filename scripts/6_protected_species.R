library(magrittr)
source("scripts/0_set_options.R")
simpleCache::loadCaches(c("inat", "redbooks_nws_gtax", "redlist_iucn_gtax"))

# Select observations of red-listed species ------------------------------------

# Select observations of species in IUCN Red List Version 2021-3

inat %>% 
  dplyr::filter(qualityGrade == "research",
                acceptedNameUsage %in% redlist_iucn_gtax$acceptedNameUsage) %>%
  dplyr::left_join(redlist_iucn_gtax,
                   by = c("acceptedNameUsage" = "acceptedNameUsage")) %>% 
  dplyr::group_by(acceptedNameUsage, stateProvince) %>% 
  dplyr::mutate(observations = dplyr::n_distinct(occurrenceID)) %>%
  dplyr::select(acceptedNameUsage,
                redlistCategory,
                stateProvince,
                observations) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(acceptedNameUsage) %>% 
  flextable::flextable() %>% 
  flextable::set_table_properties(layout = "autofit") %T>% 
  flextable::save_as_docx(path = "output/redlist_iucn.docx")

# Select observations of species in Red Books of the Russian Federation (2008)

redbook_rf <- 
  redbooks_nws_gtax %>% 
  dplyr::filter(region == "_Russian Federation") %>% 
  dplyr::pull(acceptedNameUsage)

inat %>% 
  dplyr::filter(qualityGrade == "research",
                acceptedNameUsage %in% redbook_rf) %>%
  dplyr::left_join(redbooks_nws_gtax %>% 
                     dplyr::filter(region == "_Russian Federation"),
                   by = c("acceptedNameUsage" = "acceptedNameUsage")) %>% 
  dplyr::group_by(acceptedNameUsage, stateProvince) %>% 
  dplyr::mutate(observations = dplyr::n_distinct(occurrenceID)) %>%
  dplyr::select(acceptedNameUsage,
                list,
                stateProvince,
                observations) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(acceptedNameUsage) %>% 
  flextable::flextable() %>% 
  flextable::autofit() %>% 
  flextable::set_table_properties(layout = "autofit") %T>% 
  flextable::save_as_docx(path = "output/redbook_rf.docx")

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
  dplyr::mutate(observations = dplyr::n_distinct(occurrenceID)) %>%
  dplyr::left_join(redbooks_nws_gtax %>% 
                     dplyr::filter(region == "Yamalo-Nenets Autonomous Okrug"),
                   by = c("acceptedNameUsage" = "acceptedNameUsage")) %>% 
  dplyr::select(acceptedNameUsage,
                stateProvince,
                list,
                observations) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(acceptedNameUsage)

inat_redbook_kmao <- 
  inat %>% 
  dplyr::filter(qualityGrade == "research",
                stateProvince == "Khanty-Mansi Autonomous Okrug",
                acceptedNameUsage %in% redbook_kmao) %>%
  dplyr::group_by(acceptedNameUsage) %>% 
  dplyr::mutate(observations = dplyr::n_distinct(occurrenceID)) %>%
  dplyr::left_join(redbooks_nws_gtax %>% 
                     dplyr::filter(region == "Khanty-Mansi Autonomous Okrug"),
                   by = c("acceptedNameUsage" = "acceptedNameUsage")) %>% 
  dplyr::select(acceptedNameUsage,
                stateProvince,
                list,
                observations) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(acceptedNameUsage)

inat_redbook_to <- 
  inat %>% 
  dplyr::filter(qualityGrade == "research",
                stateProvince == "Tyumen Region",
                acceptedNameUsage %in% redbook_to) %>%
  dplyr::group_by(acceptedNameUsage) %>% 
  dplyr::mutate(observations = dplyr::n_distinct(occurrenceID)) %>%
  dplyr::left_join(redbooks_nws_gtax %>% 
                     dplyr::filter(region == "Tyumen Oblast"),
                   by = c("acceptedNameUsage" = "acceptedNameUsage")) %>% 
  dplyr::select(acceptedNameUsage,
                stateProvince,
                list,
                observations) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(acceptedNameUsage)

dplyr::bind_rows(inat_redbook_ynao,
                 inat_redbook_kmao,
                 inat_redbook_to) %>% 
  flextable::flextable() %>% 
  flextable::set_table_properties(layout = "autofit") %T>% 
  flextable::save_as_docx(path = "output/redbooks_nws.docx")
