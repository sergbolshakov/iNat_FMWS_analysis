library(magrittr)

# Load data from cache --------------------------------------------------------- 

simpleCache::loadCaches(c("inat", "inat_gtax", "oopt"),
                        cacheDir = "cache")

# Show the distribution of observations by administrative unit -----------------

# Only regions

dplyr::full_join(
  inat %>% 
    dplyr::group_by(stateProvince) %>% 
    dplyr::summarize(observations_all = dplyr::n_distinct(occurrenceID)),
  inat %>% 
    dplyr::filter(qualityGrade == "research") %>%
    dplyr::group_by(stateProvince) %>% 
    dplyr::summarize(observations_rg = dplyr::n_distinct(occurrenceID),
                     species = dplyr::n_distinct(species))
  ) %>% 
  flextable::flextable() %>% 
  flextable::set_header_labels(stateProvince = "Region",
                               observations_all = "Total number of observations",
                               observations_rg = "Number of RG observations",
                               species = "Number of species") %>% 
  flextable::set_table_properties(layout = "autofit") %T>% 
  flextable::save_as_docx(path = "output/geographic_coverage.docx")

# Regions and districts
  
# dplyr::full_join(
#   inat %>%
#     dplyr::group_by(stateProvince, county) %>%
#     dplyr::summarize(observations_all = dplyr::n_distinct(occurrenceID)),
#   inat_tax %>%
#     dplyr::filter(qualityGrade == "research") %>%
#     dplyr::group_by(stateProvince, county) %>%
#     dplyr::summarize(observations_rg = dplyr::n_distinct(occurrenceID),
#                      species = dplyr::n_distinct(species))
#   ) %>%
#   flextable::flextable() %>% 
#   flextable::set_header_labels(stateProvince = "Region",
#                                county = "District",
#                                observations_all = "Total number of observations",
#                                observations_rg = "Number of RG observations",
#                                species = "Number of species") %>% 
#   flextable::set_table_properties(layout = "autofit") %T>% 
#   flextable::save_as_docx(path = "output/geographic_coverage.docx")

# Show the distribution of observations by protected areas ---------------------
  
# Select all the observations made in the protected areas

inat_oopt <- 
  inat %>%  
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude")) %>% 
  sf::st_join(oopt, left = FALSE)

# Output the summary table of observations made in the protected areas

dplyr::full_join(
  inat_oopt %>% 
    sf::st_set_geometry(NULL) %>% 
    dplyr::group_by(oopt_status, oopt_category, oopt_name) %>% 
    dplyr::summarize(observations_all = dplyr::n_distinct(occurrenceID)),
  inat_oopt %>% 
    sf::st_set_geometry(NULL) %>% 
    dplyr::filter(qualityGrade == "research") %>% 
    dplyr::group_by(oopt_status, oopt_category, oopt_name) %>% 
    dplyr::summarize(observations_rg = dplyr::n_distinct(occurrenceID),
                     species = dplyr::n_distinct(species))
  ) %>% 
  dplyr::mutate(oopt_name = stringi::stri_trans_general(oopt_name,
                                                        "russian-latin/bgn")) %>%
  flextable::flextable() %>% 
  flextable::set_header_labels(oopt_status = "Protected area status",
                               oopt_category = "Protected area category",
                               oopt_name = "Protected area name",
                               observations_all = "Number of observations",
                               observations_rg = "Number of RG observations",
                               species = "Number of species") %>% 
  flextable::set_table_properties(layout = "autofit") %T>% 
  flextable::save_as_docx(path = "output/protected_areas.docx")
