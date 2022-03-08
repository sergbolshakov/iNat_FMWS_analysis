library(magrittr)
source("scripts/set_options.R")
simpleCache::loadCaches(c("inat_csv",
                          "lit_bin", "lit_gbif",
                          "redbooks_nws", "redlist_iucn"))

# Get GBIF taxonomy for iNaturalist data ---------------------------------------

# Lookup scientific names in the GBIF Backbone Taxonomy

# name_backbone_checklist() is memoised rgbif::name_backbone_checklist()

inat_gsn <- 
  inat_csv %>% 
  dplyr::select(name = scientific_name,
                kingdom = taxon_kingdom_name,
                phylum = taxon_phylum_name,
                class = taxon_class_name,
                order = taxon_order_name,
                family = taxon_family_name,
                genus = taxon_genus_name) %>%  
  dplyr::distinct() %>%  
  dplyr::arrange(name) %>% 
  name_backbone_checklist()

# Show fuzzy matches

inat_gsn %>%  
  dplyr::filter(matchType != "EXACT") %>%  
  dplyr::group_by(matchType) %>%  
  dplyr::select(verbatim_name, status, matchType, scientificName) %>% 
  dplyr::arrange(matchType, verbatim_name) %>%  
  flextable::flextable() %>% 
  flextable::autofit()

# Obtain accepted names for synonyms

# name_usage() is memoised rgbif::name_usage()

inat_gsn_syn <- 
  inat_gsn %>% 
  dplyr::select(acceptedUsageKey) %>% 
  dplyr::distinct() %>% 
  na.omit() %>%  
  purrr::as_vector() %>% 
  purrr::map(., name_usage) %>%
  purrr::map(~.[[2]]) %>%
  dplyr::bind_rows()

# Link scientific names to accepted names

inat_gan <- 
  dplyr::bind_rows(
    inat_gsn %>% 
      dplyr::inner_join(inat_gsn_syn %>% 
                          dplyr::select(key,
                                        acceptedNameUsage = scientificName),
                        by = c("acceptedUsageKey" = "key")
                        ),
    inat_gsn %>% 
      dplyr::filter(is.na(acceptedUsageKey)) %>%
      dplyr::mutate(acceptedNameUsage = scientificName)
    ) %>% 
  dplyr::arrange(scientificName) %>% 
  dplyr::select(scientificName,
                taxonRank = rank,
                taxonomicStatus = status,
                acceptedNameUsage,
                kingdom, phylum, class, order, family, genus, species,
                verbatim_name, confidence, matchType)

# Combine GBIF taxonomy with the original iNaturalist data

inat_gtax <- 
  inat_csv %>% 
  dplyr::left_join(inat_gan,
                   by = c("scientific_name" = "verbatim_name")
                   ) %>% 
  simpleCache::simpleCache("inat_gtax", .)

# Get GBIF taxonomy for literature data ----------------------------------------

# Lookup names in the BIN literature dataset

# name_backbone_checklist() is memoised rgbif::name_backbone_checklist()

lit_bin_gsn <- 
  lit_bin %>% 
  dplyr::select(scientificName) %>% 
  dplyr::distinct() %>% 
  dplyr::rename(name = scientificName) %>% 
  dplyr::mutate(kingdom = "Fungi") %>% 
  dplyr::arrange(name) %>% 
  name_backbone_checklist()

# Show fuzzy matches

lit_bin_gsn %>% 
  dplyr::filter(matchType != "EXACT") %>% 
  dplyr::group_by(matchType) %>% 
  dplyr::select(verbatim_name, matchType, scientificName) %>% 
  dplyr::arrange(matchType, verbatim_name) %>% 
  flextable::flextable() %>% 
  flextable::autofit()

# Obtain accepted names for synonyms

# name_usage() is memoised rgbif::name_usage()

lit_bin_gsn_syn <- 
  lit_bin_gsn %>% 
  dplyr::select(acceptedUsageKey) %>% 
  dplyr::distinct() %>% 
  na.omit() %>%  
  purrr::as_vector() %>% 
  purrr::map(., name_usage) %>%
  purrr::map(~.[[2]]) %>%
  dplyr::bind_rows()

# Link scientific names to accepted names

lit_bin_gan <- 
  dplyr::bind_rows(
    lit_bin_gsn %>% 
      dplyr::inner_join(lit_bin_gsn_syn %>% 
                          dplyr::select(key,
                                        acceptedNameUsage = scientificName),
                        by = c("acceptedUsageKey" = "key")
      ),
    lit_bin_gsn %>% 
      dplyr::filter(is.na(acceptedUsageKey)) %>%
      dplyr::mutate(acceptedNameUsage = scientificName)
  ) %>% 
  dplyr::arrange(scientificName) %>% 
  dplyr::select(scientificName, 
                taxonRank = rank,
                taxonomicStatus = status,
                acceptedNameUsage,
                kingdom, phylum, class, order, family, genus, species,
                verbatim_name, confidence, matchType)

# Bind taxonomy of BIN and GBIF literature datasets

lit_all_gan <- 
  dplyr::bind_rows(
    lit_gbif %>% 
      dplyr::select(scientificName, taxonRank, taxonomicStatus,
                    acceptedNameUsage = acceptedScientificName,
                    kingdom, phylum, class, order, family, genus, species),
    lit_bin_gan %>% 
      dplyr::select(-(verbatim_name:matchType))
    ) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(scientificName) %>% 
  simpleCache::simpleCache("lit_all_gan", .)

# Get GBIF taxonomy for the Red Books and UICN Red List data -------------------

# Lookup names in the Russian Red Books dataset

# name_backbone_checklist() is memoised rgbif::name_backbone_checklist()

redbooks_nws_gsn <- 
  redbooks_nws %>% 
  dplyr::select(scientificName) %>% 
  dplyr::distinct() %>% 
  dplyr::rename(name = scientificName) %>% 
  dplyr::mutate(kingdom = "Fungi") %>% 
  dplyr::arrange(name) %>% 
  name_backbone_checklist()

# Show fuzzy matches

redbooks_nws_gsn %>% 
  dplyr::filter(matchType != "EXACT") %>% 
  dplyr::group_by(matchType) %>% 
  dplyr::select(verbatim_name, matchType, scientificName) %>% 
  dplyr::arrange(matchType, verbatim_name) %>% 
  flextable::flextable() %>% 
  flextable::autofit()

# Lookup names in the UICN Red List dataset

redlist_iucn_gsn <- 
  redlist_iucn %>% 
  dplyr::select(name = fullName,
                kingdom = kingdomName,
                phylum = phylumName,
                class = className,
                order = orderName,
                family = familyName,
                genus = genusName) %>% 
  dplyr::arrange(name) %>% 
  dplyr::filter(!is.na(name)) %>% 
  name_backbone_checklist()

# Show fuzzy matches

redlist_iucn_gsn %>% 
  dplyr::filter(matchType != "EXACT") %>% 
  dplyr::group_by(matchType) %>% 
  dplyr::select(verbatim_name, matchType, scientificName) %>% 
  dplyr::arrange(matchType, verbatim_name) %>% 
  flextable::flextable() %>% 
  flextable::autofit()

# Obtain accepted names for synonyms

# name_usage() is memoised rgbif::name_usage()

redbooks_nws_gsn_syn <- 
  redbooks_nws_gsn %>% 
  dplyr::select(acceptedUsageKey) %>% 
  dplyr::distinct() %>% 
  na.omit() %>%  
  purrr::as_vector() %>% 
  purrr::map(., name_usage) %>%
  purrr::map(~.[[2]]) %>%
  dplyr::bind_rows()

redlist_iucn_gsn_syn <- 
  redlist_iucn_gsn %>% 
  dplyr::select(acceptedUsageKey) %>% 
  dplyr::distinct() %>% 
  na.omit() %>%  
  purrr::as_vector() %>% 
  purrr::map(., name_usage) %>%
  purrr::map(~.[[2]]) %>%
  dplyr::bind_rows()

# Link scientific names to accepted names

redbooks_nws_gan <- 
  dplyr::bind_rows(
    redbooks_nws_gsn %>% 
      dplyr::inner_join(redbooks_nws_gsn_syn %>% 
                          dplyr::select(key,
                                        acceptedNameUsage = scientificName),
                        by = c("acceptedUsageKey" = "key")
      ),
    redbooks_nws_gsn %>% 
      dplyr::filter(is.na(acceptedUsageKey)) %>%
      dplyr::mutate(acceptedNameUsage = scientificName)
  ) %>% 
  dplyr::arrange(scientificName) %>% 
  dplyr::select(scientificName,
                taxonRank = rank,
                taxonomicStatus = status,
                acceptedNameUsage,
                kingdom, phylum, class, order, family, genus, species,
                verbatim_name, confidence, matchType)

redlist_iucn_gan <- 
  dplyr::bind_rows(
    redlist_iucn_gsn %>% 
      dplyr::inner_join(redlist_iucn_gsn_syn %>% 
                          dplyr::select(key,
                                        acceptedNameUsage = scientificName),
                        by = c("acceptedUsageKey" = "key")
      ),
    redlist_iucn_gsn %>% 
      dplyr::filter(is.na(acceptedUsageKey)) %>%
      dplyr::mutate(acceptedNameUsage = scientificName)
  ) %>% 
  dplyr::arrange(scientificName) %>% 
  dplyr::select(scientificName,
                taxonRank = rank,
                taxonomicStatus = status,
                acceptedNameUsage,
                kingdom, phylum, class, order, family, genus, species,
                verbatim_name, confidence, matchType)

# Combine GBIF taxonomy with the original Russian Red Books data

redbooks_nws_gtax <- 
  redbooks_nws %>% 
  dplyr::select(-acceptedName) %>% 
  dplyr::rename(verbatim_name = scientificName) %>% 
  dplyr::left_join(redbooks_nws_gan) %>% 
  simpleCache::simpleCache("redbooks_nws_gtax", .)

# Combine GBIF taxonomy with the original UICN Red List data,
# filter taxa with "Least Concern" status

redlist_iucn_gtax <- 
  redlist_iucn %>% 
  dplyr::select(redlistCategory:fullName) %>% 
  dplyr::rename(verbatim_name = fullName) %>% 
  dplyr::left_join(redlist_iucn_gan) %>% 
  dplyr::filter(!is.na(species),
                redlistCategory != "Least Concern") %>% 
  simpleCache::simpleCache("redlist_iucn_gtax", .)
