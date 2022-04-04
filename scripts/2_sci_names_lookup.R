library(magrittr)

# Set cache directory for simpleCache calls ------------------------------------

simpleCache::setCacheDir("cache")

# Load data from cache --------------------------------------------------------- 

simpleCache::loadCaches(c("inat_csv",
                          "lit_bin", "lit_gbif",
                          "redbooks_nws", "redlist_iucn"))

# Memoise function for queries to GBIF Species API -----------------------------

name_backbone_checklist <- 
  memoise::memoise(rgbif::name_backbone_checklist,
                   cache = memoise::cache_filesystem("cache"))

# Write function to get accepted names 
# for all names (including synonyms) obtained with name_backbone_checklist()

get_gbif_accepted_names <- 
  function(gsn) {
    
    # Memoise function for queries to GBIF Species API
    name_usage <- 
      memoise::memoise(rgbif::name_usage,
                       cache = memoise::cache_filesystem("cache/gbif_name_usage"))
    
    # Obtain accepted names for synonyms
    gsn_syn <- 
      gsn %>% 
      dplyr::select(acceptedUsageKey) %>% 
      dplyr::distinct() %>% 
      na.omit() %>%  
      purrr::as_vector() %>% 
      purrr::map(., name_usage) %>%
      purrr::map(~.[[2]]) %>%
      dplyr::bind_rows()
    
    # Link all scientific names to accepted names
    gan <- 
      dplyr::bind_rows(
        # Add accepted names of synonyms to source table
        gsn %>% 
          dplyr::inner_join(gsn_syn %>% 
                              dplyr::select(key,
                                            acceptedNameUsage = scientificName),
                            by = c("acceptedUsageKey" = "key")
          ),
        gsn %>% 
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
    
    return(gan)
  }

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

# Obtain accepted names for all names

inat_gan <- get_gbif_accepted_names(inat_gsn)

# Combine GBIF taxonomy with the original iNaturalist data

inat_gtax <- 
  inat_csv %>% 
  dplyr::left_join(inat_gan,
                   by = c("scientific_name" = "verbatim_name")) %>% 
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

# Obtain accepted names for all names

lit_bin_gan <- get_gbif_accepted_names(lit_bin_gsn)

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

# Obtain accepted names for all names

redbooks_nws_gan <- get_gbif_accepted_names(redbooks_nws_gsn)

redlist_iucn_gan <- get_gbif_accepted_names(redlist_iucn_gsn)

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
