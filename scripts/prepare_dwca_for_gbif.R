library(magrittr)

# Load data from cache --------------------------------------------------------- 

simpleCache::loadCaches(c("inat", "inat_new_species"), cacheDir = "cache")

# Prepare datable to publication through GBIF ----------------------------------
# https://doi.org/10.15468/yjdyam

inat_new_species %>% 
  dplyr::mutate(dynamicProperties = stringr::str_c(
    '"noveltyStatus":"new for "', stateProvince, '"')) %>% 
  dplyr::select(occurrenceID, dynamicProperties) %>% 
  dplyr::right_join(inat,
                    by = c("occurrenceID" = "occurrenceID")) %>%
  dplyr::mutate(language = "en | ru",
                rightsHolder = recordedBy,
                basisOfRecord = "HumanObservation",
                countryCode = "RU",
                geodeticDatum = "WGS84") %>% 
  dplyr::select(language,
                license,
                rightsHolder,
                basisOfRecord,
                dynamicProperties,
                occurrenceID,
                recordedBy,
                associatedMedia,
                associatedReferences,
                occurrenceRemarks,
                eventDate,
                country,
                countryCode,
                stateProvince,
                county,
                verbatimLocality,
                decimalLatitude,
                decimalLongitude,
                geodeticDatum,
                coordinateUncertaintyInMeters,
                scientificName:identificationRemarks) %>% 
  readr::write_csv("output/dwca_occurrence.csv",
                   na = "",
                   quote = "needed",
                   escape = "double")
