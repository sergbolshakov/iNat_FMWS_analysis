library(magrittr)

# Set cache directory for simpleCache calls ------------------------------------

simpleCache::setCacheDir("cache")

# Memoise functions for queries to iNaturalist, GBIF, and Google Sheets API ----

get_inat_obs_id <- memoise::memoise(rinat::get_inat_obs_id,
                                    cache = memoise::cache_filesystem("temp/inat_obs"))

occ_data <- memoise::memoise(rgbif::occ_data,
                             cache = memoise::cache_filesystem("cache/"))

read_sheet <- memoise::memoise(googlesheets4::read_sheet,
                               cache = memoise::cache_filesystem("cache/"))

# Set up progress bar to monitor status of queries to iNaturalist API ----------

progressr::handlers(
  progressr::handler_progress(
    format = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
    width = 80,
    complete = "+"
  )
)

# Import the iNaturalist data --------------------------------------------------

# Data exported on 2022-02-24 from iNaturalist website by query 
# "quality_grade=any&identifications=any&projects[]=fungi-and-myxomycetes-in-northern-west-siberia"
# (choosed all columns)

inat_csv <- readr::read_csv("data/observations-215002.csv.zip",
                            guess_max = 16000) %>% 
  purrr::discard(~ all(is.na(.))) %>% 
  simpleCache::simpleCache("inat_csv", .)

# Request full вфеф on project observations via the iNaturalist API
# for information on identifications

# Be careful! This request will take a long time (more than 6 hours).
# The resulting file will be large (1.6 GB)

# get_inat_obs_id() is memoised rinat::get_inat_obs_id()

inat_ids <- inat_csv$id

inat_raw <- vector(mode = "list",
                   length = length(inat_ids))

progressr::with_progress({
  p <- progressr::progressor(steps = length(inat_ids))
  for(i in seq_along(inat_ids)) {
    p()
    Sys.sleep(.2)
    inat_raw[[i]] <- get_inat_obs_id(id = inat_ids[i])
    if(inat_ids[i] %in% inat_ids[seq_along(inat_ids) %% 60 == 0]) {
      cat(".")
      Sys.sleep(30)
    }
  }
})

simpleCache::storeCache("inat_raw", cacheDir = "temp")

# Import the literature data ---------------------------------------------------

# Download literature data published via GBIF by dataset uuids
# using GBIF Occurrence API 

# occ_data() is memoised rgbif::occ_data()

lit_gbif_1 <- occ_data(datasetKey = "29e78377-34c3-4c91-8062-550069a92b70",
                       limit = 23000)
lit_gbif_2 <- occ_data(datasetKey = "dd7d031f-b18f-4631-929a-049fcf00ac8f",
                       limit = 8000)

lit_gbif <- dplyr::bind_rows(lit_gbif_1$data,
                             lit_gbif_2$data) %>%
  dplyr::mutate(dplyr::across(
    stateProvince,
    stringr::str_replace, "Tyumen oblast", "Tyumen Oblast")
    ) %>% 
  dplyr::filter(stateProvince %in% c("Yamalo-Nenets Autonomous Okrug",
                                     "Khanty-Mansiyskiy Avtonomnyy Okrug",
                                     "Tyumen Oblast")
                ) %>%
  simpleCache::simpleCache("lit_gbif", .)

# Import data from literature database of the Komarov Botanical Institute

# The database is under active development and has not yet been published,
# so subset for only studied regions made in advance specifically for this project

load("data/bin_lit_db_nws_subset.RData")

simpleCache::storeCache("lit_bin", cacheDir = "cache")

# Import geospatial data of the protected areas boundaries ---------------------

oopt <- 
  dplyr::bind_rows(
    sf::read_sf("data/NWS_OOPT.gpkg", layer = "Yamalo-Nenets") %>% 
      dplyr::rename(name = "NAME"),
    sf::read_sf("data/NWS_OOPT.gpkg", layer = "Khanty-Mansi"),
    sf::read_sf("data/NWS_OOPT.gpkg", layer = "Tyumen")
  ) %>% 
  dplyr::filter(!is.na(category)) %>% 
  dplyr::rename(oopt_name = "name",
                oopt_cluster = "cluster",
                oopt_status = "status",
                oopt_category = "category",
                oopt_type = "type") %>% 
  dplyr::mutate(
    oopt_status = stringr::str_replace_all(
      string = oopt_status,
      stringr::coll(c("региональный" = "Regional",
                      "федеральный" = "Federal")
      )
    ),
    oopt_category = stringr::str_replace_all(
      string = oopt_category,
      stringr::coll(c("заповедник" = "State Nature Reserve",
                      "национальный парк" = "National Park",
                      "природный парк" = "Natural Park",
                      "заказник" = "Sanctuary",
                      "памятник природы" = "Natural Monument")
      )
    )
  ) %>% 
  simpleCache::simpleCache("oopt", .)

# Import data on fungi included in the Russian Red Books -----------------------

# read_sheet() is memoised googlesheets4::read_sheet()

googlesheets4::gs4_deauth()

redbooks_nws <- 
  read_sheet("19p9PKcaqLryYP_g0Mcl1fRikDP_YNZzbfQxB4fVTK-A",
             "Data") %>% 
  dplyr::filter(region %in% c("_Russian Federation",
                              "Yamalo-Nenets Autonomous Okrug",
                              "Khanty-Mansi Autonomous Okrug",
                              "Tyumen Oblast")
                ) %>% 
  dplyr::arrange(region) %>% 
  simpleCache::simpleCache("redbooks_nws", .)

# Import data on fungi included in The IUCN Red List of Threatened Species. Version 2021-3
# pre-downloaded by a registered user

redlist_iucn <- 
  readr::read_csv("data/redlist_species_data_0adf3dd0-2993-4156-8501-832b7e017f0e.zip") %>%  
  purrr::discard(~ all(is.na(.))) %>% 
  dplyr::mutate(authority = stringr::str_replace_all(authority, "&amp;", "&")) %>%  
  dplyr::mutate(fullName = stringr::str_c(scientificName, authority, sep = " ")) %>% 
  simpleCache::simpleCache("redlist_iucn", .)
