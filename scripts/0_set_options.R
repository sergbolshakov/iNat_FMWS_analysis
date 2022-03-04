# Set cache directory for simpleCache calls ------------------------------------

simpleCache::setCacheDir("cache")

# Memoise functions for API queries --------------------------------------------

# Store cache on disk

disk_cache <- memoise::cache_filesystem("cache/")

# Memoise functions for queries to API Google Sheets, GBIF, and iNaturalist 

occ_data <- memoise::memoise(rgbif::occ_data,
                             cache = disk_cache)

read_sheet <- memoise::memoise(googlesheets4::read_sheet,
                               cache = disk_cache)

name_backbone_checklist <- memoise::memoise(rgbif::name_backbone_checklist,
                                            cache = disk_cache)

name_usage <- memoise::memoise(rgbif::name_usage,
                               cache = disk_cache)

# Configure parallel processing of API queries ---------------------------------

# Plan to resolves futures in multisession strategy

future::plan("multisession",
             workers = parallelly::availableCores())

# Set up progress bar to monitor status of queries

progressr::handlers(
  progressr::handler_progress(
    format = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
    width = 80,
    complete = "+"
  )
)