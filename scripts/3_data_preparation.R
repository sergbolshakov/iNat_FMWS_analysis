library(magrittr)

# Load data from cache --------------------------------------------------------- 

simpleCache::loadCaches("inat_raw", cacheDir = "temp")
simpleCache::simpleCache("inat_gtax", cacheDir = "cache")

# Assign users' logins their real names ----------------------------------------

inat_logins <- 
  c("adelinka" = "Adelina Katargulova",
    #"adelmyagkova"
    "aleks86" = "Alexandra Mingalimova",
    "aleksandraesengeldenova" = "Alexandra Esengeldenova",
    "alemts" = "Alexander Emtsev",
    "alexandravasina" = "Alexandra Vasina",
    "alexey25" = "Alexei Moseevskii",
    "allaverkhozina" = "Alla Verkhozina",
    "ana_lu" = "Anastasia Lukinykh",
    "anastasiavlasenko" = "Anastasia Vlasenko",
    "andreyefremov" = "Andrei Efremov",
    "antonrezvyi" = "Anton Rezvyi",
    "asfav" = "Alexandra Filippova",
    "birdchuvashia" = "Alexander Yakovlev",
    "borisbolshakov" = "Boris Bolshakov",
    "captain_lavrushka" = "Vasilii Dudka",
    "caseymclowe" = "Ksenia Atuchina",
    "convallaria1128" = "Dmitrii Bochkov",
    "dakileno" = "Maria Ryazantseva",
    "dariagassman" = "Daria Gassman",
    "diogeno" = "Sergei Kvashnin",
    "eadavydov" = "Eugene Davydov",
    "elenabutunina" = "Elena Butunina",
    "epopov" = "Eugene Popov",
    "g_bushmakova" = "Galina Bushmakova",
    "goncholgaj" = "Olga Goncharova",
    "igor_kuzmin" = "Igor Kuzmin",
    "ivankaramelkin" = "Dmitry Ageev",
    "jameskm" = "James Mitchell",
    "johnplischke" = "John Plischke",
    "juhakinnunen" = "Juha Kinnunen",
    "julia_shner" = "Iulia Shner",
    "kastani" = "Ruslan Nurhanov",
    "khalilova_ekaterina" = "Ekaterina Khalilova",
    "kim_potapov" = "Kim Potapov",
    #"kne"
    #"kohab"
    "lioncbc" = "Vyacheslav Vlasenko",
    "ludmila2" = "Liudmila Usenko",
    "lyudmilal" = "Liudmila Federova",
    "marasmius" = "Elena Zviagina",
    "mayacw" = "Maya Zhitkova",
    "milakalinina" = "Liudmila Kalinina",
    "natalia_gamova" = "Natalia Gamova",
    "natalyakorotkikh" = "Natalia Korotkikh",
    "naturalist19584" = "Marina Levasheva",
    "nikolai_nakonechnyi" = "Nikolai Nakonechnyi",
    "ninacourlee" = "Nina Filippova",
    "numto_86" = "Tatiana Chupag",
    #"pandabanda"
    "saranchine" = "Eugene Saranchin",
    "sbolshakov" = "Sergei Bolshakov",
    "sofya_hm" = "Sofia Romanova",
    "svetlanakhanty" = "Svetlana Tsarakhova",
    "svg52" = "Sergei Gerasimov",
    "tls-60" = "Tatiana Strus",
    "tmaximo" = "Massimo Tabone",
    "trh_blue" = "Tchaylet Handel",
    "tsvetasheva" = "Tatiana Svetasheva",
    "urmansky" = "Alexander Korepanov",
    "vaglazunov" = "Valerii Glazunov",
    "veksha" = "Ksenia Stepanova",
    "viktoriabilous" = "Viktoria Bilous",
    "woodmen19" = "Vladimir Briukhov",
    "yurii_basov" = "Yurii Basov",
    "yury_rebriev" = "Yurii Rebriev",
    "zukan" = "Alexander Zukanov")

# Prepare the iNaturalist data for further analyses ----------------------------

# Unnest the nested list of verbose data obtained via API,
# selectively pull out the components concerning identifications information,
# replace the logins of naturalists with their real names

inat_idents <- 
  inat_raw %>% 
  tibble::tibble(json = .) %>% 
  tidyr::unnest_wider(json) %>% 
  tidyr::hoist(taxon, scientific_name = "name") %>%
  tidyr::hoist(identifications, taxon_name = c("taxon", "name")) %>%
  tidyr::hoist(identifications, taxon_rank = c("taxon", "rank")) %>% 
  tidyr::hoist(identifications, current = "current") %>% 
  tidyr::hoist(identifications, category = "category") %>% 
  tidyr::hoist(identifications, identifier = c("user", "login")) %>% 
  tidyr::hoist(identifications, datetime = "updated_at") %>% 
  dplyr::select(id,
                quality_grade,
                identifications_count,
                scientific_name,
                taxon_name,
                taxon_rank,
                current,
                category,
                identifier,
                datetime) %>% 
  tidyr::unnest_longer(col = c(taxon_name,
                               taxon_rank,
                               current,
                               category,
                               identifier,
                               datetime)) %>% 
  dplyr::mutate(
    dplyr::across(identifier, ~ stringr::str_replace_all(., inat_logins)),
    identified_on = lubridate::as_date(lubridate::ymd_hms(datetime)),
  ) %>% 
  dplyr::select(-datetime) %>% 
  simpleCache::simpleCache("inat_idents", ., cacheDir = "cache")

# Merge data imported from csv and combined with GBIF taxonomy
# with data on identifications,
# rename the fields to DarwinCore terms (where possible),
# replace the logins of naturalists with their real names

inat_temp <- 
  inat_idents %>% 
  dplyr::filter(current == "TRUE",
                quality_grade == "research",
                taxon_rank == "species") %>% 
  dplyr::mutate(
    identifiedBy = dplyr::case_when(
      scientific_name == taxon_name & category == "improving" ~ identifier,
      scientific_name == taxon_name & category == "leading" ~ identifier
    ),
    identificationRemarks = dplyr::case_when(
      category == "supporting" ~ identifier
    )) %>% 
  dplyr::group_by(id) %>% 
  dplyr::mutate(
    identificationRemarks = stringr::str_c(
      identificationRemarks[!is.na(identificationRemarks)], collapse = ", ")
  ) %>% 
  dplyr::ungroup(identifiedBy) %>% 
  dplyr::mutate(identificationRemarks = dplyr::case_when(
    identificationRemarks != "" ~ stringr::str_c("verified by: ", identificationRemarks))) %>%
  dplyr::filter(!is.na(identifiedBy)) %>% 
  dplyr::select(id,
                identifiedBy,
                dateIdentified = identified_on,
                identificationRemarks)

inat <- 
  inat_gtax %>% 
  dplyr::left_join(inat_temp,
                   by = c("id" = "id")) %>% 
  dplyr::select(
    occurrenceID = id,
    eventDate = observed_on,
    recordedBy = user_login,
    qualityGrade = quality_grade,
    license,
    associatedReferences = url,
    associatedMedia = image_url,
    occurenceRemarks = description,
    decimalLatitude = latitude,
    decimalLongitude = longitude,
    coordinateUncertaintyInMeters = positional_accuracy,
    country = place_country_name,
    stateProvince = place_admin1_name,
    county = place_admin2_name,
    verbatimLocality = place_guess,
    scientificName:species,
    identifiedBy,
    dateIdentified,
    identificationRemarks) %>% 
  dplyr::mutate(
    decimalLatitude = round(decimalLatitude, 5),
    decimalLongitude = round(decimalLongitude, 5),
    dplyr::across(stateProvince, ~ stringr::str_replace_all(
      ., c("Yamal-Nenets" = "Yamalo-Nenets Autonomous Okrug",
           "Khanty-Mansiy" = "Khanty-Mansi Autonomous Okrug",
           "Tyumen'" = "Tyumen Region")
      )),
    dplyr::across(recordedBy, ~ stringr::str_replace_all(., inat_logins))) %>%
  simpleCache::simpleCache("inat", ., cacheDir = "cache")
