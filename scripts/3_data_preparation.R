library(magrittr)

# Load data from cache --------------------------------------------------------- 

simpleCache::simpleCache("inat_gtax", cacheDir = "cache")
simpleCache::loadCaches("inat_raw", cacheDir = "temp")

# Prepare the iNaturalist data for further analyses ----------------------------

# Data imported from csv and combined with GBIF taxonomy

# Rename the fields to DarwinCore terms (where possible),
# replace the logins of naturalists with their real names

inat <- 
  inat_gtax %>% 
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
    scientificName:species) %>% 
  dplyr::mutate(
    decimalLatitude = round(decimalLatitude, 5),
    decimalLongitude = round(decimalLongitude, 5),
    dplyr::across(
      stateProvince,
      ~ stringr::str_replace_all(.,
                                 c("Yamal-Nenets" = "Yamalo-Nenets Autonomous Okrug",
                                   "Khanty-Mansiy" = "Khanty-Mansi Autonomous Okrug",
                                   "Tyumen'" = "Tyumen Region")
      )
    ),
    dplyr::across(
      recordedBy,
      ~ stringr::str_replace_all(.,
                                 c("ninacourlee" = "Nina Filippova",
                                   "tls-60" = "Tatiana Strus",
                                   "viktoriabilous" = "Viktoria Bilous",
                                   "nikolai_nakonechnyi" = "Nikolai Nakonechnyi",
                                   "elenabutunina" = "Elena Butunina",
                                   "igor_kuzmin" = "Igor Kuzmin",
                                   "urmansky" = "Alexander Korepanov",
                                   "natalyakorotkikh" = "Natalia Korotkikh",
                                   "marasmius" = "Elena Zvyagina",
                                   "sofya_hm" = "Sofia Romanova",
                                   "svetlanakhanty" = "Svetlana Tsarakhova",
                                   "yurii_basov" = "Yurii Basov",
                                   "numto_86" = "Tatiana Chupag",
                                   "g_bushmakova" = "Galina Bushmakova",
                                   "alexandravasina" = "Alexandra Vasina",
                                   "diogeno" = "Sergei Kvashnin",
                                   "aleksandraesengeldenova" = "Alexandra Esengeldenova",
                                   "zukan" = "Alexander Zukanov",
                                   "lyudmilal" = "Liudmila Federova",
                                   "birdchuvashia" = "Alexander Yakovlev",
                                   "alexey25" = "Alexei Moseevskii",
                                   "antonrezvyi" = "Anton Rezvyi",
                                   "goncholgaj" = "Olga Goncharova",
                                   "naturalist19584" = "Marina Levasheva",
                                   "dakileno" = "Maria Ryazantseva",
                                   "ana_lu" = "Anastasia Lukinykh",
                                   #"kohab"
                                   "vaglazunov" = "Valerii Glazunov",
                                   #"adelmyagkova"
                                   "andreyefremov" = "Andrei Efremov",
                                   "alemts" = "Alexander Emtsev",
                                   "saranchine" = "Eugene Saranchin",
                                   "natalia_gamova" = "Natalia Gamova",
                                   "caseymclowe" = "Ksenia Atuchina",
                                   "mayacw" = "Maya Zhitkova",
                                   "ludmila2" = "Liudmila Usenko",
                                   #"pandabanda"
                                   "dariagassman" = "Daria Gassman",
                                   "khalilova_ekaterina" = "Ekaterina Khalilova"
                                   #"captain_lavrushka"
                                   #"kne"
                                   )
                                 )
      )
    ) %>% 
  simpleCache::simpleCache("inat", ., cacheDir = "cache")

# Data obtained via API

# Unnest the nested list of verbose data,
# and selectively pull out the necessary components of a list-columns

inat_api <- 
  inat_raw %>% 
  tibble::tibble(json = .) %>% 
  tidyr::unnest_wider(json) %>% 
  tidyr::hoist(taxon, scientific_name = "name") %>% 
  tidyr::hoist(identifications, current_id = "current") %>% 
  tidyr::hoist(identifications, identifier = c("user", "login")) %>% 
  dplyr::select(id,
                quality_grade,
                scientific_name,
                identifications_count,
                current_id,
                identifier) %>% 
  tidyr::unnest_longer(col = c(current_id, identifier)) %>% 
  dplyr::mutate(dplyr::across(
    identifier,
    ~ stringr::str_replace_all(.,
                               c("ninacourlee" = "Nina Filippova",
                                 "viktoriabilous" = "Viktoria Bilous",
                                 "tls-60" = "Tatiana Strus",
                                 "marasmius" = "Elena Zvyagina",
                                 "lioncbc" = "Vyacheslav Vlasenko",
                                 "elenabutunina" = "Elena Butunina",
                                 "aleks86" = "Alexandra Mingalimova",
                                 "epopov" = "Eugene Popov",
                                 "nikolai_nakonechnyi" = "Nikolai Nakonechnyi",
                                 "eadavydov" = "Eugene Davydov",
                                 "convallaria1128" = "Dmitrii Bochkov",
                                 "kim_potapov" = "Kim Potapov",
                                 "asfav" = "Alexandra Filippova",
                                 "igor_kuzmin" = "Igor Kuzmin",
                                 "ivankaramelkin" = "Dmitry Ageev",
                                 "tsvetasheva" = "Tatiana Svetasheva",
                                 "yury_rebriev" = "Yurii Rebriev",
                                 "svg52" = "Sergei Gerasimov",
                                 "urmansky" = "Alexander Korepanov",
                                 "sbolshakov" = "Sergei Bolshakov",
                                 "juhakinnunen" = "Juha Kinnunen",
                                 "natalyakorotkikh" = "Natalia Korotkikh",
                                 "milakalinina" = "Liudmila Kalinina",
                                 "kastani" = "Ruslan Nurhanov",
                                 "tmaximo" = "Massimo Tabone",
                                 "anastasiavlasenko" = "Anastasia Vlasenko",
                                 "johnplischke" = "John Plischke")
                               )
    )) %>% 
  simpleCache::simpleCache("inat_api", ., cacheDir = "cache")
