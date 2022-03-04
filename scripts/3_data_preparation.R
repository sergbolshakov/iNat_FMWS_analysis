library(magrittr)
source("scripts/0_set_options.R")
simpleCache::simpleCache("inat_gtax")

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
  simpleCache::simpleCache("inat", .)
