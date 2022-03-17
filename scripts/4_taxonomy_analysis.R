library(magrittr)

# Load data from cache --------------------------------------------------------- 

simpleCache::loadCaches(c("inat", "inat_csv", "lit_all_gan"),
                        cacheDir = "cache")

# Taxonomic coverage -----------------------------------------------------------

# GBIF Backbone taxonomy
# (names that exist in GBIF Backbone Taxonomy and have accepted species)

# Output the summary table

inat %>% 
  # dplyr::filter(qualityGrade == "research") %>% 
  dplyr::group_by(kingdom, phylum, class) %>% 
  dplyr::summarise(orders = dplyr::n_distinct(order, na.rm = TRUE),
                   families = dplyr::n_distinct(family, na.rm = TRUE),
                   genera = dplyr::n_distinct(genus, na.rm = TRUE),
                   species = dplyr::n_distinct(species, na.rm = TRUE),
                   observations = dplyr::n_distinct(occurrenceID)
                   ) %>% 
  flextable::flextable() %>%
  flextable::set_table_properties(layout = "autofit") %T>% 
  flextable::save_as_docx(path = "output/taxonomic_coverage.docx")

# Output the treemap

# inat %>%
#   dplyr::filter(qualityGrade == "research") |>
#   dplyr::group_by(kingdom, phylum, class) %>%
#   dplyr::summarise(orders = dplyr::n_distinct(order, na.rm = TRUE),
#                    families = dplyr::n_distinct(family, na.rm = TRUE),
#                    genera = dplyr::n_distinct(genus, na.rm = TRUE),
#                    species = dplyr::n_distinct(species, na.rm = TRUE),
#                    observations = dplyr::n_distinct(occurrenceID)
#                    ) %>%
#   treemap::treemap(index = c("phylum", "class"),
#                    vSize = "species",
#                    type = "index",
#                    algorithm = "squarified",
#                    align.labels = list(c("center", "bottom"),
#                                        c("center", "top")),
#                    palette = "Set3")

# Heat tree

inat_taxonomy <- 
  inat %>% 
  # dplyr::filter(qualityGrade == "research") %>% 
  dplyr::group_by(acceptedNameUsage) %>% 
  dplyr::mutate(n_obs = dplyr::n(),
                n_spe = dplyr::n_distinct(species, na.rm = TRUE)) %>%
  metacoder::parse_tax_data(class_cols = 20:26,
                            named_by_rank = TRUE)

set.seed(1)

plot_mycetozoa <- 
  inat_taxonomy %>% 
  metacoder::filter_taxa(taxon_names == "Mycetozoa",
                         subtaxa = TRUE) %>% 
  metacoder::filter_taxa(taxon_ranks == "family", supertaxa = TRUE) %>%
  metacoder::heat_tree(initial_layout = "reingold-tilford",
                       layout = "davidson-harel",
                       node_label = taxon_names,
                       node_size = n_obs,
                       node_size_range = c(0.01, 0.05),
                       node_color = n_obs,
                       node_color_axis_label = "Number of \n observations")

plot_asco <- 
  inat_taxonomy %>% 
  metacoder::filter_taxa(taxon_names == "Ascomycota", subtaxa = TRUE) %>% 
  metacoder::filter_taxa(taxon_ranks == "family", supertaxa = TRUE) %>%
  metacoder::heat_tree(initial_layout = "reingold-tilford",
                       layout = "davidson-harel",
                       node_label = taxon_names,
                       node_size = n_obs,
                       node_size_range = c(0.01, 0.05),
                       node_color = n_obs,
                       node_color_axis_label = "Number of \n observations")

plot_basidio <-
  inat_taxonomy %>% 
  metacoder::filter_taxa(taxon_names == "Basidiomycota", subtaxa = TRUE) %>% 
  metacoder::filter_taxa(taxon_ranks == "family", supertaxa = TRUE) %>%
  metacoder::heat_tree(initial_layout = "reingold-tilford",
                       layout = "davidson-harel",
                       node_label = taxon_names,
                       node_size = n_obs,
                       node_size_range = c(0.01, 0.05),
                       node_color = n_obs,
                       node_color_axis_label = "Number of \n observations")

cowplot::plot_grid(plot_asco,
                   plot_basidio,
                   ncol = 2, nrow = 1)

ggplot2::ggsave("output/taxonomic_coverage.jpg",
                dpi = 1200,
                bg = "white")

# iNaturalist taxonomy

inat_csv %>% 
  # dplyr::filter(quality_grade == "research") %>% 
  dplyr::group_by(taxon_kingdom_name, taxon_phylum_name, taxon_class_name) %>% 
  dplyr::summarise(orders = dplyr::n_distinct(taxon_order_name, na.rm = TRUE),
                   families = dplyr::n_distinct(taxon_family_name, na.rm = TRUE),
                   genera = dplyr::n_distinct(taxon_genus_name, na.rm = TRUE),
                   species = dplyr::n_distinct(taxon_species_name, na.rm = TRUE),
                   observations = dplyr::n_distinct(id)
                   ) %>% 
  flextable::flextable() %>%
  flextable::autofit()

# Species new for the studied regions ------------------------------------------

# Load accepted names with the same basionyms
# see https://github.com/gbif/portal-feedback/issues/3963
# filter all such names that are in both datasets

simpleCache::loadCaches("gbif_backbone_fungi_duplicates", cacheDir = "data")

duplicate_acc_names <- 
  dplyr::anti_join(
    inat %>% 
      dplyr::filter(acceptedNameUsage %in% gbif_backbone_fungi_duplicates$scientificName) %>% 
      dplyr::select(acceptedNameUsage) %>% 
      dplyr::distinct() %>% 
      dplyr::inner_join(gbif_backbone_fungi_duplicates,
                        by = c("acceptedNameUsage" = "scientificName")),
    lit_all_gan %>% 
      dplyr::filter(acceptedNameUsage %in% gbif_backbone_fungi_duplicates$scientificName) %>% 
      dplyr::select(acceptedNameUsage) %>% 
      dplyr::distinct() %>% 
      dplyr::inner_join(gbif_backbone_fungi_duplicates,
                        by = c("acceptedNameUsage" = "scientificName"))
    ) %>% 
  dplyr::pull(acceptedNameUsage)

# Compare the accepted species from the iNaturalist and literature dataб
# taking into account these accepted names with the same basionyms

inat_new <- 
  setdiff(inat$species[!(inat$acceptedNameUsage %in% duplicate_acc_names)],
          lit_all_gan$species) %>% 
  sort()

# Get new species finds

inat_new_species <-
  inat %>% 
  dplyr::filter(qualityGrade == "research",
                species %in% inat_new) %>% 
  dplyr::group_by(acceptedNameUsage, stateProvince) %>% 
  dplyr::select(occurrenceID,
                acceptedNameUsage,
                stateProvince,
                decimalLatitude,
                decimalLongitude,
                eventDate,
                recordedBy,
                associatedReferences,
                ) %>%
  dplyr::distinct() %>% 
  dplyr::arrange(acceptedNameUsage) %>% 
  dplyr::ungroup() %>% 
  simpleCache::simpleCache("inat_new_species", ., cacheDir = "cache")

# Print statistics

inat_new_species %>% 
  dplyr::group_by(stateProvince) %>% 
  dplyr::summarize(species = dplyr::n_distinct(acceptedNameUsage),
                   observations = dplyr::n_distinct(occurrenceID)) %>% 
  flextable::flextable() %>% 
  flextable::autofit()

# Output the document with new species finds

inat_new_species_strings <- 
  inat_new_species %>% 
  dplyr::mutate(
    coordinates = stringr::str_c(format(decimalLatitude, nsmall = 5), "° N, ",
                                 format(decimalLongitude, nsmall = 5), "° E"),
    decimalLatitude = NULL,
    decimalLongitude = NULL,
    eventDate = format(eventDate, "%d.%m.%Y"),
    .after = stateProvince
  ) %>% 
  dplyr::group_by(acceptedNameUsage, stateProvince) %>% 
  dplyr::mutate(
    obs_number = dplyr::n(),
    obs_info = dplyr::case_when(
      obs_number > 3 ~ stringr::str_c(obs_number, " observations"),
      TRUE ~ stringr::str_c(coordinates, eventDate, recordedBy, associatedReferences,
                            sep = ", ")
    )
  ) %>% 
  dplyr::select(-(coordinates:obs_number)) %>% 
  dplyr::distinct() %>%
  dplyr::ungroup() %>% 
  dplyr::transmute(
    new_species = stringr::str_c(acceptedNameUsage, " — new for ",
                                 stateProvince, ", ",
                                 obs_info, ".")
    ) %>% 
  dplyr::pull()

inat_new_species_doc <- officer::read_docx(path = "data/output_style.docx")

for(i in seq_along(inat_new_species_strings)) {
  inat_new_species_doc <- officer::body_add_par(inat_new_species_doc,
                                                inat_new_species_strings[[i]])
}

print(inat_new_species_doc, target = "output/new_species.docx")
