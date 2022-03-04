# Analysis of observations of fungi and myxomycetes in iNaturalist

This repository contains scripts and data for the article "Crowdsourcing fungal biodiversity: revision of iNaturalist observations in Northern West Siberia" submitted to the journal "Nature Conservation Research".

## About the project

We analyze data from the iNaturalist project "[Fungi and myxomycetes in Northern West Siberia](https://www.inaturalist.org/projects/fungi-and-myxomycetes-in-northern-west-siberia)", which filters observations of fungi and myxomycetes from three regions of Western Siberia — Yamalo-Nenets AO, Khanty-Mansi AO and Tyumen Region.

## How to use scripts

The project [`renv`](https://rstudio.github.io/renv/index.html) is used for script reproducibility, so to restore the project library you need to execute `renv::restore()`. This will load the required R packages.

Some functions [are memoised](cashe/) using [`memoise`](https://memoise.r-lib.org/) to avoid repeating API queries. Load it into your global environment with `source("scripts/set_options.R")`

Also, load `library(magrittr)` for pipes.

The order in which the scripts are executed:

0. [Memoization](scripts/0_set_options.R) functions to cache the results of requests to the GBIF API.
1. [Import](scripts/1_import.R) of required raw data.
2. [Search](scripts/2_sci_names_lookup.R) scientific names of taxa used in the datasets.
3. [Preparing](scripts/3_data_preparation.R) the iNaturalist data for further analysis.
4. [Taxonomic analysis](scripts/4_taxonomy_analysis.R) of the iNaturalist data and the finds of species new to the study regions.
5. [Geospatial distribution analysis](scripts/5_geographic_coverage.R) of observations by administrative regions and protected areas.
6. [Selection of observations of the protected species](scripts/6_protected_species.R) included in the IUCN Red List, the Red Book of the Russian Federation, and Red Books of the study regions.
