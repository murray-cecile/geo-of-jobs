# geo-of-jobs
## Analytical work on geography of jobs project

The map_clusters subdirectory of this repo contains the code for this Shiny interactive: https://murray-cecile.shinyapps.io/map_clusters/
It should stand alone, with all necessary data and scripts. However, you will need to make sure the relevant packages are installed, as the setup script does not check for that per Shiny guidelines.

### Getting started:

The first step is to clone or download and unzip the repository, and to change directions into the project directory.

You will need a somewhat recent version of R/RStudio installed. You can run the following code to ensure you have the necessary packages installed:

```

libs <- c("tidyverse","janitor", "sp", "maptools", "tigris", "foreign", 
          "readstata13", "openxlsx", "censusapi", "leaflet", "sf", "tidycensus", 
          "here", "shiny", "viridis", "ggridges", "ggthemes", "matrixStats")
lapply(libs, install.packages)

```

### Running the interactive:

All of the necessary data files are included in the map_clusters subdirectory, but the first time you run the Shiny app locally, you'll need to run `source(create_data_env.R)`. This script loads the underlying data, geographic crosswalks, and shapefiles, pre-processes much of the data used in the Shiny app, and calls another script `calc_descriptive_stats.R`, to compute descriptive stats. The resulting R environment is saved locally to speed up future use and for use in the version of Shiny app hosted online. Subsequently, you won't need to run either of these scripts, since the saved image is loaded as part of the app.

Once you've created the data environment, you can work from `app.R`. If you're running the Shiny app locally, you'll need to make sure your working directory is set to the map_clusters subdirectory, and not to the geo_of_jobs master directory. There are two lines of code that will do this at the top of `app.R`. However, when publishing to shinyapps.io, make sure to comment out those lines.


### Running other analytics:

This repo also contains code to run other analytics, including some national descriptive statistics. The subdirectories are organized as follows:

Subdirectory | Contents
------------ | --------
raw | raw tract and block geography files 
temp | intermediate environments after initial processing
R | all R scripts
output | all results tables 
docs | documentation of data sources, methods, data checks


#### Key scripts: 

* setup.R - load R packages, set filepaths, import utility functions from other directories
* prepare_LEHD.R - import clean LEHD data from shared network drive, join with tract area data
* identify_clusters.R -  collapse to tract, compute density, flag clusters based on density and number of jobs
* calc_natl_descriptive_stats.R - compute national shares plus high-level metro summaries
* calc_industry_descriptive_stats.R - compute descriptive stats about industries

#### Other scripts:

* crosscheck_jobs.R - compared LEHD job totals vs. Moody's Analytics data (no longer available)
* geocode_metros.R - used Google API to get latitude and longitude for each metro in order to center the map
* get_pop_density.R - compute population density by tract
* initial_exploration.R - initial plots and summaries of job density by metro
* make_leaflet_maps.R - experiment with creating Leaflet maps, some on Rpubs
* map_baltimore_blocks.R - create block group level density map
* map_setup.R - imports some shapefiles to enable mapping
