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

### Running the interactive

All of the necessary data files are included in the map_clusters subdirectory, but the first time you run the Shiny app locally, you'll need to run `source(create_data_env.R)`. This script loads the underlying data, geographic crosswalks, and shapefiles, pre-processes much of the data used in the Shiny app, and calls another script `calc_descriptive_stats.R`, to compute descriptive stats. The resulting R environment is saved locally to speed up future use and for use in the version of Shiny app hosted online. Subsequently, you won't need to run either of these scripts, since the saved image is loaded as part of the app.

Once you've created the data environment, you can work from `app.R`. If you're running the Shiny app locally, you'll need to make sure your working directory is set to the map_clusters subdirectory, and not to the geo_of_jobs master directory. There are two lines of code that will do this at the top of `app.R`. However, when publishing to shinyapps.io, make sure to comment out those lines.


### Code Structure:

setup.R -  set libraries, filepaths, import utility functions
prepare_LEHD.R - import clean LEHD data from shared network drive, join with tract area data
identify_clusters.R -  collapse to tract, compute density, flag high density and min jobs
calc_natl_descriptive_stats.R - compute national shares