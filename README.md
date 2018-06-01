DRAFT README

# geo-of-jobs
Analytical work on geography of jobs project

This repo contains the code for this Shiny interactive: https://murray-cecile.shinyapps.io/map_clusters/
The interactive runs out of the map_clusters subfolder - should stand alone, with all necessary data and scripts. However, you will need to make sure the relevant packages are installed, as the setup script does not check for that per Shiny guidelines.


CODE STRUCTURE:
setup.R -  set libraries, filepaths, import utility functions
prepare_LEHD.R - import clean LEHD data from shared network drive, join with tract area data
identify_clusters.R -  collapse to tract, compute density, flag high density and min jobs