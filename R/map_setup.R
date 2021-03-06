##==================================================================================##
# GEO OF JOBS: MAPPING SETUP
#  import shapefiles to enable mapping
#
# Cecile Murray
# February 2018
##==================================================================================##

library(here)
source(here("R", "setup.R"))

#============================================================#
# SHAPEFILES
#============================================================#

# download once
# options(tigris_use_cache=TRUE)
# tracts.shp <- lapply(unique(fips_codes$state_code)[1:51],
#                      function(x) {tracts(x, cb=TRUE)}) %>%
#   rbind_tigris()
# counties.shp <- counties()
# states.shp <- states()

# convert to sf
tracts.sf <- st_as_sf(tracts.shp)
counties.sf <- st_as_sf(counties.shp)
states.sf <- st_as_sf(states.shp) 

# cleaning: restrict to lower 48 + DC, rename geoids
clean_tigris_sf <- function(sf, geoid) {
  rv <- sf %>% filter(as.numeric(STATEFP)<57,
                      !STATEFP %in% c("02", "15")) %>%
    dplyr::rename_at(vars(GEOID), funs(gsub(., geoid, .)))
  return(rv)
}

counties.sf %<>% clean_tigris_sf("stcofips")
states.sf %<>% clean_tigris_sf("stfips")

# save
# st_write(tracts.sf, "temp/tracts.shp")
# st_write(counties.sf, "temp/counties.shp")
# st_write(states.sf, "temp/states.shp")

# read in
# tracts.sf <- st_read()

# convert to point for standard ggplot viz
tracts.df <- tidy(tracts.shp, region = "GEOID") %>%
  dplyr::rename(tract = id)
counties.df <- tidy(counties.shp, region = "GEOID") %>%
  dplyr::rename(stcofips = id) %>%
  filter(!substr(stcofips, 1, 2) %in% c("02", "15"),
         as.numeric(substr(stcofips, 1, 2)) < 57)
states.df <- tidy(states.shp, region = "GEOID") %>%
  dplyr::rename(stfips = id) %>%
  filter(!stfips %in% c("02", "15"),
         as.numeric(stfips) < 57)