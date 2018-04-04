##==================================================================================##
# GEO OF JOBS: LEAFLET MAPS
#  Create leaflet maps of job clusters
#
# Cecile Murray
# April 2018
##==================================================================================##

# bringing in libraries, filepaths, other global vars
library(here)
source(here("R", "identify_clusters.R"))

#============================================================#
# CREATE CBSA-COUNTY XWALK 
#============================================================#

# produce more workable crosswalk
top100_xwalk <- cbsa_xwalk %>% filter(top100==1) %>% 
  select(cbsa, cbsa_name, stcofips) 

# function to filter down to tracts only in a given cbsa
select_cbsa_tracts <- function(df, cbsa_id, xwalk = top100_xwalk) {
  ctys <- filter(xwalk, cbsa==cbsa_id)
  rv <- df %>% filter(substr(tract, 1, 5) %in% ctys$stcofips)
  return(rv)
}

# chi <- select_cbsa_tracts(density, "16980")

#============================================================#
# LEAFLET
#============================================================#

# make tract shapefile
tracts.shp <- readOGR("tracts.shp")

# function to produce the trimmed shapefile
create_cbsa_shp <- function(cbsa_tracts, tr.shp = tracts.shp){
  
  cbsa.shp <- geo_join(tr.shp, select(cbsa_tracts, tract, job_tot, density, dense_cat, most_dense),
                       by_sp = "GEOID", by_df = "tract", how = "inner")
  return(cbsa.shp)
}

chi.shp <- create_cbsa_shp(chi)

chi <- leaflet(chi.shp) %>%
  setView(lng = -87.6298, lat = 41.8782, zoom = 10) %>%
  addTiles() %>%
  addPolygons(color = "#FFFFFF", weight = 1, smoothFactor = 0.5,
              opacity = 1, fillOpacity = 0.75, 
              fillColor = ~colorBin(c("#E0ECFB", "#00649F"),
                                    most_dense, bins = 2)(most_dense))
# chi



#============================================================#
# PLOTLY
#============================================================#

tracts.df <- tracts.shp %>% tidy(region = "GEOID")

# trim the shapefile
oh.df <- filter(tracts.shp, substr(id, 1, 2)=="39") %>%
  dplyr::rename(tract = id)
il.df <- filter(tracts.shp, substr(id, 1, 2)=="17") %>%
  dplyr::rename(tract = id)

# select data for OH
ohdata <- density %>% select(tract, job_tot, density, dense_cat, most_dense) %>%
  filter(substr(tract, 1, 2)=="39") %>%
  left_join(oh.df, by="tract")

# ggplot
m <- ggplot(ohdata) +
  geom_polygon