##==================================================================================##
# GEO OF JOBS: GET TOP 100 METRO LAT/LONGS
#  Get coordinates on which to center a leaflet map
#
# Cecile Murray
# April 2018
##==================================================================================##

library(here)
source(here("R", "setup.R"))

# produce more workable crosswalk
top100_xwalk <- cbsa_xwalk %>% filter(top100==1) %>% 
  select(cbsa, cbsa_name, stcofips)

top100 <- unique(top100_xwalk$cbsa_name)

# top100_coords <- top100_xwalk %>% select(cbsa, cbsa_name) %>% distinct() %>%
#   mutate(lat = 0, lon = 0)

for(i in seq_along(top100)[1:100]) {
  
  # skip if already filled in
  if(top100_coords$lat[i]==0 & top100_coords$lon[i]==0 | is.na(top100_coords$lat[i])){
    latlon <- geocode(top100[i], "latlon", "google")
    top100_coords$lat[top100_coords$cbsa_name==top100[i]] <- latlon$lat
    top100_coords$lon[top100_coords$cbsa_name==top100[i]] <- latlon$lon
    Sys.sleep(5)
  } else {
    Sys.sleep(5)
    next
  }
  
}


# fix Pittsburgh, Winston-Salem, and Sam Antonio manually
top100_coords$lat[top100_coords$cbsa == "38300"] <- 40.4313473
top100_coords$lon[top100_coords$cbsa == "38300"] <- -80.05054
top100_coords$lat[top100_coords$cbsa == "41700"] <- 29.4813572
top100_coords$lon[top100_coords$cbsa == "41700"] <- -98.6544876
top100_coords$lat[top100_coords$cbsa == "49180"] <- 36.1045516
top100_coords$lon[top100_coords$cbsa == "49180"] <- -80.3134975

# save(top100_coords, file = "map_clusters/top100_coords.Rdata")
