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

top100_coords <- top100_xwalk %>% select(cbsa, cbsa_name) %>% distinct() %>%
  mutate(lat = 0, lon = 0)

for(i in seq_along(top100)[1:100]) {
  
  if(is.na(top100_coords$lat[i])){
    latlon <- geocode(top100[i], "latlon", "google")
    top100_coords$lat[top100_coords$cbsa_name==top100[i]] <- latlon$lat
    top100_coords$lon[top100_coords$cbsa_name==top100[i]] <- latlon$lon
    Sys.sleep(10)
  } else {
    next
  }
  
}