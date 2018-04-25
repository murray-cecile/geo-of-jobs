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
  ctys <- dplyr::filter(xwalk, cbsa==cbsa_id)
  rv <- df %>% dplyr::filter(substr(tract, 1, 5) %in% ctys$stcofips)
  return(rv)
}

cbsa <- select_cbsa_tracts(density, "41860") 


#============================================================#
# PREP A SHAPEFILE
#============================================================#

# make tract shapefile
# tracts.shp <- readOGR("map_clusters/shp/tracts.shp")

density %<>% arrange(tract, job_tot, ALAND_SQMI, density)

# function to produce the trimmed shapefile
create_cbsa_shp <- function(cbsa_tracts, tr.shp = tracts.shp){
  
  cbsa.shp <- geo_join(tr.shp, select(cbsa_tracts, tract, job_tot, density, ALAND_SQMI,
                                      contains("dense_cat"), contains("most_dense")),
                       by_sp = "GEOID", by_df = "tract", how = "inner")
  return(cbsa.shp)
}

cbsa.shp <- create_cbsa_shp(cbsa)

check <- cbsa.shp@data

# create map var so we can make 

#============================================================#
# LEAFLET
#============================================================#

# create labels
labels <- sprintf("<strong>Tract %s</strong><br/>%g jobs in %g mi<sup>2</sup> = density of %g",
                  cbsa.shp@data$tract, cbsa.shp@data$job_tot, cbsa.shp@data$ALAND_SQMI,
                  cbsa.shp@data$density) %>%
  lapply(htmltools::HTML)

# # create color scheme for job hubs
# pal0 <- colorFactor(c("#000000", "#FFCF1A"), as.factor(cbsa$most_dense_10))
# 
# # create color scheme for job density
# pal1 <- colorBin(colorRamp(c("#E0ECFB", "#00649F"), interpolate = "spline"), 
#                  cbsa$dense_cat)
# 
# # returns a palette function determined by whether the tract is a cluster 
# master_pal <- function() {
#   
# }
# 
# 
# # simple map of clusters
# cbsa.hub <- leaflet(cbsa.shp) %>%
#   setView(lng = -122.08080, lat = 37.662, zoom = 10) %>%
#   addTiles() %>%
#   addPolygons(color = ~pal0(most_dense_10), weight = 1, smoothFactor = 0.5,
#               opacity = 1, fillOpacity = 0.75,
#               fillColor = ~pal1(dense_cat_10),
#               highlight = highlightOptions(
#                 weight = 5,
#                 color = "#666",
#                 dashArray = "",
#                 fillOpacity = 0.7,
#                 bringToFront = TRUE),
#               label = labels,
#               labelOptions = labelOptions(list("font-weight" = "normal",
#                                                padding = "3px 8px"),
#                                           textsize = "15px", direction = "auto"))
# %>%
#   addPolygons(color = "#FFFFFF", weight = 1, smoothFactor = 0.5,
#               opacity = 1, fillOpacity = 0.3,
#               fillColor = ~pal0(most_dense),
#               highlight = highlightOptions(
#                 weight = 5,
#                 color = "#666",
#                 dashArray = "",
#                 fillOpacity = 0.7,
#                 bringToFront = TRUE),
#               label = labels,
#               labelOptions = labelOptions(list("font-weight" = "normal",
#                                                padding = "3px 8px"),
#                                           textsize = "15px", direction = "auto")) %>%
#   addLegend(pal = pal1, values = ~most_dense_10, opacity = 0.75, title = NULL,
#             position = "bottomright")
# cbsa.hub


# 
# # simple map of density deciles
# cbsa.dense <- leaflet(cbsa.shp) %>%
#   setView(lng = -76.6122, lat = 39.2904, zoom = 10) %>%
#   addTiles() %>%
#   addPolygons(color = "#FFFFFF", weight = 1, smoothFactor = 0.5,
#               opacity = 1, fillOpacity = 0.75,
#               fillColor = ~pal1(dense_cat),
#               highlight = highlightOptions(
#                 weight = 5,
#                 color = "#666",
#                 dashArray = "",
#                 fillOpacity = 0.7,
#                 bringToFront = TRUE),
#               label = labels,
#               labelOptions = labelOptions(list("font-weight" = "normal",
#                                                padding = "3px 8px"),
#                                           textsize = "15px", direction = "auto")) %>%
#   addLegend(pal = pal1, values = ~dense_cat, opacity = 0.75, title = NULL,
#             position = "bottomright")
# 
# cbsa.dense

# labels2 <-sprintf("<strong>Tract %s</strong><br/>%g total jobs",
#                   cbsa.shp@data$tract, cbsa.shp@data$job_tot) %>%
#   lapply(htmltools::HTML)
# 
# # create color scheme
# pal2 <- colorBin(colorRamp(c("#E0ECFB", "#00649F"), interpolate = "spline"), 
#                  cbsa$job_tot, bins = 10)
# 
# # simple map of density deciles
# cbsa.tot <- leaflet(cbsa.shp) %>%
#   setView(lng = -76.6122, lat = 39.2904, zoom = 10) %>%
#   addTiles() %>%
#   addPolygons(color = "#FFFFFF", weight = 1, smoothFactor = 0.5,
#               opacity = 1, fillOpacity = 0.75,
#               fillColor = ~pal2(job_tot),
#               highlight = highlightOptions(
#                 weight = 5,
#                 color = "#666",
#                 dashArray = "",
#                 fillOpacity = 0.7,
#                 bringToFront = TRUE),
#               label = labels2,
#               labelOptions = labelOptions(list("font-weight" = "normal",
#                                                padding = "3px 8px"),
#                                           textsize = "15px", direction = "auto")) %>%
#   addLegend(pal = pal2, values = ~job_tot, opacity = 0.75, title = NULL,
#             position = "bottomright")
# 
# cbsa.tot



# #============================================================#
# # JUST MAP JOB COUNTS
# #============================================================#
# 
# # create labels
# labels1 <- sprintf("<strong>Tract %s</strong><br/>%g jobs / mi<sup>2</sup>",
#                   cbsa$tract, cbsa$density) %>%
#   lapply(htmltools::HTML)
# 
# # create color scheme
# pal1 <- colorQuantile(colorRamp(c("#E0ECFB", "#00649F"), interpolate = "spline"), 
#                  cbsa$job_tot)
# 
# # simple map of clusters
# cbsa.hub <- leaflet(cbsa.shp) %>%
#   setView(lng = -76.6122, lat = 39.2904, zoom = 10) %>%
#   addTiles() %>%
#   addPolygons(color = "#FFFFFF", weight = 1, smoothFactor = 0.5,
#               opacity = 1, fillOpacity = 0.75,
#               fillColor = ~pal1(job_tot),
#               highlight = highlightOptions(
#                 weight = 5,
#                 color = "#666",
#                 dashArray = "",
#                 fillOpacity = 0.7,
#                 bringToFront = TRUE),
#               label = labels,
#               labelOptions = labelOptions(list("font-weight" = "normal",
#                                                padding = "3px 8px"),
#                                           textsize = "15px", direction = "auto")) %>%
#   addLegend(pal = pal0, values = ~job_tot, opacity = 0.75, title = NULL,
#             position = "bottomright")
# 
# cbsa.hub

# # create color scheme
# pal1 <- function(dense_cat) {
#   colorBin(colorRamp(c("#E0ECFB", "#00649F"), interpolate = "spline"), 
#                 chi$dense_cat, bin = 10) 
#   }
# 
# # attempt to layer on clusters
# chi.map <- leaflet(chi.shp) %>%
#   setView(lng = -87.6298, lat = 41.8782, zoom = 10) %>%
#   addTiles() %>%
#   addPolygons(color = "#FFFFFF", weight = 1, smoothFactor = 0.5,
#               opacity = 1, fillOpacity = 0.75, 
#               fillColor = ~pal1(dense_cat), 
#               highlight = highlightOptions(
#                 weight = 5, 
#                 color = "#666",
#                 dashArray = "",
#                 fillOpacity = 0.7, 
#                 bringToFront = TRUE),
#               label = labels, 
#               labelOptions = labelOptions(list("font-weight" = "normal",
#                                           padding = "3px 8px"),
#               textsize = "15px", direction = "auto")) %>%
#   addLegend(pal = pal, values = ~dense_cat, opacity = 0.75, title = NULL,
#             position = "bottomright")

# chi.map



 
# #============================================================#
# # PLOTLY
# #============================================================#
# 
# tracts.df <- tracts.shp %>% tidy(region = "GEOID")
# 
# # trim the shapefile
# oh.df <- filter(tracts.shp, substr(id, 1, 2)=="39") %>%
#   dplyr::rename(tract = id)
# il.df <- filter(tracts.shp, substr(id, 1, 2)=="17") %>%
#   dplyr::rename(tract = id)
# 
# # select data for OH
# ohdata <- density %>% select(tract, job_tot, density, dense_cat, most_dense) %>%
#   filter(substr(tract, 1, 2)=="39") %>%
#   left_join(oh.df, by="tract")
# 
# # ggplot
# m <- ggplot(ohdata) +
#   geom_polygon