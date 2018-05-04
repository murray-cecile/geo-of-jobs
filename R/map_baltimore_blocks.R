##==================================================================================##
# GEO OF JOBS: MAKE BALTIMORE BLOCK LEVEL MAP
#  Demo block-level data for Jennifer
#
# Cecile Murray
# April 2018
##==================================================================================##

library(here)
source(here("R", "setup.R"))
# source(here("R", "identify_clusters.R"))

load(here("map_clusters", "top100_coords.Rdata"))
baltimore <- filter(top100_coords, cbsa=="12580")

# load("temp/baltimore_block_maps.Rdata")

#============================================================#
# PREPARE BLOCK AREA DATA
#============================================================#

md <- read.xlsx("raw/MD_block_area.xlsx")

md$tract <- padz(as.character(md$tract * 100))
md$block <- padz(as.character(md$block))
md %<>% mutate(fips = paste0(county, tract, block),
               county = as.character(county)) %>%
  dplyr::rename(stcofips = county, trfips = tract, 
                blfips = block, ALAND_SQMI = LandSQMI) %>%
  select(fips, stcofips, ALAND_SQMI)

md_blkgrp <- md %>% mutate(blkgrp = substr(fips, 1, 12)) %>%
  select(-fips, -stcofips) %>% group_by(blkgrp) %>% summarize_all(sum, na.rm=TRUE) %>%
  dplyr::rename(fips = blkgrp) %>%
  mutate(stcofips = substr(fips, 1, 5))

md.shp <- block_groups("24")
# mdshp <- md.shp
# writeOGR(obj = mdshp, layer = "mdshp", dsn = "shapefiles", driver = "ESRI Shapefile")
# md.shp <- readOGR("shapefiles/mdshp.shp")

#============================================================#
# COMPUTE JOB DENSITY IN BALTIMORE
#============================================================#

bm <- filter(raw_lehd, substr(fips, 1, 5) %in% 
               cbsa_xwalk$stcofips[cbsa_xwalk$cbsa=="12580"]) %>%
  mutate(blkgrp = substr(fips, 1, 12)) %>% select(-fips) %>%
  group_by(blkgrp) %>% summarize_all(sum, na.rm=TRUE) %>%
  mutate(stcofips = substr(blkgrp, 1, 5))

# compute density w/rt given variable, add metro
compute_block_density <- function(df, area_df, var, cbsa = cbsa_xwalk) {
  var <- enquo(var)
  rv <- df %>% select(fips, !!var) %>%
    left_join(select(area_df, fips, ALAND_SQMI, stcofips), by="fips") %>%
    mutate(density  = UQ(var) / ALAND_SQMI) %>%
    left_join(select(cbsa, cbsa, cbsa_name, stcofips), by="stcofips") #%>%
    # select(-stcofips)

  return(rv)
}

bm %<>% dplyr::rename(fips = blkgrp) %>%
  compute_block_density(md_blkgrp, var = job_tot, cbsa = cbsa_xwalk) %>%
  dplyr::rename(blkgrp = fips)

#============================================================#
# CREATE DENSITY FLAGS
#============================================================#

# flag tracts in the xth percentile of density by cbsa
flag_dense_blocks <- function(df, num_cats) {
  
  cat_name <- paste0("dense_cat_", num_cats)
  dense_name <- paste0("most_dense_", num_cats)
  map_name <- paste0("map_var_", num_cats)
  
  rv <- df %>% group_by(cbsa) %>%
    mutate(UQ(cat_name) := ntile(density, num_cats)) %>%
    ungroup()
  
  # create boolean for top ntile
  rv[, c(dense_name)] <- as.vector(rv[, c(cat_name)]==num_cats)
  
  return(rv)
}

bm %<>% flag_dense_blocks(20) %>% 
  flag_dense_blocks(10) %>% 
  flag_dense_blocks(5) 


# recode data for the map
bm %<>% mutate(
  mapvar_20 = case_when(
    dense_cat_20 < 17 | is.na(dense_cat_20) ~ "low density",
    dense_cat_20 == 17 ~ "p80",
    dense_cat_20 == 18 ~ "p85",
    dense_cat_20 == 19 ~ "p90",
    dense_cat_20 == 20 ~ "cluster"
  ), 
  mapvar_10 = case_when(
    dense_cat_20 < 17 | is.na(dense_cat_20) ~ "low density",
    dense_cat_20 == 17 ~ "p80",
    dense_cat_20 == 18 ~ "p85",
    dense_cat_10 == 10 ~ "cluster"
  ), 
  mapvar_5 = ifelse(dense_cat_20 >= 17, "cluster", "low_density")
)

#============================================================#
# PREP SHAPEFILE
#============================================================#

bm %<>% arrange(blkgrp, job_tot, ALAND_SQMI, density) 

# function to produce the trimmed shapefile
create_cbsa_shp <- function(cbsa_df, bl.shp = md.shp){
  
  cbsa.shp <- geo_join(bl.shp, select(cbsa_df, blkgrp, job_tot, density, ALAND_SQMI,
                                      contains("dense_cat"), contains("most_dense"),
                                      contains("mapvar")),
                       by_sp = "GEOID", by_df = "blkgrp", how = "inner")

  return(cbsa.shp)
}

bm.shp <- create_cbsa_shp(bm) 

#============================================================#
# CREATE LEAFLET MAP
#============================================================#

# create labels
labels <- sprintf("<strong>Block Group%s</strong><br/>%g jobs in %g mi<sup>2</sup> = density of %g",
                  bm.shp@data$blkgrp, bm.shp@data$job_tot, bm.shp@data$ALAND_SQMI,
                  bm.shp@data$density) %>%
  lapply(htmltools::HTML)

# create color scheme
pal1 <- colorFactor(c("#FFCF1A", "#E0ECFB", "#A4C7F2", "#3E83C1", "#00649F"),
                    as.factor(bm$mapvar_20))

# make the leaflet map
bm.map  <- leaflet(bm.shp) %>%
  setView(lng = baltimore$lon, lat = baltimore$lat, zoom = 9) %>%
  addTiles() %>%
  addPolygons(color = "#FFFFFF", weight = 0.5, fillColor = ~pal1(mapvar_20),
              label = labels, fillOpacity = 0.60,
              labelOptions = labelOptions(list("font-weight" = "normal",
                                               padding = "3px 8px"),
                                          textsize = "15px", direction = "auto")) %>%
  addLegend(pal = pal1, values = ~mapvar_20, opacity = 0.75, title = NULL,
            position = "bottomright")
bm.map



# save.image(file = "temp/baltimore_block_maps.Rdata")

