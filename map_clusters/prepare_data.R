##==================================================================================##
# GEO OF JOBS: PREPARE DATA FOR SHINY APP
#  Backend data and functions
#
# Cecile Murray
# April 2018
##==================================================================================##

# bringing in data
load("LEHD_WAC_JT00_2015.Rdata")
load("cbsa_xwalk.Rdata")
load("top100_coords.Rdata")

# recode data for the map
density %<>% mutate(
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

#============================================================#
# PREP A SHAPEFILE
#============================================================#

# make tract shapefile
tracts.shp <- readOGR("tracts.shp")

density %<>% arrange(tract, job_tot, ALAND_SQMI, density)

# function to produce the trimmed shapefile
create_cbsa_shp <- function(cbsa_tracts, tr.shp = tracts.shp){
  
  cbsa.shp <- geo_join(tr.shp, select(cbsa_tracts, tract, job_tot, density, ALAND_SQMI,
                                      contains("dense_cat"), contains("mapvar")),
                       by_sp = "GEOID", by_df = "tract", how = "inner")
  return(cbsa.shp)
}

#============================================================#
# COMPUTE DESCRIPTIVE STATS
#============================================================#

# summarize to 100 metros
summarize_by_metro <- function(df, var = "job_tot", f = "sum", cbsa = cbsa_xwalk) {
  
  # prepare cbsa list, select vars of interest from density df
  cbsa <- cbsa %>% select(cbsa, cbsa_name, top100) %>% distinct() %>%
    filter(top100==1)
  rv <- df %>% select(-tract, -cbsa_name, -contains("dense_cat"), -contains("mapvar")) %>%
    mutate(tr_ct = 1,
           cluster_ct_20 = ifelse(most_dense_20, 1, 0),
           cluster_ct_10 = ifelse(most_dense_10, 1, 0),
           cluster_ct_5 = ifelse(most_dense_5, 1, 0))

  # summarize given the number of functions provided
  if(length(f)==1){
    rv %<>% group_by(cbsa) %>% summarize_all(funs(!!f))
  } else {
    rv %<>% group_by(cbsa) %>% summarize_all(funs_(f))
  }

  # join with cbsa names, select relevant ones
  rv %<>% left_join(select(cbsa, cbsa, cbsa_name), by="cbsa") %>%
    select(cbsa, cbsa_name, starts_with(var), contains("most_dense"), density_min,
           tr_ct_sum, contains("cluster"), -ends_with("0_min"), -most_dense_5_min)

  return(rv)
}

met_summary <- summarize_by_metro(density, var = "job_tot", f = c("sum", "min")) %>%
  filter(cbsa %in% top100_xwalk$cbsa)
