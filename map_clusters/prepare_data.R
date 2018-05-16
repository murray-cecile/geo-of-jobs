##==================================================================================##
# GEO OF JOBS: PREPARE DATA FOR SHINY APP
#  Loads in backend data and defines key functions
#
# Cecile Murray
# April 2018; reconfigured May 2018
##==================================================================================##

load("data_for_shiny.Rdata")

#============================================================#
# PREP A SHAPEFILE
#============================================================#

# function to filter down to tracts only in a given cbsa
select_cbsa_tracts <- function(df, cbsa_id, xwalk = top100_xwalk) {
  ctys <- dplyr::filter(xwalk, cbsa==cbsa_id)
  rv <- df %>% dplyr::filter(substr(tract, 1, 5) %in% ctys$stcofips)
  return(rv)
}

# function to produce the trimmed shapefile
create_cbsa_shp <- function(cbsa_tracts, tr.shp = tracts.shp){
  
  cbsa.shp <- geo_join(tr.shp, select(cbsa_tracts, tract, job_tot, density, ALAND_SQMI,
                                      contains("dense_cat"), contains("mapvar")),
                       by_sp = "GEOID", by_df = "tract", how = "inner")
  return(cbsa.shp)
}

#============================================================#
# IMPLEMENT MINIMUM JOB SHARE THRESHOLD
#============================================================#

# identify relevant job minimum and return that value
id_job_min <- function(min_pp, cbsa_id, yr, met_jobs = met_jobs){
 
  min <- case_when(
    min_pp == 0 ~ "zero_pp",
    min_pp == 0.25 ~ "oquart_pp",
    min_pp == 0.5 ~ "half_pp",
    min_pp == 0.75 ~ "tquart_pp",
    min_pp == 1 ~ "one_pp",
    TRUE ~ "zero_pp"
  )
  
  mins <- met_jobs %>% filter(cbsa == cbsa_id, year == yr, pp_min == min)
  return(mins$job_min)
}

# function to recode mapped variable to filter out low job count tracts
recode_job_min <- function(df, cbsa_id, yr, thresh, min_pp, met_jobs = met_jobs){
  
  job_min <- id_job_min(min_pp = min_pp, cbsa_id = cbsa_id,
                        yr = yr, met_jobs = met_jobs)
  
  # now recode the mapvar variable to reflect minimim job threshold
  rv <- df %>% mutate(mapvar = case_when(
    (mapvar == "cluster") & (job_tot > job_min) ~ "cluster",
    (mapvar == "cluster") & (job_tot <= job_min) ~ "high density",
    mapvar != "cluster" ~ mapvar
  ))
  
  return(rv)
}





