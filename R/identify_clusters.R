##==================================================================================##
# GEOGRAPHY OF JOBS: IDENTIFY JOB CLUSTERS
#  Script to identify job clusters in the largest 100 metros
#
# Cecile Murray
# April 2018
##==================================================================================##

# bringing in libraries, filepaths, other global vars
library(here)
source(here("R", "setup.R"))

load(here("temp", "prepped_LEHD_2015.Rdata"))

#============================================================#
# PREPARE LEHD
#============================================================#

# function that collapses block-level LEHD into tracts
collapse_lehd <- function(lehd_df) {
  rv <- lehd_df %>% mutate(tract = substr(fips, 1, 11)) %>%
    select(-fips) %>% group_by(tract) %>% summarize_all(sum, na.rm=TRUE)
  return(rv)
}

lehd <- collapse_lehd(raw_lehd)

# take collapsed LEHD, add tract area, compute density w/rt given variable, add metro
compute_tract_density <- function(df, area_df, var, cbsa = cbsa_xwalk) {
  var <- enquo(var)
  rv <- df %>% select(tract, !!var) %>% 
    left_join(select(area_df, tract, ALAND_SQMI, stcofips), by="tract") %>%
    mutate(density  = UQ(var) / ALAND_SQMI) %>%
    left_join(select(cbsa, cbsa, cbsa_name, stcofips), by="stcofips") %>%
    select(-stcofips)
  
  return(rv)
}

density <- compute_tract_density(lehd, tract_area, var = job_tot, cbsa = cbsa_xwalk)

#============================================================#
# CREATE DENSITY FLAGS
#============================================================#

# flag tracts in the xth percentile of density by cbsa
flag_dense_tracts <- function(df, num_cats) {
  
  rv <- df %>% group_by(cbsa) %>%
    mutate(dense_cat = ntile(density, num_cats),
           most_dense = ifelse(dense_cat==num_cats, 1, 0)) %>%
    ungroup()
  
  return(rv)
}

density %<>% flag_dense_tracts(20)

# summarize to 100 metrs
summarize_by_metro <- function(df, var = "job_tot", f = "sum", cbsa = cbsa_xwalk) {
  
  cbsa <- cbsa %>% select(cbsa, cbsa_name, top100) %>% distinct() %>%
    filter(top100==1)
  rv <- df %>% select(-tract, -cbsa_name, -dense_cat) %>% mutate(tr_ct = 1)
  
  if(length(f)==1){
    rv %<>% group_by(cbsa) %>% summarize_all(funs(!!f)) 
  } else {
    rv %<>% group_by(cbsa) %>% summarize_all(funs_(f)) 
  }
  
  rv %<>% left_join(select(cbsa, cbsa, cbsa_name), by="cbsa") %>%
    select(cbsa, cbsa_name, starts_with(var), most_dense_sum, density_min, tr_ct_sum)

  return(rv)
}

met_summary <- summarize_by_metro(density, var = "job_tot", f = c("sum", "min")) %>%
  filter(cbsa %in% top100_xwalk$cbsa)


