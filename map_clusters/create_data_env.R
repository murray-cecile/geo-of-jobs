##==================================================================================##
# GEO OF JOBS: CREATE INITIAL DATA ENVIRONMENT FOR SHINY APP
#  Build density dataset, crosswalks, etc, for future use
#  This script only needs to be run ONCE EVER
#
# Cecile Murray
# May 2018
##==================================================================================##

libs <- c("tidyverse", "magrittr", "stringr", "readr", "openxlsx", "janitor", "sp",
          "tigris", "foreign", "readstata13", "shiny", "rgdal")
lapply(libs, library, character.only=TRUE)

# bringing in data
load("LEHD_WAC_JT00_2010.Rdata")
load("LEHD_WAC_JT00_2015.Rdata")
load("cbsa_xwalk.Rdata")
load("top100_coords.Rdata")

#============================================================#
# SHAPEFILE; TOP 100 CROSSWALK
#============================================================#

# make tract shapefile
tracts.shp <- readOGR("tracts.shp")

# produce more workable crosswalk
top100_xwalk <- cbsa_xwalk %>% filter(top100==1) %>% 
  select(cbsa, cbsa_name, stcofips) 

#============================================================#
# CONSTRUCT DENSITY DATASET
#============================================================#

# function to add year indicator and to recode data for the map
recode_map_vars <- function(df){
  rv <- mutate(df,
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
               mapvar_5 = ifelse(dense_cat_20 >= 17, "cluster", "low density")
  )
  return(rv)
}

# gather data into long, num_quant represents number of quantiles 
# NB: 20 quantiles means tract in top 5% is a cluster
convert_to_long <- function(df, yr) {
  rv <- recode_map_vars(df) %>%
    mutate(year = yr) %>%
    gather("num_quant", "mapvar", contains("mapvar")) %>%
    # mutate(num_quant = gsub("most_dense", "quant", num_quant1)) %>%
    select(-contains("dense_cat"), -contains("most_dense"))
  return(rv)
}

# recode variables and drop extraneous vars
density <- bind_rows(convert_to_long(density15, 2015),
                     convert_to_long(density10, 2010)) %>%
  arrange(tract)

#============================================================#
# CALCULATE JOB MINIMUMS
#============================================================#

met_jobs <- density %>% filter(num_quant == "mapvar_20") %>%
  select(cbsa, cbsa_name, year, job_tot) %>%
  group_by(cbsa, cbsa_name, year) %>% summarize(job_tot = sum(job_tot, na.rm=TRUE)) %>% 
  mutate(zero_pp = job_tot * 0,
         oquart_pp = job_tot * 0.0025,
         half_pp = job_tot * 0.005,
         tquart_pp = job_tot * 0.0075,
         one_pp = job_tot * 0.01) %>%
  gather("pp_min", "job_min", contains("pp"), -cbsa, -cbsa_name, -year)

#============================================================#
# COMPUTE YEAR BY YEAR DESCRIPTIVE STATS
#============================================================#

# compute density cutoffs
compute_density_thresholds <- function(df, cbsa = cbsa_xwalk) {
  
  # prepare cbsa list, select vars of interest from density df
  cbsa <- cbsa %>% select(cbsa, cbsa_name, top100) %>% distinct() %>%
    filter(top100==1)
  rv <- df %>% select(cbsa, density) %>% 
    group_by(cbsa) %>% mutate(p80 = quantile(density, 0.8, na.rm=TRUE),
                              p85 = quantile(density, 0.85, na.rm=TRUE),
                              p90 = quantile(density, 0.9, na.rm=TRUE),
                              p95 = quantile(density, 0.95, na.rm=TRUE)) %>%
    select(-density) %>% summarize_all(first) %>%
    left_join(cbsa, by="cbsa") %>% filter(top100==1) %>%
    select(cbsa, cbsa_name, everything()) %>% select(-top100) %>%
    gather("percentile", "n" ,-cbsa, -cbsa_name)
  return(rv)
}

thresholds <- full_join(compute_density_thresholds(density10),
                        compute_density_thresholds(density15),
                        by = c("cbsa", "cbsa_name", "percentile"),
                        suffix = c("_2010", "_2015"))

source("calc_descriptive_stats.R")

save.image("data_for_shiny.Rdata")
