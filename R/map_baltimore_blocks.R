##==================================================================================##
# GEO OF JOBS: MAKE BALTIMORE BLOCK LEVEL MAP
#  Demo block-level data for Jennifer
#
# Cecile Murray
# April 2018
##==================================================================================##

library(here)
source(here("R", "setup.R"))
source(here("R", "identify_clusters.R"))

#============================================================#
# BALTIMORE
#============================================================#

bm <- filter(raw_lehd, substr(fips, 1, 5) %in% 
               cbsa_xwalk$stcofips[cbsa_xwalk$cbsa=="12580"])

# # compute density w/rt given variable, add metro
# compute_tract_density <- function(df, area_df, var, cbsa = cbsa_xwalk) {
#   var <- enquo(var)
#   rv <- df %>% select(fips, !!var) %>% 
#     left_join(select(area_df, tract, ALAND_SQMI, stcofips), by="tract") %>%
#     mutate(density  = UQ(var) / ALAND_SQMI) %>%
#     left_join(select(cbsa, cbsa, cbsa_name, stcofips), by="stcofips") %>%
#     select(-stcofips)
#   
#   return(rv)
# }