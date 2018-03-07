##==================================================================================##
# GEO OF JOBS: PULL POPULATION AND POP DENSITY
#  Pull population totals and compute population density
#
# Cecile Murray
# March 2018
##==================================================================================##

# bringing in libraries, filepaths, other global vars
library(here)
source(here("R", "setup.R"))

#============================================================#
# PULL FROM API
#============================================================#

# get API key
setwd(scripts_dir)
source("api_keychain.R")
setwd(here())
census_key <- Sys.getenv("CENSUSAPI_KEY")

# pull 5-year population data
pop16 <- lapply(unique(fips_codes$state_code)[1:51], 
                function(x) {getCensus(name = "acs/acs5", vintage = 2016,
                                       key = census_key, vars = "B01001_001E",
                                       region = "tract:*", regionin = paste0("state:", x))}) %>%
  bind_rows() %>% clean_api_pull("tract") %>%
  dplyr::rename(pop = B01001_001E)

#============================================================#
# COMPUTE POPULATION DENSITY
#============================================================#

# merge and compute population density
pop_density <- left_join(pop16, tract_area, by="tract") %>%
  mutate(pop_sqmi = pop / ALAND_SQMI) %>%
  select(tract, stcofips, pop, ALAND_SQMI, pop_sqmi)

# identify most dense tracts based on population
# interestingly, these tracts contain a much smaller share of population than the job dense ones
most_pop_dense <- filter(pop_density, ntile(pop_sqmi, 100)>97) # 2,182 tracts
sum(most_pop_dense$pop, na.rm=TRUE) / sum(pop_density$pop, na.rm=TRUE)

