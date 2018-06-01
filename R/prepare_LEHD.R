##==================================================================================##
# GEO OF JOBS: CREATE LEHD TRACT DATASET
#  Prepare LEHD dataset for analysis
#
# Cecile Murray
# March 2018
##==================================================================================##

# bringing in libraries, filepaths, other global vars
library(here)
source(here("R", "setup.R"))

#============================================================#
# LOAD GEOGRAPHIC DATA
#============================================================#

# county-CBSA xwalk
setwd(xwalk_dir)
cbsa_xwalk <- read.dta13("county metro xwalk.dta") %>%
  filter(metro_micro=="Metropolitan Statistical Area")
setwd(here())

# Land area files from the 2016 Census Gazetteer
cty_area <- read_tsv(here("raw", "2016_Gaz_counties_national.txt")) %>%
  dplyr::rename(stcofips = GEOID)
tract_area <- data.table::fread(here("raw", "2016_Gaz_tracts_national.txt"), integer64 = "numeric")
tract_area %<>% mutate(tract = padz(as.character(GEOID)),
                       stcofips = substr(tract, 1, 5))

#============================================================#
# BRING IN LEHD DATA
#============================================================#

setwd(paste0(lehd_dir, "/wac_JT00_2015"))
raw_lehd <- read.dta13("2015_ALL_wac_JT00.dta")
setwd(here())

# collapse to tract, merge with land area data
lehd_tract <- raw_lehd %>% mutate(tract = substr(fips, 1, 11)) %>%
  select(-fips) %>%
  group_by(tract) %>% summarize_all(sum, na.rm=TRUE) %>%
  recode_dumb_tracts() %>%
  left_join(select(tract_area, tract, ALAND_SQMI), by="tract") %>%
  mutate(jobs_sqmi = job_tot / ALAND_SQMI,
         hwage_share = wage_high / job_tot,
         hwage_density = wage_high / ALAND_SQMI,
         ba_share = baplus / job_tot,
         ba_density = baplus / ALAND_SQMI)

all_jobs <- lehd_tract %>% mutate(stfips = substr(tract, 1, 2),
                                  stcofips = substr(tract, 1, 5)) %>%
  select(tract, stfips, stcofips, job_tot, ALAND_SQMI, jobs_sqmi)

# save.image(file = here("temp", "prepped_LEHD_2015.Rdata"))
