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

# 2015
setwd(paste0(lehd_dir, "/wac_JT00_2015"))
raw_lehd_15 <- read.dta13("2015_ALL_wac_JT00.dta")

# 2010
setwd(paste0(lehd_dir, "/wac_JT00_2010"))
raw_lehd_10 <- read.dta13("2010_ALL_wac_JT00.dta")

# change working directory back to main directory
setwd(here())

#============================================================#
# COLLAPSE LEHD
#============================================================#

# collapse to tract, merge with land area data
collapse_to_tract <- function(lehd_df, area = tract_area){  
  
  area %<>% select(tract, ALAND_SQMI)
  
  rv <- lehd_df %>% mutate(tract = substr(fips, 1, 11)) %>%
    select(-fips) %>%
    group_by(tract) %>% summarize_all(sum, na.rm=TRUE) %>%
    recode_dumb_tracts() %>%
    left_join(area, by="tract") 
  
  return(rv)
}

lehd_tract_10 <- collapse_to_tract(raw_lehd_10)
lehd_tract_15 <- collapse_to_tract(raw_lehd_15)

# rm(raw_lehd_10, raw_lehd_15)
# save.image(file = here("temp", "prepped_LEHD_2010_2015.Rdata"))
