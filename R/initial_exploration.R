##==================================================================================##
# GEO OF JOBS: CREATE LEHD FILE
#  Clean, prep, and explore a year of LEHD data
#
# Cecile Murray
# February 2018 
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

setwd(paste0(lehd_dir, "wac_2015"))
raw_lehd_15 <- read.dta13("2015_ALL_wac.dta")
setwd(here())

# collapse to tract, merge with land area data
lehd_tract <- raw_lehd_15 %>% mutate(tract = substr(fips, 1, 11)) %>%
  select(-fips) %>%
  group_by(tract) %>% summarize_all(sum, na.rm=TRUE) %>%
  recode_dumb_tracts() %>%
  left_join(select(tract_area, tract, ALAND_SQMI), by="tract") %>%
  mutate(jobs_sqmi = job_tot / ALAND_SQMI,
         hwage_share = wage_high / job_tot,
         hwage_density = wage_high / ALAND_SQMI,
         ba_share = baplus / job_tot,
         ba_density = baplus / ALAND_SQMI)

#============================================================#
# BASIC SUMMARIES
#============================================================#

all_jobs <- lehd_tract %>% mutate(stfips = substr(tract, 1, 2),
                                  stcofips = substr(tract, 1, 5)) %>%
  select(tract, stfips, stcofips, job_tot, ALAND_SQMI, jobs_sqmi)

ggplot(filter(all_jobs, substr(tract, 1, 5) %in% cbsa_xwalk$stcofips)) +
  geom_histogram(aes(x = jobs_sqmi))

# state level summary
st_summary <- all_jobs %>% select(-tract, -stcofips) %>%
  group_by(stfips) %>% summarize_all(sum, na.rm=TRUE) %>%
  mutate(jobs_sqmi = job_tot / ALAND_SQMI)
# View(st_summary)

# county level summary
cty_summary <- all_jobs %>% select(-tract, -stfips) %>%
  group_by(stcofips) %>% summarize_all(sum, na.rm=TRUE) %>%
  mutate(jobs_sqmi = job_tot / ALAND_SQMI)

# what percentage of jobs do tracts in the top decile capture? About 1/3.
# what about the top 5%? A little over 20%. 
# and the top 2% contain just about 1/6 of all jobs
most_dense <- filter(all_jobs, ntile(jobs_sqmi, 100)>97)
sum(most_dense$job_tot, na.rm=TRUE) / sum(all_jobs$job_tot, na.rm=TRUE)

