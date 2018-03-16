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
  select(tract, stcofips, pop, ALAND_SQMI, pop_sqmi) %>%
  left_join(select(cbsa_xwalk, stcofips, cbsa, cbsa_name), by="stcofips") %>%
  mutate(short_name = substr(cbsa_name, 1, regexpr("-", cbsa_name)-1),
         short_name = ifelse(short_name=="", cbsa_name, short_name))


# identify most dense tracts based on population
# interestingly, these tracts contain a much smaller share of population than the job dense ones
most_pop_dense <- filter(pop_density, ntile(pop_sqmi, 100)>97) # 2,182 tracts
sum(most_pop_dense$pop, na.rm=TRUE) / sum(pop_density$pop, na.rm=TRUE)

# relationship between tract size and population is a bit negative, unsurprisingly
ggplot(pop_density) +
  geom_point(aes(x = ALAND_SQMI, y = pop)) +
  geom_smooth(aes(x = ALAND_SQMI, y = pop), method = lm)

pop_density$ALAND_SQMI[which.max(pop_density$pop)]

#============================================================#
# RELATIONSHIP BETWEEN JOB DENSITY AND POP DENSITY
#============================================================#

density <- left_join(all_jobs, pop_density, by=c("tract", "stcofips", "ALAND_SQMI")) %>%
  filter(!is.na(ALAND_SQMI), ALAND_SQMI > 0)

density$tract[which(is.na(density$jobs_sqmi))]

# a weakish correlation in the subset of tracts in both universes 
with(density, cor(jobs_sqmi, pop_sqmi))


