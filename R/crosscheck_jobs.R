##==================================================================================##
# GEO OF JOBS: CROSS-CHECK EMPLOYMENT TOTALS
#  Compare employment totals by county
#
# Cecile Murray
# March 2018
##==================================================================================##

library(here)
source(here("R", "setup.R"))

load(here("temp", "prepped_LEHD_2015.Rdata"))

# collapse LEHD data to counties
lehd <- raw_lehd %>% mutate(stcofips = substr(fips, 1, 5)) %>%
  select(-fips) %>% group_by(stcofips) %>% summarize_all(sum, na.rm=TRUE)

#============================================================#
# PULL IN MOODY'S DATA
#============================================================#

# bring in the dataset
setwd("P:/Projects/Older Industrial Cities/Analysis/code/older_industrial_cities")
source("R/pull_employment_data.R")
rm(list=setdiff(ls(), "cty_comp"))
setwd(here())

emp <- separate(cty_comp, DBcode, c("industry", "sep", "geo"), sep = "A") %>%
  dplyr::rename_at(vars(num_range("", 1970:2016)), funs(paste0("y", .))) %>%
  select(stcofips, industry, num_range("y", 2002:2016))

# rm(cty_comp)

# split off total employment into its own data frame
totemp15 <- emp %>% select(stcofips, industry, y2015) %>% filter(industry=="RET")

# modify emp to better match LEHD NAICS codes
emp %<>% filter(industry!="RET") %>%
  mutate(naics = gsub("RE", "naics_", industry),
         naics = ifelse(substr(naics, 7, 8)=="GV", "naics_GV", naics))

#============================================================#
# COMPARE COUNTY TOTALS
#============================================================#

tot_check <- lehd %>% select(stcofips, job_tot) %>% 
  left_join(totemp15, by="stcofips") %>%
  mutate(m_tot = y2015 * 1000,
         delta = job_tot - m_tot,
         percent_delta = delta / m_tot) %>% filter(!is.na(m_tot))

ggplot(tot_check) +
  geom_histogram(aes(x = percent_delta))

# in total, LEHD undershoots Moody's by just under 3.8M jobs
sum(tot_check$delta, na.rm=TRUE)

# now check by naics sector
naics_check <- lehd %>% select(stcofips, contains("naics")) %>%
  gather(key = "naics", value ="jobs", -stcofips) %>%
  mutate(naics2 = gsub("naics_", "", naics),
         naics2 = ifelse(naics2=="31to33", "MF",
                         ifelse(naics2=="44to45", "RT",
                                ifelse(naics2=="48to49", "RW", 
                                       ifelse(naics2=="92", "GV", naics2)))),
         naics = paste0("naics_", naics2)) %>% 
  select(-naics2) %>%
  left_join(select(emp, stcofips, naics, y2015), by=c("stcofips", "naics")) %>%
  mutate(m_tot = y2015 * 1000,
         delta = jobs - y2015,
         percent_delta = delta / m_tot) %>%
  filter(!is.na(m_tot))

ggplot(naics_check) +
  geom_histogram(aes(x = percent_delta, group = naics))
