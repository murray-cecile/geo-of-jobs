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

totemp15 <- emp %>% select(stcofips, industry, y2015) %>% filter(industry=="RET")

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
