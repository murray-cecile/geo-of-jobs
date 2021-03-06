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
         naics = ifelse(substr(naics, 7, 8)=="GV", "naics_GV", naics)) %>%
  select(-industry) %>% group_by(stcofips, naics) %>% summarize_all(sum, na.rm=TRUE)

#============================================================#
# COMPARE JOB TOTALS BY INDUSTRY
#============================================================#

tot_check <- lehd %>% select(stcofips, job_tot) %>% 
  left_join(totemp15, by="stcofips") %>%
  mutate(m_tot = y2015 * 1000,
         delta = job_tot - m_tot,
         percent_delta = delta / m_tot) %>% filter(!is.na(m_tot))

ggplot(tot_check) +
  geom_histogram(aes(x = percent_delta))

# in total, LEHD undershoots Moody's by just under 3.8M jobs
sum(tot_check$delta, na.rm=TRUE) / sum(tot_check$m_tot, na.rm=TRUE)

# now check by naics sector across all counties
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
  select(-stcofips) %>% group_by(naics) %>% summarise_all(sum, na.rm=TRUE) %>%
  mutate(m_tot = y2015 * 1000,
         delta = jobs - m_tot,
         percent_delta = delta / m_tot, 
         short_name = gsub("naics_", "", naics)) %>%
  filter(!is.na(m_tot), naics!="naics_11") 

# this shows some significant differences by sector
ggplot(gather(naics_check, key = "source", value = "job_ct", jobs, m_tot)) +
  geom_col(aes(x = reorder(short_name, -job_ct), y = job_ct, group = source,
               fill = source), position = "dodge")

# by industry, percentage differences are mostly small except in 61, 22, GV, 81
ggplot(naics_check) +
  geom_col(aes(x = reorder(short_name, -percent_delta), y = percent_delta*100))

#============================================================#
# COMPARE JOB TOTALS BY METRO & INDUSTRY
#============================================================#

# collapse job totals to metro
met_check <- lehd %>% select(stcofips, job_tot) %>%
  left_join(totemp15, by="stcofips") %>%
  left_join(select(cbsa_xwalk, stcofips, cbsa, top100), by="stcofips") %>%
  select(-stcofips, -industry) %>% group_by(cbsa) %>% summarize_all(sum, na.rm=TRUE) %>%
  mutate(m_tot = y2015 * 1000,
         delta = job_tot - m_tot,
         percent_delta = delta / m_tot) %>% 
  filter(top100 > 0) %>%
  left_join(select(cbsa_xwalk, cbsa, cbsa_name), by="cbsa") %>% distinct() 

# wide range of job deltas, unfortunately.
summary(met_check$delta)
summary(met_check$percent_delta)

ggplot(met_check) +
  geom_col(aes(x = reorder(short_name, -delta), y = delta)) 

# identify the ones with a wider gap than the nation overall
met_check %<>% mutate(wide_gap = ifelse(abs(percent_delta) >
                                          abs(sum(tot_check$delta, na.rm=TRUE)/sum(tot_check$m_tot, na.rm=TRUE)),
                                        1, 0))
# plot the subset
ggplot(filter(met_check, wide_gap==1)) +
  geom_col(aes(x = reorder(short_name, -percent_delta), y = percent_delta))
  
# collapse job totals by metro by industry
met_naics_check <- lehd %>% select(stcofips, job_tot, contains("naics")) %>%
  gather(key = "naics", value ="jobs", -stcofips, -job_tot) %>%
  mutate(naics2 = gsub("naics_", "", naics),
         naics2 = ifelse(naics2=="31to33", "MF",
                         ifelse(naics2=="44to45", "RT",
                                ifelse(naics2=="48to49", "RW", 
                                       ifelse(naics2=="92", "GV", naics2)))),
         naics = paste0("naics_", naics2)) %>% 
  select(-naics2) %>%
  left_join(select(emp, stcofips, naics, y2015), by=c("stcofips", "naics")) %>%
  left_join(select(cbsa_xwalk, stcofips, cbsa, top100), by="stcofips") %>%
  filter(top100==1) %>%
  select(-stcofips) %>% group_by(cbsa, naics) %>% summarize_all(sum, na.rm=TRUE) %>%
  mutate(m_tot = y2015 * 1000,
         delta = jobs - m_tot,
         percent_delta = ifelse(m_tot > 0, delta / m_tot, 0),
         delta_share = ifelse(job_tot > 0, delta / job_tot, 0))

bad_naics <- c("naics_22", "naics_61", "naics_62", "naics_GV")
summary(met_naics_check$percent_delta[!met_naics_check$naics %in% bad_naics])
summary(met_naics_check$delta_share)

ggplot(filter(met_naics_check, !naics %in% bad_naics)) +
  geom_histogram(aes(x = delta_share))

