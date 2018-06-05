##==================================================================================##
# GEO OF JOBS: CALCULATE INDUSTRY DESCRIPTIVE STATS
#  Industry growth patterns and LQs
#
# Cecile Murray
# June 2018
##==================================================================================##

library(here)
source(here("R", "setup.R"))
load(here("temp", "cluster_options_2010_2015.Rdata"))

# first, gather industry data
industry <- lehd %>% filter(!substr(tract, 1, 2) %in% c("25", "56")) %>%
  select(tract, year, job_tot, ALAND_SQMI, starts_with("naics")) %>% 
  gather("naics", "jobs", -tract, -year, -ALAND_SQMI, -job_tot)

#============================================================#
# INDUSTRY SUMMARIES
#============================================================#

# create table holding national totals
industry_totals <- industry %>% select(-tract, -ALAND_SQMI, -job_tot) %>%  
  group_by(year, naics) %>% summarize_all(sum, na.rm=TRUE) %>%
  spread(year, jobs, sep = "_") %>% 
  dplyr::rename(emp_2010 = year_2010, emp_2015 = year_2015) %>% 
  mutate(tot_2010 = sum(emp_2010),
         tot_2015 = sum(emp_2015),
         share_2010 = (emp_2010 / tot_2010) * 100,
         share_2015 = (emp_2015 / tot_2015) * 100,
         job_change = emp_2015 - emp_2010,
         share_change = share_2015 - share_2010) %>% 
  select(-starts_with("tot")) %>%
  select(naics, contains("2010"), contains("2015"), contains("change")) %>% 
  convert_naics_to_name(create_new_var = TRUE) %>% 
  adorn_totals(which = "row")

# write_excel_csv(industry_totals, path = here("output", "industry totals.csv"))

# compute industry share of jobs in clusters
industry_shares <- select(density, tract, year, num_quant, pp_min, mapvar) %>% 
  filter(num_quant == "mapvar_10") %>% 
  left_join(industry, by = c("tract", "year")) %>% 
  select(-tract, -ALAND_SQMI) %>% 
  group_by(year, num_quant, pp_min, mapvar, naics) %>%
  summarize_all(sum, na.rm=TRUE) %>% 
  filter(mapvar == "cluster", !is.na(naics)) %>% 
  mutate(share = (jobs / job_tot) * 100) %>% select(-job_tot, -jobs) %>% 
  spread(year, share) %>% clean_names() %>%  
  left_join(select(industry_totals, naics, share_2010, share_2015),
            by = "naics") %>% 
  mutate(lq_2010 = x2010 / share_2010, lq_2015 = x2015 / share_2015) %>% 
  ungroup() %>% select(-num_quant, -mapvar) %>%
  convert_naics_to_name(create_new_var = TRUE)

# write_excel_csv(industry_shares, path = here("output", "industry cluster shares.csv"))
