##==================================================================================##
# GEO OF JOBS: CALCULATE NATIONAL DESCRIPTIVE STATISTICS
#  National stats on the numbers of jobs in clusters, etc
#
# Cecile Murray
# June 2018
##==================================================================================##

library(here)
load(here("temp", "cluster_options_2010_2015.Rdata"))

#============================================================#
# NATIONAL SUMMARY
#============================================================#

# screen out Massachusetts and Wyoming to maximize comparability
# Compute national job total
natl_job_tot <- lehd %>% filter(!substr(tract, 1, 2) %in% c("25", "56")) %>%  
  select(year, job_tot) %>% group_by(year) %>%
  summarize(natl_job_tot = sum(job_tot, na.rm = TRUE))

# compute national job total in top 100 metro areas
top100_stcofips <- unique(cbsa_xwalk$stcofips[cbsa_xwalk$top100 == 1]) 
top100_job_tot <- lehd %>% filter(!substr(tract, 1, 2) %in% c("25", "56"),
                                  substr(tract, 1, 5) %in% top100_stcofips) %>% 
  group_by(year) %>% summarize(natl_job_tot = sum(job_tot, na.rm = TRUE))

# Compute number and share of jobs in clusters for variety of thresholds
compute_natl_clusters <- function(df, yr, natl_jobs_df = natl_job_tot){
  
  natl_jobs <- filter(natl_jobs_df, year == yr)
  
  # again, screening out MA and WY
  rv <- df %>% filter(year == yr, !substr(tract, 1, 2) %in% c("25", "56")) %>% 
    select(num_quant, year, mapvar, pp_min, job_tot) %>%
    group_by(year, num_quant, pp_min, mapvar) %>%
    summarize(jobs = sum(job_tot, na.rm=TRUE)) %>%
    mutate(natl_jobs = natl_jobs$natl_job_tot,
           share = jobs / natl_jobs) %>%
    filter(mapvar == "cluster") %>% ungroup() %>% 
    select(-mapvar, -natl_jobs, -year) %>%
    mutate(share = share * 100,
           Density = case_when(
             num_quant == "mapvar_20" ~ "Top 5 percent",
             num_quant == "mapvar_10" ~ "Top 10 percent",
             num_quant == "mapvar_5" ~ "Top 20 percent"
           )) %>%
    select(-num_quant) %>%
    dplyr::rename(`Minimum job share` = pp_min,
                  `Jobs in clusters` = jobs,
                  `% of jobs in clusters` = share) %>%
    select(Density, everything())
  
  return(rv)
}

natl_clusters <- merge(compute_natl_clusters(density, 2010),
                       compute_natl_clusters(density, 2015),
                       by = c("Density", "Minimum job share"),
                       suffix = c(" 2010", " 2015"))

natl_clusters_met <- merge(compute_natl_clusters(density, 2010, natl_jobs_df = top100_job_tot),
                           compute_natl_clusters(density, 2015, natl_jobs_df = top100_job_tot),
                           by = c("Density", "Minimum job share"),
                           suffix = c(" 2010", " 2015"))

# export this table
# write_excel_csv(natl_clusters, here("output", "National cluster shares.csv"))

#============================================================#
# METRO-LEVEL SUMMARIES
#============================================================#

# Summarize cbsa clusters and jobs for a given density threshold and job min combo
summarize_by_metro <- function(df, min_pp, filter_st = TRUE, 
                               met_jobs = met_jobs, cbsa = cbsa_xwalk) {
  
  # prepare cbsa list 
  cbsa <- filter(cbsa_xwalk, top100 == 1) %>% select(cbsa, cbsa_name) %>% distinct()
 
  # filter out Wyoming and Massachusetts
  if(filter_st){
    rv <- df %>% filter(!substr(tract, 1, 2) %in% c("25", "56"))
    cbsa %<>% filter(cbsa != "14460")
  } else {
    rv <- df
  }
  
  # group by cbsa and summarize, only include top 100 metros
  rv %<>% filter(pp_min == min_pp) %>% 
    select(-tract, -ALAND_SQMI, -density, -cbsa_name) %>%
    mutate(tr_ct = 1,
           high_ct = ifelse(mapvar == "high density", 1, 0),
           cluster_ct = ifelse(mapvar == "cluster", 1, 0)) %>% 
    group_by(cbsa, year, num_quant) %>%
    summarize(jobs = sum(job_tot, na.rm=TRUE),
              cluster_jobs = sum(job_tot * cluster_ct, na.rm=TRUE),
              tr_ct = sum(tr_ct),
              high_ct = sum(high_ct, na.rm=TRUE),
              cluster_ct = sum(cluster_ct, na.rm=TRUE)) %>%
    left_join(select(cbsa, cbsa, cbsa_name), by="cbsa") %>% 
    filter(!is.na(cbsa_name)) %>%
    mutate(min = min_pp) %>% 
    select(cbsa, cbsa_name, year, num_quant, min, everything())
  
  return(rv)
}

met_summary <- lapply(seq(0.25, 0.75, 0.25), function(x){
  summarize_by_metro(density, min_pp = x, met_jobs = met_jobs, cbsa = cbsa_xwalk)}) %>%
  bind_rows() %>% 
  mutate(cluster_share = cluster_ct / tr_ct,
         job_share = (cluster_jobs / jobs) * 100) %>% 
  mutate(num_quant = case_when(
    num_quant == "mapvar_20" ~ "95th percentile",
    num_quant == "mapvar_10" ~ "90th percentile",
    num_quant == "mapvar_5" ~ "80th percentile"
  ))

# View(select(ungroup(met_summary), cbsa_name, size_cat) %>% distinct())

met_export <- merge(filter(met_summary, year == 2010),
                    filter(met_summary, year == 2015),
                    by = c("cbsa", "cbsa_name", "num_quant", "min"),
                    suffix = c(" 2010", "2015")) %>% 
  select(-starts_with("year"))

# write_excel_csv(met_export, here("output", "metro cluster shares.csv"))
