##==================================================================================##
# GEO OF JOBS: JOB THRESHOLD DESCRIPTIVE STATS
#  Examine effect of a job minimum threshold
#
# Cecile Murray
# May 2018
##==================================================================================##

library(here)
# source(here("map_clusters", "prepare_data.R"))


# Flag thresholds for all density thresholds at a given job minimum
flag_clusters <- function(df, job_min_df){
  # NB: job_min_df contains all cbsa ID-year combos but only one job min
  
  rv <- df %>% left_join(job_min_df, by = c("cbsa", "year")) %>% 
    mutate(
    mapvar = case_when(
      (mapvar == "cluster") & (job_tot > job_min) ~ "cluster",
      (mapvar == "cluster") & (job_tot <= job_min) ~ "high density",
      mapvar != "cluster" ~ mapvar
    ),
    tr_ct = 1,
    high_ct = ifelse(mapvar == "high density", 1, 0),
    cluster_ct = ifelse(mapvar == "cluster", 1, 0))
  
  return(rv)
}

# create job min data frame for top 100 metros at given job min
trim_job_min_df <- function(met_jobs, min_pp){
  # weird fix because I can't pass a string directly somehow
  min <- case_when(
    min_pp == 0 ~ "zero_pp",
    min_pp == 0.25 ~ "oquart_pp",
    min_pp == 0.5 ~ "half_pp",
    min_pp == 0.75 ~ "tquart_pp",
    min_pp == 1 ~ "one_pp",
    TRUE ~ "zero_pp"
  )
  job_min_df <- filter(ungroup(met_jobs), pp_min == min) %>%
    select(cbsa, year, job_min)
  return(job_min_df)
}


# Summarize cbsa clusters and jobs for a given density threshold and job min combo
summarize_by_metro <- function(df, min_pp, met_jobs = met_jobs, cbsa = cbsa_xwalk) {
  
  # prepare cbsa list and create job minimum df
  cbsa <- top100_coords %>% select(cbsa, cbsa_name)
  job_min_df <- met_jobs %>% trim_job_min_df(min_pp)
  
  # flag clusters
  rv <- df %>% filter(!is.na(cbsa)) %>% flag_clusters(job_min_df) 
  
  # group by cbsa and summarize, only include top 100 metros
  rv %<>% select(-tract, -ALAND_SQMI, -density, -cbsa_name) %>%
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

met_summary <- lapply(seq(0, 1, 0.25), function(x){
  summarize_by_metro(density, min_pp = x, met_jobs = met_jobs, cbsa = cbsa_xwalk)}) %>%
  bind_rows() %>% 
  mutate(cluster_share = cluster_ct / tr_ct,
         job_share = (cluster_jobs / jobs) * 100) %>% 
  mutate(num_quant = case_when(
    num_quant == "mapvar_20" ~ "95th percentile",
    num_quant == "mapvar_10" ~ "90th percentile",
    num_quant == "mapvar_5" ~ "80th percentile"
  ))

# write_excel_csv(met_summary, na = "", "met_summary.csv")

# formatting numbers for output
met_summary %<>% mutate(min = prettyNum(min),
                        cluster_ct = prettyNum(cluster_ct),
  cluster_jobs = prettyNum(cluster_jobs, big.mark = ","),
  job_share = prettyNum(job_share, digits = 2))


