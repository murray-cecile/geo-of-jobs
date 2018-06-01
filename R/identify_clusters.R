##==================================================================================##
# GEOGRAPHY OF JOBS: IDENTIFY JOB CLUSTERS
#  Script to identify job clusters in the largest 100 metros
#
# Cecile Murray
# April 2018; Updated June 2018
##==================================================================================##

# Outline:
# 1. Bind years together, compute job density
# 2. Flag tracts in top 20th, 10th, and 5th percentiles of density.
# 3. Recode to indicate tract categorization, convert to long.
# 4. Compute a variety of job minimum thresholds for each metro


# bringing in libraries, filepaths, other global vars
library(here)
source(here("R", "setup.R"))

load(here("temp", "prepped_LEHD_2010_2015.Rdata"))


#============================================================#
# COMBINE YEARS; COMPUTE DENSITY
#============================================================#

# combine years of data
lehd <- bind_rows(mutate(lehd_tract_10, year = 2010),
                  mutate(lehd_tract_15, year = 2015))

# take collapsed LEHD, compute density w/rt given variable, add metro
compute_tract_density <- function(df, var, cbsa = cbsa_xwalk) {
  var <- enquo(var)
  rv <- df %>% select(tract, year, !!var, ALAND_SQMI) %>% 
    mutate(density  = UQ(var) / ALAND_SQMI,
           stcofips = substr(tract, 1, 5)) %>%
    left_join(select(cbsa, cbsa, cbsa_name, stcofips), by="stcofips") %>%
    select(-stcofips)
  
  return(rv)
}

density <- compute_tract_density(lehd, var = job_tot, cbsa = cbsa_xwalk)

#============================================================#
# CREATE DENSITY FLAGS
#============================================================#

# flag tracts in the xth percentile of density by cbsa
flag_dense_tracts <- function(df, num_cats) {
  
  cat_name <- paste0("dense_cat_", num_cats)
  dense_name <- paste0("most_dense_", num_cats)

  rv <- df %>% group_by(cbsa, year) %>%
    mutate(UQ(cat_name) := ntile(density, num_cats)) %>%
    ungroup()
  
  # create boolean for top ntile
  rv[, c(dense_name)] <- as.vector(rv[, c(cat_name)]==num_cats)
  
  return(rv)
}

# apply function for 20th, 10th, 5th percentile
density %<>% flag_dense_tracts(20) %>% 
  flag_dense_tracts(10) %>% 
  flag_dense_tracts(5)

#============================================================#
# CATEGORIZE TRACTS BY DENSITY
#============================================================#

# function to recode data to indicate which category a tract falls in
recode_map_vars <- function(df){
  rv <- mutate(df,
               mapvar_20 = case_when(
                 dense_cat_20 < 17 | is.na(dense_cat_20) ~ "low density",
                 dense_cat_20 == 17 ~ "p80",
                 dense_cat_20 == 18 ~ "p85",
                 dense_cat_20 == 19 ~ "p90",
                 dense_cat_20 == 20 ~ "cluster"
               ), 
               mapvar_10 = case_when(
                 dense_cat_20 < 17 | is.na(dense_cat_20) ~ "low density",
                 dense_cat_20 == 17 ~ "p80",
                 dense_cat_20 == 18 ~ "p85",
                 dense_cat_10 == 10 ~ "cluster"
               ), 
               mapvar_5 = ifelse(dense_cat_20 >= 17, "cluster", "low density")
  )
  return(rv)
}

# gather data into long, num_quant represents number of quantiles 
# NB: 20 quantiles means tract in top 5% is a cluster
density %<>% recode_map_vars() %>%
  gather("num_quant", "mapvar", contains("mapvar")) %>%
  select(-contains("dense_cat"), -contains("most_dense"))

#============================================================#
# CALCULATE MINIMUM JOB SHARES BY METRO
#============================================================#

# calculate minimum jobs for each metro in each year at each minimum percentage
met_jobs <- density %>% filter(num_quant == "mapvar_20") %>%
  select(cbsa, cbsa_name, year, job_tot) %>%
  group_by(cbsa, cbsa_name, year) %>% summarize(job_tot = sum(job_tot, na.rm=TRUE)) %>% 
  mutate(zero_pp = job_tot * 0,
         oquart_pp = job_tot * 0.0025,
         half_pp = job_tot * 0.005,
         tquart_pp = job_tot * 0.0075,
         one_pp = job_tot * 0.01) %>%
  gather("pp_min", "job_min", contains("pp"), -cbsa, -cbsa_name, -year) %>%
  ungroup()

#============================================================#
# IMPLEMENT MINIMUM JOB SHARE THRESHOLD
#============================================================#

# identify relevant job minimum and return that value
id_job_min <- function(min_pp, met_jobs = met_jobs){
  
  min <- case_when(
    min_pp == 0 ~ "zero_pp",
    min_pp == 0.25 ~ "oquart_pp",
    min_pp == 0.5 ~ "half_pp",
    min_pp == 0.75 ~ "tquart_pp",
    min_pp == 1 ~ "one_pp",
    TRUE ~ "zero_pp"
  )
  
  mins <- met_jobs %>% filter(pp_min == min) %>% select(cbsa, year, job_min)
  return(mins)
}

# function to recode mapped variable to filter out low job count tracts
recode_job_min <- function(df, min_pp, met_jobs = met_jobs){
  
  mins <- id_job_min(min_pp = min_pp, met_jobs = met_jobs)
  
  # now recode the mapvar variable to reflect minimim job threshold
  rv <- df %>% left_join(mins, by = c("cbsa", "year")) %>% 
  mutate(pp_min = min_pp, 
         mapvar = case_when(
           (mapvar == "cluster") & (job_tot > job_min) ~ "cluster",
           (mapvar == "cluster") & (job_tot <= job_min) ~ "high density",
           mapvar != "cluster" ~ mapvar
         ))
  
  return(rv)
}


# apply job minimum threshold for 0.25, 0.5, and 0.75; bind together
density <- lapply(seq(0.25, 0.75, 0.25), 
                    function(x) {recode_job_min(density, x, met_jobs = met_jobs)}) %>%
  bind_rows()

# save.image(here("temp", "cluster_options_2010_2015.Rdata"))






