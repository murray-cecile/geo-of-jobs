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

load(here("temp", "prepped_LEHD_2015.Rdata"))

#============================================================#
# PLOT, SUMMARIES
#============================================================#

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

#============================================================#
# IDENTIFYING DENSITY
#============================================================#

# what percentage of jobs do tracts in the top decile capture? About 1/3.
# what about the top 5%? A little over 20%. 
# and the top 2% contain just about 1/6 of all jobs
most_dense <- filter(all_jobs, ntile(jobs_sqmi, 100)>97)
sum(most_dense$job_tot, na.rm=TRUE) / sum(all_jobs$job_tot, na.rm=TRUE)

# map that
ggplot(left_join(counties.df, most_dense, by="stcofips")) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = jobs_sqmi)) +
  coord_map() +
  theme_minimal()

# how many metros are represented? 188
length(unique(cbsa_xwalk$cbsa_name[cbsa_xwalk$stcofips %in% unique(most_dense$stcofips)]))

# okay, let's look at metro status: 
most_dense %<>% left_join(select(cbsa_xwalk, stcofips, cbsa, cbsa_name, top100), by="stcofips")
length(which(is.na(most_dense$cbsa))) # 4 non-metro tracts!
# View(most_dense[is.na(most_dense$cbsa), ]) # includes two universities

# how many in top 100?
sum(most_dense$top100, na.rm = TRUE) # 2,022 of 2,173, approx 93%
# View(most_dense[most_dense$top100==0, ])

#============================================================#
# TOP 100 METROS
#============================================================#

# okay, how do the top 100 metros shake out? # NYC, LA dominate. 
top100_summary <- data.frame(with(most_dense, table(cbsa_name)))

# mean density by metro?
top100_summary <- left_join(all_jobs,
                            select(cbsa_xwalk, stcofips, cbsa, cbsa_name, top100),
                            by="stcofips") %>%
  filter(top100==1, ALAND_SQMI > 0) %>% 
  select(cbsa, cbsa_name, job_tot, ALAND_SQMI, jobs_sqmi) %>%
  group_by(cbsa, cbsa_name) %>% summarize_all(median, na.rm=TRUE) %>%
  left_join(top100_summary, by="cbsa_name") %>%
  mutate(short_name = substr(cbsa_name, 1, regexpr("-", cbsa_name)-1),
         short_name = ifelse(short_name=="", cbsa_name, short_name))
  

ggplot(top100_summary) +
  geom_col(aes(x = reorder(short_name, -jobs_sqmi), y = jobs_sqmi))
