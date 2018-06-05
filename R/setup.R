##==================================================================================##
# GEO OF JOBS: SET GLOBAL PROJECT PATHS
#  Setting some global file path directories to streamline possible file moves
#
# Cecile Murray
# February 2018
##==================================================================================##

# loads required packages
libs <- c("tidyverse", "magrittr", "stringr", "readr", "openxlsx", "janitor", "sp",
          "maptools", "tigris", "censusapi", "broom", "here", "foreign", "readstata13",
          "sf", "leaflet", "plotly", "shiny", "sp", "rgdal")
lapply(libs, library, character.only=TRUE)

#============================================================#
# DIRECTORIES
#============================================================#

# directory with LEHD data
lehd_dir <- "V:/LEHD LODES"
# lehd_dir <- "D:/LEHD LODES/"

# geographic crosswalks
xwalk_dir <- "V:/Metro Poverty/CMM/xwalks/"
# xwalk_dir <- "C:/Users/cmurray/Documents/xwalks"

# Cecile's scripts with extra functions
scripts_dir <- "V:/Metro Poverty/CMM/scripts/"
# scripts_dir <- "C:/Users/cmurray/Documents/scripts"

#============================================================#
# FNS
#============================================================#
 
padz <- function(x, n=max(nchar(x))) gsub(" ", "0", formatC(x, width=n)) 

setwd(scripts_dir)
source("utils.R")
setwd(here())

# function to recode NAICS sectors into sensible names
convert_naics_to_name <- function(df, create_new_var = FALSE){
  var <- ifelse(create_new_var, "naics_label", "naics")

  rv <- mutate(df, !!var := case_when(
    naics == "naics_11" ~ "Logging",
    naics == "naics_21" ~ "Extraction",
    naics == "naics_22" ~ "Utilities",
    naics == "naics_23" ~ "Construction",
    naics == "naics_31to33" ~ "Manufacturing",
    naics == "naics_42" ~ "Wholesale",
    naics == "naics_44to45" ~ "Retail",
    naics == "naics_48to49" ~ "Logistics",
    naics == "naics_51" ~ "Information",
    naics == "naics_52" ~ "Finance & insurance",
    naics == "naics_53" ~ "Real estate",
    naics == "naics_54" ~ "Prof/sci/tech services",
    naics == "naics_55" ~ "Management",
    naics == "naics_56" ~ "Admin & waste management",
    naics == "naics_61" ~ "Education",
    naics == "naics_62" ~ "Health care",
    naics == "naics_71" ~ "Arts",
    naics == "naics_72" ~ "Hospitality",
    naics == "naics_81" ~ "Other services",
    naics == "naics_92" ~ "Public administration"
  )) %>% select(starts_with("naics"), everything())

  return(rv)
} 

