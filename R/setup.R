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
          "sf")
lapply(libs, library, character.only=TRUE)

#============================================================#
# DIRECTORIES
#============================================================#

# directory with LEHD data
lehd_dir <- "V:/LEHD LODES/"

# geographic crosswalks
xwalk_dir <- "V:/Metro Poverty/CMM/xwalks/"

# Cecile's scripts with extra functions
scripts_dir <- "V:/Metro Poverty/CMM/scripts/"

#============================================================#
# FNS
#============================================================#
 
padz <- function(x, n=max(nchar(x))) gsub(" ", "0", formatC(x, width=n)) 

setwd(scripts_dir)
source("utils.R")
setwd(here())