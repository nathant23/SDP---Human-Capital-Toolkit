##########################################################################
##  File name: School_Task_1.R                                          ##
##  Author:    Nathan Trenholm                                          ##
##  Created:   10/23/2016                                               ##
##  Updates:                                                            ##
##                                                                      ##
##  Description: This script generates the school_clean file which has: ##
##      1. One record per school                                        ##
##      2. Indicates school level                                       ##
##                                                                      ##
##  Inputs:  data/raw/School_Raw.dta                                    ##
##  Outputs: data/clean/school_clean.csv                                ##
##########################################################################


# Set working directory and load packages ---------------------------------

  setwd("C:/Users/Nathan/Desktop/SDP - Human Capital Toolkit")
  
  library(haven)  
  library(tidyverse)
  library(compare)


# Read File & Remove Duplicates -------------------------------------------

  school_raw <- read_dta('data/raw/School_Raw.dta')
  View(school_raw) 
  nrow(school_raw) == length(unique(school_raw$school_code)) # No duplicate school_codes.

  
# Recode and create level columns -----------------------------------------

  # Get unique codes to be recoded.
  unique(school_raw$school_lvl)	  #  Returned:  ""     "Elem" "Mid"  "High"
  unique(school_raw$alternative) 	#  Returned:  "No"  "Yes" "."
  
  # Create columns required for the final clean file with recoded data.
  school_raw$elementary <- ifelse(school_raw$school_lvl == 'Elem', 1,0)
  school_raw$middle     <- ifelse(school_raw$school_lvl == 'Mid' , 1,0)
  school_raw$high       <- ifelse(school_raw$school_lvl == 'High', 1,0)
  
  school_raw$alternative <- as.numeric(recode(school_raw$alternative, No = '0', Yes = '1', .default = NA_character_))
  
  school_clean <- school_raw[,c('school_code', 'school_name', 'school_lvl', 'elementary', 'middle', 'high', 'alternative')]

  
# Read in clean SDP file to check results. --------------------------------

  sdp_school_clean  <- read_dta('data/sdp_clean/School_Clean.dta')
  # Looks like there is an error in the SDP file for the middle school column.  The rest matches.
  compare(school_clean, sdp_school_clean)
  compare(school_clean, sdp_school_clean, allowAll = TRUE)
  
  
# Write to file. ----------------------------------------------------------

  write.csv(school_clean, file = 'data/clean/school_clean.csv', row.names = FALSE)
