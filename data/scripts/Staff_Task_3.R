##############################################################################
##  File name: Staff_Task_3.R                                               ##
##  Author:    Nathan Trenholm                                              ##
##  Created:   11/4/2016                                                    ##
##                                                                          ##
##  Description: Generates the staff_degrees_job_codes_clean file which:    ##
##                 1. Resolves instances in which an employee has multiple  ##
##                    degree levels in a single year.                       ##
##                 2. Indentifies a single job code for each individual     ##
##                    within each school year.                              ##
##                 3. One record per employee per year per school code.     ##
##                                                                          ##
##  Inputs:    data/raw/Staff_School_Year_Raw.dta                           ##
##  Outputs:   data/clean/staff_degrees_job_codes_clean.csv                 ##
##############################################################################



# Set working directory and load packages. ----------------------------------------------------

  setwd("C:/Users/natha/Desktop/SDP - Human Capital Toolkit")  

  library(compare)
  library(haven)
  library(magrittr)
  library(stringr)
  library(tidyverse)
  library(frequencies) # install_github('nathant23/Frequencies')


# Load raw data and prep for cleaning. ------------------------------------------------------------------------------

  staff_raw <- read_dta('data/raw/Staff_School_Year_Raw.dta')
  View(staff_raw)

  # Keep pertinent columns and remove duplicates.
  staff_raw %<>% select(tid, school_year, school_code, degree, job_code,
                        job_code_desc, experience, hire_date, termination_date) %>%
                 unique(.)
  

# Standardize the degree levels. --------------------------------------------------------------

  freq_table('staff_raw', 'degree')
  
  # Convert to upper case and then to common names.
  staff_raw$degree <- toupper(staff_raw$degree)
  
  staff_raw$degree <- ifelse(str_detect(staff_raw$degree, 'BA|A.B'), 'BACHELORS DEGREE', staff_raw$degree)
  staff_raw$degree <- ifelse(str_detect(staff_raw$degree, 'MA'),      'MASTERS DEGREE'  , staff_raw$degree)
  staff_raw$degree <- ifelse(str_detect(staff_raw$degree, 'PH'),     'DOCTORATE DEGREE', staff_raw$degree)
  freq_table('staff_raw', 'degree', sort_by_count = TRUE)
  
  # Convert degree names to numeric values
  staff_raw$degree_num <- recode(staff_raw$degree, `BACHELORS DEGREE` = 1L,
                                                   `MASTERS DEGREE` = 2L,
                                                   `DOCTORATE DEGREE` = 3L, .default = NA_integer_)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  