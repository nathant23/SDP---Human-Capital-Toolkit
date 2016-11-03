#############################################################################
##  File name: Staff_Task_2.R                                              ##
##  Author:    Nathan Trenholm                                             ##
##  Created:   10/24/2016                                                  ##
##  Updates:                                                               ##
##                                                                         ##
##  Description: Generates the staff_certifications_clean file which has:  ##
##    1. Identify teachers with special certifications                     ##
##    2. Determine year teachers became certified and                      ##
##       the year certifications expired                                   ##
##    2. One record per teacher per year                                   ##
##                                                                         ##
##  Inputs:  data/raw/staff_certifications.dta                             ##
##  Outputs: data/clean/staff_certifications_clean.csv                     ##
#############################################################################


# Set working directory and load packages ---------------------------------

  setwd("C:/Users/natha/Desktop/SDP - Human Capital Toolkit")

  library(compare)
  library(haven)
  library(lubridate)
  library(magrittr)
  library(tidyverse)


# Load raw data -----------------------------------------------------------

  cert_raw <- read_dta('data/raw/staff_certifications.dta')
  View(cert_raw)
 
  
# Standardize certification values ----------------------------------------

  count(cert_raw, certification_code)
  cert_raw$certification_code <- toupper(cert_raw$certification_code)
  cert_raw$certification_code <- recode(cert_raw$certification_code,
                                        'ESL'             = 'ENGLISH AS A SECOND LANGUAGE CERTIFICATION',
                                        'SPED'            = 'SPECIAL EDUCATION CERTIFICATION',
                                        'NB'              = 'NATIONAL BOARD CERTIFICATION',
                                        'NATL BOARD'      = 'NATIONAL BOARD CERTIFICATION',
                                        'SPECIAL ED'      = 'SPECIAL EDUCATION CERTIFICATION',
                                        'ELS'             = 'ENGLISH AS A SECOND LANGUAGE CERTIFICATION',
                                        'BOARD CERTIFIED' = 'NATIONAL BOARD CERTIFICATION')
  count(cert_raw, certification_code)

  
# Formate Certification Eff. & Exp. Dates ---------------------------------
  #	- School year will be defined by the year during the Spring
  #	- Certifications effective after May 1 will be valid during the following school year
  #	- Add one year to eff. dates between Apr. 1 and Dec. 31
  #	- Certifications that expire before Oct. 1 are only valid through the previous school year
  #	- Add one year to exp. dates between Oct 1 and Dec 31 to align to the valid school year
  #	- Drop records missing a valid effectie year and expiration year
  
  cert_raw$effective_date       <- mdy(cert_raw$effective_date)	
  cert_raw$effective_date_year  <- year(cert_raw$effective_date)
  cert_raw$effective_date_month <- month(cert_raw$effective_date)
  cert_raw$effective_date_year  <- ifelse(cert_raw$effective_date_month >= 5 & cert_raw$effective_date_month <= 12, 
                                          (cert_raw$effective_date_year + 1), cert_raw$effective_date_year)
  
  cert_raw$expiration_date       <- mdy(cert_raw$expiration_date)
  cert_raw$expiration_date_year  <- year(cert_raw$expiration_date)
  cert_raw$expiration_date_month <- month(cert_raw$expiration_date)
  cert_raw$expiration_date_year  <- ifelse(cert_raw$expiration_date_month >= 10 & cert_raw$expiration_date_month <= 12,
                                           (cert_raw$expiration_date_year + 1), cert_raw$expiration_date_year)
  
  # Drop records where both the effective year and expiration year are null
  cert_raw   <- cert_raw[!(is.na(cert_raw$effective_date_year) & is.na(cert_raw$expiration_date_year)),]
  
  # If the eff. year is missing have it match the exp. year and vice versa.
  cert_raw$effective_date_year  <- ifelse(is.na(cert_raw$effective_date_year), cert_raw$expiration_date_year, cert_raw$effective_date_year)
  cert_raw$expiration_date_year <- ifelse(is.na(cert_raw$expiration_date_year), cert_raw$effective_date_year, cert_raw$expiration_date_year)
  
  # Drop the month columns they are no longer necessary.
  # %<>% operator uses cert_raw as the first argument and then passes the result back to cert_raw.  -contains('month') selects all columns that do not have the word month in the name.
  cert_raw   %<>% select(-contains('month'))


  
# Determine and create valid school years for each teacher certificaion --------------
  
  # Create a column with the number of years the certification is valid.
  cert_raw$years <- cert_raw$expiration_date_year - cert_raw$effective_date_year + 1
  
  # Use rep() to repeate each row for the number of years the certification was valid and then
  #  use mutate to explicitly fill in the sequence of valid school years for each teacher, certificaiton combo.
  cert_all_years <- cert_raw[rep(1:nrow(cert_raw), cert_raw$years),] %>%
    group_by(tid, certification_code) %>%
    mutate(school_year = effective_date_year + row_number() - 1,
           certified = 1) %>%
    select(tid, school_year, certification_code, certified)
  
  # Reshape from long to wide format so each teacher has one row per year.
  cert_all_years <- spread(cert_all_years,
                           key = certification_code, # Column whose values will become column headings
                           value = certified) # Column whose alues will populate the cells
  
  # Rename and order needed columns.
  cert_all_years %<>% select(tid,
                             school_year,
                             certification_esl = `ENGLISH AS A SECOND LANGUAGE CERTIFICATION`,
                             certification_nbct = `NATIONAL BOARD CERTIFICATION`,
                             certificaiton_sped = `SPECIAL EDUCATION CERTIFICATION`)
  

  
  
  
  
  
  
  