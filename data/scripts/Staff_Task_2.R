#############################################################################
##  File name: Staff_Task_2.R                                              ##
##  Author:    Nathan Trenholm                                             ##
##  Created:   10/24/2016                                                  ##
##  Updates:                                                               ##
##                                                                         ##
##  Description: Generates the staff_certifications_clean file which has:  ##
##		1. Identify teachers with special certifications                     ##
##		2. Determine year teachers became certified and                      ##
##			the year certifications expired                                    ##
##		2. One record per teacher per year                                   ##
##                                                                         ##
##  Inputs:  data/raw/staff_certifications.dta                             ##
##  Outputs: data/clean/staff_certifications_clean.csv                     ##
#############################################################################


# Set working directory and load packages ---------------------------------

  setwd("C:/Users/Nathan/Desktop/SDP - Human Capital Toolkit")

  library(foreign)
  library(plyr)
  library(dplyr)
  library(lubridate)
  library(reshape)
  library(compare)


# Load raw data -----------------------------------------------------------

  cert_raw <- read.dta('data/raw/staff_certifications.dta')
  View(cert_raw)
 
  
  
  
  
# Standardize certification values ----------------------------------------

  unique(cert_raw$certification_code)
  cert_raw$certification_code <- toupper(cert_raw$certification_code)
  cert_raw$certification_code <- revalue(cert_raw$certification_code,
                                         c('ESL' 			        = 'ENGLISH AS A SECOND LANGUAGE CERTIFICATION',
                                           'SPED' 		        = 'SPECIAL EDUCATION CERTIFICATION',
                                           'NB' 			        = 'NATIONAL BOARD CERTIFICATION',
                                           'NATL BOARD'       = 'NATIONAL BOARD CERTIFICATION',
                                           'SPECIAL ED'       = 'SPECIAL EDUCATION CERTIFICATION',
                                           'ELS' 			        = 'ENGLISH AS A SECOND LANGUAGE CERTIFICATION',
                                           'BOARD CERTIFIED' 	= 'NATIONAL BOARD CERTIFICATION'))
  
  table(cert_raw$certification_code)
  
  
  
# Formate Certification Eff. & Exp. Dates ---------------------------------
  #	- School year will be defined by the year during the Spring                                   ##
  #	- Certifications effective after May 1 will be valid during the following school year         ##
  #	- Add one year to eff. dates between Apr. 1 and Dec. 31                                       ##
  #	- Certifications that expire before Oct. 1 are only valid through the previous school year    ## 
  #	- Add one year to exp. dates between Oct 1 and Dec 31 to align to the valid school year       ##
  #	- Drop records missing a valid effectie year and expiration year
  
  cert_raw$effective_date       <- mdy(cert_raw$effective_date)	
  cert_raw$effective_date_year  <- year(cert_raw$effective_date)
  cert_raw$effective_date_month <- month(cert_raw$effective_date)
  cert_raw$effective_date_year  <- ifelse(cert_raw$effective_date_month >= 5 & cert_raw$effective_date_month <= 12, 
                                          (cert_raw$effective_date_year + 1), cert_raw$effective_date_year)
  
  cert_raw$expiration_date       <- mdy(cert_raw$expiration_date)
  cert_raw$expiration_date_year  <- year(cert_raw$expiration_date)
  cert_raw$expiration_date_month <- month(cert_raw$expiration_date)
  cert_raw$expiration_date_year  <- ifelse(cert_raw$expiration_date_month >= 10 & cert_raw$expiration_date_mont <= 12,
                                           (cert_raw$expiration_date_year + 1), cert_raw$expiration_date_year)
  
  # Drop records where both the effective year and expiration year are null
  cert_raw   <- cert_raw[!(is.na(cert_raw$effective_date_year) & is.na(cert_raw$expiration_date_year)),]
  
  # If the eff. year is missing have it match the exp. year and vice versa.
  cert_raw$effective_date_year  <- ifelse(is.na(cert_raw$effective_date_year), cert_raw$expiration_date_year, cert_raw$effective_date_year)
  cert_raw$expiration_date_year <- ifelse(is.na(cert_raw$expiration_date_year), cert_raw$effective_date_year, cert_raw$expiration_date_year)
  
  # Drop the month columns they are no longer necessary.
  # grep('month', names(cert_raw)) returns the column numbers with the word month, putting the negative sign in front excludes those columns.
  cert_raw   <- cert_raw[,-grep('month', names(cert_raw))]


  
# Determine and create valid school years for each teacher certificaion --------------

  # Create empty vectors to hold data while looping through the records.
  tid         <- vector('numeric')
  cert_code   <- vector('character')
  school_year <- vector('numeric')
  certified   <- vector('numeric')
  
  # Function to apply to each row of the cert_raw data frame in order to create every valid year per certification.
  certYear <- function(x) {
    # x[[5]]:x[[6]] is the range of years from effective_date_year to expiration_date_year.
    for (year in x[[5]]:x[[6]]) {
      tid         <<- c(tid, as.numeric(x[[1]]))
      cert_code   <<- c(cert_code, x[[2]])
      school_year <<- c(school_year, year)
      certified   <<- c(certified, 1)
    }		
  }
  
  # Loop through each record then combine created vectors into new data.frame and remove duplicates.
  output         <- apply(cert_raw, 1, certYear)
  cert_all_years <- data.frame(tid, school_year, cert_code, certified)
  cert_all_years <- unique(cert_all_years)
  
  # Reshape from long to wide format so each teacher has one row per year
  cert_all_years 		<- reshape(cert_all_years, 
                              idvar = c('tid', 'school_year'), 
                              timevar = 'cert_code', 
                              direction = 'wide')
  # Rename and order needed columns
  cert_all_years %<>% select(tid,
                             school_year,
                             certification_esl = `certified.ENGLISH AS A SECOND LANGUAGE CERTIFICATION`,
                             certification_nbct = `certified.NATIONAL BOARD CERTIFICATION`,
                             certificaiton_sped = `certified.SPECIAL EDUCATION CERTIFICATION`)
  
  
  
  
  
  
  