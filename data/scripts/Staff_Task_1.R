##########################################################################
##  File name: Staff_Task_1.R                                           ##
##  Author:    Nathan Trenholm                                          ##
##  Created:   10/24/2016                                               ##
##  Updates:                                                            ##
##                                                                      ##
##  Description: Generates the staff_attributes_clean file which has:   ##
##    1. Resolved instances when teachers appear with                   ##			   
##       inconsistent attributes over available years.                  ##
##    2. One record per teacher.                                        ##
##                                                                      ##
##  Inputs:   data/raw/Staff_School_Year_Raw.dta                        ##
##  Outputs:  data/clean/staff_attributes_clean.csv                     ##
##########################################################################



# Set working directory and load packages ---------------------------------

  setwd("C:/Users/Nathan/Desktop/SDP - Human Capital Toolkit")

  library(compare)
  library(haven)
  library(tidyverse)
  

# Mode Function -----------------------------------------------------------

  ## From: https://gist.github.com/jmarhee/8530768 
  #  If there are multiple modes it returns the first instance.
  #  By arragning years in descending order, when there are multiple modes the most recent will be returned.

  Mode <- function(x) {
    ux <- unique(x)
    ux <- ux[!is.na(ux)]
    ux[which.max(tabulate(match(x, ux)))]
  }


# Read raw data file  & keep only needed fields ---------------------------

  staff_raw <- read_dta('data/raw/Staff_School_Year_Raw.dta')
  staff_raw <- unique(staff_raw[,c('tid', 'school_year', 'male', 'race_ethnicity', 'certification_pathway', 'birth_date')])
  

  
# Standardize Certification Values ----------------------------------------

  count(staff_raw, certification_pathway)
  staff_raw$certification_pathway <- toupper(staff_raw$certification_pathway)
  ## Convert certifications to numbers and then set to factors with labels.
  staff_raw$certification_pathway <- recode(staff_raw$certification_pathway,
                                            'STD CERT'                  = 1,
                                            'STANDARD CERTIFICATION'    = 1,
                                            'ALTERNATIVE CERTIFICATION' = 2,
                                            'ALTCERT'                   = 2,
                                            'ALT'  = 2,
                                            'TAF'  = 3,
                                            'TFA'  = 3)
  
  staff_raw$certification_pathway <- factor(staff_raw$certification_pathway,
                                            levels = c(1,2,3),
                                            labels = c("Standard Certification", "Alternative Certification", "TFA"))
  
  count(staff_raw, certification_pathway)
  


# Resolve duplicated 'male' and 'certification_pathway' by teacher --------
  
  ##  Check for multiple gender or certification values per teacher.If the max returns more than 1 then there are teachers with more than one value.
  staff_raw %>%
    group_by(tid) %>%
    mutate(nvals_male = n_distinct(male),
           nvals_cert = n_distinct(certification_pathway)) %>%
    ungroup() %>%
    summarize(max(nvals_male), max(nvals_cert)) 
  
  ## Set the teacher's gender and certification pathway to their modal value (most recent in bimodal cases).
  staff_raw <- staff_raw %>%
    group_by(tid) %>%
    arrange(desc(school_year)) %>%
    mutate(male 	= Mode(male),
           certification_pathway 	= Mode(certification_pathway)) %>%
    ungroup() 


# Standardize Ethnicity Values --------------------------------------------

  ## Convert Ethnicities to numeric values to change to factors later:
  # 1 = 'Black'	
  # 2 = 'Asian'	
  # 3 = 'Latino'
  # 4 = 'Native American'
  # 5 = 'White'
  # 6 = 'Multiple/Other'

  count(staff_raw, race_ethnicity)
  staff_raw$race_ethnicity[staff_raw$race_ethnicity == ''] <- NA
  staff_raw$race_ethnicity <- toupper(staff_raw$race_ethnicity)
  staff_raw$race_ethnicity <- recode(staff_raw$race_ethnicity,
                                     'WHITE'            = 5,
                                     'HISPANIC'         = 3,
                                     'HISP'             = 3,
                                     'ASIAN'            = 2,
                                     'NATIVE AMERICAN'  = 4,
                                     'MULTIPLE / OTHER' = 6,
                                     'AFRICAN AMERICAN' = 1,
                                     'AFAM'             = 1,
                                     'BLACK'            = 1)
  

# Prioritize Latino and Multiracial within years. -------------------------

  # Set temporary value to transform any instance of Latino in a year as Latino throughout the year.
  staff_raw$temp_islatino <- ifelse(staff_raw$race_ethnicity == 'Latino', 1, 0)
  
  # Prioritize Latino then multi-racial within years for teachers.
  staff_raw <- staff_raw %>%
    group_by(tid, school_year) %>%
    mutate(t_count         = n(),
           isLatino        = max(temp_islatino, na.rm = TRUE),
           race_ethnicity  = ifelse(!is.na(isLatino) && isLatino == 1, 3, race_ethnicity), # 3 is the numeric value representing Latino
           nvals_race_year = n_distinct(race_ethnicity),
           race_ethnicity  = ifelse(nvals_race_year > 1, 6, race_ethnicity)) %>% # 6 is the numeric value representing Multiple/Other
    ungroup()
  
  # Convert race_ethnicity to factor to match the SDP output.
  staff_raw$race_ethnicity <- factor(staff_raw$race_ethnicity,
                                     levels = c(1, 2, 3, 4, 5, 6),
                                     labels = c("Black", 
                                                "Asian", 
                                                "Latino",
                                                "Native American",
                                                "White", 
                                                "Multiple/Other"))

    
# Resolve duplicated race_ethnicity across years. -------------------------
  #	- Instances of multiple values the modal value is chosen.
  #	- When there are multiple modal values the most recent is chosen.
  #	- Most recent is returned by the Mode function by arranging the data set in descending order of school year.
  staff_raw <- staff_raw %>%
    group_by(tid) %>%
    arrange(desc(school_year)) %>%
    mutate(race_ethnicity = Mode(race_ethnicity)) %>%
    ungroup()

  
# Check/resolve duplicate or missing dates of birth. ----------------------------

  # Below returned no multiple birthdates for individual teachers.
  staff_raw %>%
         group_by(tid) %>%
         mutate(nvals_dob = n_distinct(birth_date)) %>%
         filter(nvals_dob > 1)
  
  # Below returned no missing values for birth dates.
  sum(is.na(staff_raw$birth_date))
  
  staff_raw$birth_date <- as.Date(staff_raw$birth_date, '%m/%d/%Y')
  

# Select needed columns and write to file. --------------------------------

  staff_clean 	<- unique(staff_raw[,c('tid', 'male', 'race_ethnicity', 'certification_pathway', 'birth_date')])
  rm(staff_raw)
  write.csv(staff_clean, file = 'data/clean/staff_attributes_clean.csv')
  

# Read in and compare to SDP clean file. ----------------------------------

  sdp_staff_clean  <- read_dta('data/sdp_clean/Staff_Attributes_Clean.dta')
  
  compare(staff_clean, sdp_staff_clean)
  compare(staff_clean, sdp_staff_clean, allowAll = TRUE)
