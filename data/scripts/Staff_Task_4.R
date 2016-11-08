###################################################################################################
##  File name: Staff_Task4.R                                                                     ##
##  Author:    Nathan Trenholm                                                                   ##
##  Created:   11/8/2015                                                                         ##
##                                                                                               ##
##  Description: Create a file that contains one observation for each school year                ##
##               a teacher is in the data.  This will:                                           ##
##                                                                                               ##
##               1. Identify one unique school code per teacher within each school year.         ##
##               2. Resolve inconsistencies in years of teacher experience across school years.  ##
##               3. Assign one hire and terminiation date to each employment period.             ##
##                                                                                               ##
##  Inputs:    staff_degrees__job_codes_clean.rda                                                ##
##             school_clean.csv                                                                  ##
##  Outputs:   staff_school_year_clean.csv                                                       ##
###################################################################################################


# Load packages -------------------------------------------------------------------------------

  library(compare)
  library(haven)
  library(magrittr)
  library(stringr)
  library(lubridate)
  library(tidyverse)
  library(frequencies) # install_github('nathant23/Frequencies')


# Load data and prep for cleaning. ------------------------------------------------------------

  school_clean <- read_csv('data/clean/school_clean.csv')
  load('data/clean/staff_degrees_job_codes_clean.rda')
  
  # Join school and staff files.
  staff_school_year <- full_join(staff_degrees_job_codes_clean, school_clean,
                                 by = c('school_code' = 'school_code'))
  
  # Check table of job codes at possible non-school locations.  School codes 9 and 800 are possible non-school locations.
  table(staff_school_year$job_code[staff_school_year$school_code %in% c(9,800)])
  
  
# Clean up job codes and school assignments: Alternative and high schools. --------------------------------------------------

  # Create variable that shows how many unique values school_code assumes for each individual and school year.
  staff_school_year %<>% 
    group_by(tid, school_year) %>%
    mutate(nvals_school = n_distinct(school_code)) %>%
    ungroup()
  
  freq_table('staff_school_year', 'nvals_school')
  
  # If an individual has more than one school code within the same year and one of them is a   
  #   nontraditional school and the other is a traditional school, keep the traditional.				    
  staff_school_year %<>% filter(!(school_code %in% c(9,800) & nvals_school > 1 & t_is_teacher == 1))
  
  #  If an individual has more than one school code within the same year and one of them is a high school
  # 	and the other is either a middle or elementary school, keep the middle or elementary observation, since SDP
  # 	does not calculate teacher effects for high school teachers.
  staff_school_year %<>% 
    mutate(temp_ms_es = ifelse(elementary == 1 | middle == 1, 1,0)) %>%
    group_by(tid, school_year) %>%
    mutate(ms_es = max(temp_ms_es)) %>%
    ungroup() %>%
    filter(!(high == 1 & ms_es == 1))
    
  # If an individual has more than one school code within the same year and one of them is a nontraditional school
  #   and the other is a traditional school, keep the traditional school observation.
  
  staff_school_year %<>%
    group_by(tid, school_year) %>%
    mutate(non_alternative = min(alternative)) %>%
    ungroup() %>%
    filter(!(alternative == 1 & non_alternative == 1))
  
  staff_school_year %<>%
    group_by(tid, school_year) %>%
    mutate(nvals_school2 = n_distinct(school_code)) %>%
    ungroup()
  
  freq_table('staff_school_year', 'nvals_school2')
  
  # Remove unnecessary columns
  staff_school_year %<>% select(-nvals_school, -temp_ms_es, -ms_es, -non_alternative)
  
# Clean up job codes and school assigments: Across yearsr. -------------------------------------

  # For teachers with more than one school code per year, use the school code from the 
  # following (n+1) year if one of the school codes matches that school code. 
  school_assign_adjacent <- function(data_frame, prior_or_next){
    
    arrangement <- ifelse(prior_or_next == 'prior',
                          'desc(school_year)',
                          'school_year')
    data_frame %>%
      group_by(tid, school_year) %>%
      summarise(school_min = min(school_code),
                school_max = max(school_code)) %>%
      ungroup() %>%
      arrange_('tid', arrangement) %>%
      mutate(school_min_next = lead(school_min), # school_min from next record.
             school_max_next = lead(school_max), # school_max from next record.
             tid_next        = lead(tid)) %>%    # tid from next record.
      # Reduce to those with two schools in one year and one of them was where they were in the next record (prior or next school year).
      filter(school_min != school_max &
               tid == tid_next &
               school_min_next == school_max_next &
               school_min_next %in% c(school_min, school_max)) %>%
      select(tid, school_year, clean_school_code = school_min_next) %>%
      # Merge in results to original data frame and over write differences.
      right_join(data_frame, by = c('tid' = 'tid', 'school_year' = 'school_year')) %>%
      mutate(school_code = ifelse(is.na(clean_school_code), school_code, clean_school_code)) %>%
      group_by(tid, school_year) %>%
      mutate(nvals_school2 = n_distinct(school_code)) %>%
      ungroup() %>%
      select(-clean_school_code) 
  }
    
    
  staff_school_year <- school_assign_adjacent(staff_school_year, 'next')
  freq_table('staff_school_year', 'nvals_school2')
  
  staff_school_year <- school_assign_adjacent(staff_school_year, 'prior')
  freq_table('staff_school_year', 'nvals_school2')

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
