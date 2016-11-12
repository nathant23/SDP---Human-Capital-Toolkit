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
    filter(!(high == 1 & ms_es == 1 & nvals_school > 1))
    
  # If an individual has more than one school code within the same year and one of them is a nontraditional school
  #   and the other is a traditional school, keep the traditional school observation.
  
  staff_school_year %<>%
    group_by(tid, school_year) %>%
    mutate(non_alternative = min(alternative)) %>%
    ungroup() %>%
    filter(!(alternative == 1 & non_alternative == 0 & nvals_school > 1))
  
  staff_school_year %<>%
    group_by(tid, school_year) %>%
    mutate(nvals_school2 = n_distinct(school_code)) %>%
    ungroup()
  
  freq_table('staff_school_year', 'nvals_school2')
  
  # Remove unnecessary columns
  staff_school_year %<>% select(tid, school_year, school_code, job_code, degree, t_is_teacher,
                                experience, hire_date, termination_date, nvals_school2)
  
# Clean up job codes and school assigments: Across years. -------------------------------------


  # Create a function that for teachers with more than one school code per year, use the school code from the 
  # following (n+1) year  or prior (n-1) year if one of the school codes matches that school code. 
  school_assign_adjacent <- function(data_frame, prior_or_following){
    
    arrangement <- ifelse(prior_or_following == 'prior',
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
    
    

  staff_school_year <- school_assign_adjacent(staff_school_year, 'following')
  freq_table('staff_school_year', 'nvals_school2')
  
  staff_school_year <- school_assign_adjacent(staff_school_year, 'prior')
  freq_table('staff_school_year', 'nvals_school2')

  staff_school_year %<>% unique(.)

# Clean up job codes and school assignments: Random. ------------------------------------------
  
  # Now the only teachers assigned to more than one school are assigned to two schools that 
  #   do not repeat in the years following or preceding the year in which the teacher has 
  #   two schools. For these remaining cases, keep an observation at random.
  staff_school_year %<>% 
    group_by(tid, school_year) %>%
    mutate(keep = row_number()) %>% 
    filter(keep == 1) %>%
    select(-nvals_school2, -keep) %>%
    ungroup()
  
  # Check that the data file is unique by teacher and school year.
  nrow(staff_school_year) == nrow(unique(staff_school_year[,c("tid", "school_year")]))

    
# P. 54 - Drop variables that still need to be cleaned and save as a temp file ---------------
  
  clean_school <- select(staff_school_year, 
                         -experience, -hire_date, -termination_date)
  
  
# Resolve inconsistencies in years of teacher experience across years. ------------------------
  
  # Keep only observations where the individual is a teacher.
  staff_school_year %<>% filter(t_is_teacher == 1)
  
  # It is vital to have only one occurance of the first year of experience teachering.
  #  Force experience = 2 for all but the earliest instance of 1 for a given teacher.
  staff_school_year %<>% 
    group_by(tid) %>%
    mutate(min_novice_year = min(school_year),
           experience = ifelse(experience == 1 & school_year != min_novice_year, 2, experience)) %>%
    ungroup()
  
  # Write a function to flag every instance when a value of experience is less than the prior value 
  #   for a given teacher. Write over the flagged experience with the prior year's experience.
  drops <- function(data_frame){
    
    data_frame %<>%
      group_by(tid) %>%
      arrange(tid, school_year) %>%
      mutate(neg = ifelse(tid == lag(tid) & experience < lag(experience), 1, 0),
             neg = ifelse(is.na(neg), 0, neg),
             experience = ifelse(neg == 1, lag(experience), experience)) %>%
      ungroup() 
    
  }
  
  # Run the function until there are no flags left.
  staff_school_year <- drops(staff_school_year)
  while (sum(staff_school_year$neg, na.rm = TRUE) > 0) { 
    staff_school_year <- drops(staff_school_year) 
    }
  
  # Write a function to fix jumps in experience that are too large given the number of years that 
  #   have elapsed (for example, a teacher’s experience increases by two in one year), 
  #   and count all such instances in the whole data.
  jumps <- function(data_frame){
    
    data_frame %>%
      group_by(tid) %>%
      arrange(tid, school_year) %>%
      mutate(jump = ifelse(!is.na(experience) & !is.na(lag(experience)) &
                             school_year - lag(school_year) < experience - lag(experience),1,0),
             jump = ifelse(is.na(jump), 0, jump),
             experience = ifelse(jump == 1 & experience == lead(experience),experience - 1, experience),
             jump2 = lead(jump, default = 0),
             experience = ifelse(jump2 == 1, lead(experience) - 1, experience),
             jump = ifelse(!is.na(experience) & !is.na(lag(experience)) &
                             school_year - lag(school_year) < experience - lag(experience),1,0),
             jump = ifelse(is.na(jump), 0, jump)) %>%
      ungroup() %>% 
      select(-jump2)
  
  }
  
  # Run the function until there are no jump flags left.  
  staff_school_year %<>% jumps()
  while (sum(staff_school_year$jump) > 0) {
    staff_school_year %<>% jumps()
  } 
  
  # Replace years of experience in 2012 for missing observations, assuming that the teacher 
  #   gains one year of experience from the prior year.
  staff_school_year %<>%
    group_by(tid) %>%
    arrange(tid, school_year) %>%
    mutate(experience = ifelse(tid == lag(tid) & t_is_teacher == 1 & school_year == 2012 & is.na(experience),
                               lag(experience) + 1, experience)) %>%
    ungroup() 
  
  # Keep only the variables needed.
  staff_school_year %<>% select(tid, school_year, school_code, job_code, experience, t_is_teacher, hire_date, termination_date)
  
  
# Identify employment periods -----------------------------------------------------------------

  # This is from page 57 of 'SDP_Data_Building_Tasks_HC_12_18_2013.pdf'.
  #   I did not understand the purpose of this code, which made it difficult for me to think about
  #   how to replicate it in R.  When comparing dates for the hire and terminiation fields
  #   in my file at this point to the SDP clean file there were no differences where data existed.
  #   The differences were in null terminiation dates being populated in the SDP file, yet there 
  #   were still tens of thousands of null dates. Also where terminiation dates were populated,
  #   they were filled in with dates prior to the hire date.  
  #   Perhaps I'll revisit once I see how these files will be used later in the analysis.
  
   
# Merge the temporary file with cleaned school codes to current file. -------------------------
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
