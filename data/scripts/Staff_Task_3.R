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
  staff_raw$degree <- ifelse(str_detect(staff_raw$degree, 'MA'),     'MASTERS DEGREE'  , staff_raw$degree)
  staff_raw$degree <- ifelse(str_detect(staff_raw$degree, 'PH'),     'DOCTORATE DEGREE', staff_raw$degree)
  freq_table('staff_raw', 'degree', sort_by_count = TRUE)
  
  # Convert degree names to numeric values
  staff_raw$degree_num <- recode(staff_raw$degree, `BACHELORS DEGREE` = 1L,
                                                   `MASTERS DEGREE` = 2L,
                                                   `DOCTORATE DEGREE` = 3L, .default = NA_integer_)
  
 
# Clean degree values (missing or inconsistent across years). ---------------------------------
  # Loop through each degree type starting at Doctorate down to Bachelors (3:1)
  #  This will be used to: 
  #   - Identify the span of years the degree covers.
  #   - Explicitly write in hte degree type within the span of years.
  #   - Write in the degree type to any missing values in proceeding years.
  
  for (clean_degree in 3:1) {
    
    staff_raw_columns <- colnames(staff_raw)
    
    # Find the min and max years for each degree type.
    degree_years <- select(staff_raw, tid, school_year, deg_num = degree_num) %>% # Renamed degree_num to be different from original data.
                      filter(deg_num == clean_degree) %>%
                      group_by(tid, deg_num) %>%
                      summarize(min_deg_year = min(school_year),
                                max_deg_year = max(school_year)) %>%
                      ungroup()
    
    # Merge degree_years back with the raw data.
    staff_raw %<>% left_join(degree_years, by = c('tid' = 'tid'))
    
    # Write over the degree_num if it is within the span of degree years.
    staff_raw$degree_num[!is.na(staff_raw$min_deg_year) &
                           staff_raw$school_year >= staff_raw$min_deg_year &
                           staff_raw$school_year <= staff_raw$max_deg_year] <- clean_degree
    
    ##  Write in the degree type to any missing values that are after the span of years.
    staff_raw$degree_num[!is.na(staff_raw$min_deg_year) & 
                           is.na(staff_raw$degree_num)  & 
                           staff_raw$max_deg_year <= staff_raw$school_year] <- clean_degree
    
    ## Reduce the raw data back down to the original columns.
    staff_raw <- staff_raw[staff_raw_columns]
    rm(degree_years, clean_degree, staff_raw_columns)
  }
  
 
# Standardize the job_code & job_code_desc ----------------------------------------------------

  View(freq_table('staff_raw', 'job_code_desc'))
  
  staff_raw$job_code_desc <- toupper(staff_raw$job_code_desc)
  staff_raw$job_code_desc <- ifelse(staff_raw$job_code_desc == 'AP', 'PRINCIPAL / ASSISTANT PRINCIPAL', staff_raw$job_code_desc)
  staff_raw$job_code_desc <- ifelse(str_detect(staff_raw$job_code_desc, 'COACH'), 'COACH'             , staff_raw$job_code_desc)
  staff_raw$job_code_desc <- ifelse(str_detect(staff_raw$job_code_desc, 'TEACH|TAECH|TCHR'), 'TEACHER', staff_raw$job_code_desc)
  
  # Check that each job_code_desc corresponds to only one job_code and vice versa
  freq_table2('staff_raw', 'job_code', 'job_code_desc') # Job code '3' represents 'CLASSROOM ASSISTANT' & 'SUBSTITUTE'
  
  # Assign a new job code to classroom assistants
  staff_raw$job_code <- ifelse(staff_raw$job_code_desc == 'CLASSROOM ASSISTANT', 9, staff_raw$job_code)
  staff_raw$job_code <- as.numeric(staff_raw$job_code)
  
  # Check range of job codes per person within school years.
  staff_raw <- staff_raw %>%
                 group_by(tid, school_year) %>%
                 mutate(nvals_job = n_distinct(job_code)) %>%
                 ungroup()
  
  freq_table('staff_raw', 'nvals_job')
  

# Clean job codes. ----------------------------------------------------------------------------
  
  # Remove duplicate job codes within school years by job priority:
  #  1. TEACHER (1)
  #  2. PRINCIPAL / ASSISTANT PRINCIPAL (2)
  #  3. COUNSELOR (5)
  #  4. SPECIAL EDUCAITON ASSISTANT (6)
  #  5. CLASSROOM ASSISTANT (9)
  #  6. SCHOOL STAFF (8)
  #  7. COACH (4)
  #  8. SUBSTITUTE (3)
  #  9. TEMP (7)
  
  ##  Create a column to designate if an employee was identified at all as a teacher during the year.
  ##  Create a column to designate if an employee was identified at all as a principal during the year and not a teacher.
  staff_raw %<>% group_by(tid, school_year) %>%
                   mutate(t_is_teacher = ifelse(1 %in% job_code, 1, 0),
                          principal    = ifelse(2 %in% job_code & t_is_teacher != 1, 1, 0)) %>%
                   ungroup()
  
  ## Write over all job codes to teacher or principal if they were identified as either during the year.
  staff_raw$job_code <- ifelse(staff_raw$t_is_teacher == 1, 1, staff_raw$job_code)
  staff_raw$job_code <- ifelse(staff_raw$principal == 1,    2, staff_raw$job_code)
  
  
  ## To match SDP output, this for loop goes through each job code in reverse order of priority
  ##      and if an employee has muliple job codes within the year it drops that job code.  
  ##      Then it recalculates the number of job codes within the year (nvals_job) and moves to the next job code.
  for (code in c(7,3,4,8,9,6)) {
    staff_raw <- staff_raw %>%
      group_by(tid, school_year) %>%
      filter(!(nvals_job > 1 & job_code == code)) %>%
      mutate(nvals_job = n_distinct(job_code)) %>%
      ungroup()
  }
  

# Format data and create final data frame. ----------------------------------------------------

  ## Convert the job codes to factors.
  staff_raw$job_code <- factor(staff_raw$job_code,
                               levels = c(1,2,3,4,5,6,7,8,9),
                               labels = c('Teacher', 'Principal / Assistant Principal', 'Substitute', 'Coach', 'Counselor', 
                                          'Special Education Assistant', 'Temp', 'School Staff', 'Classroom Assistant'))
  
  ## Convert the degrees to factors.
  staff_raw$degree <- factor(staff_raw$degree_num,
                             levels = c(1,2,3),
                             labels = c("Bachelor's Degree", "Master's Degree", "Doctorate Degree"))
  
  ## Order the data and take only unique rows.
  staff_degrees_job_codes_clean <- staff_raw %>%
                           select(tid, school_year, school_code, job_code, degree, 
                                  t_is_teacher, experience, hire_date, termination_date) %>%
                           arrange(tid, school_year) %>%
                           unique(.)
    
 
# Compare with clean SDP file. ----------------------------------------------------------------
  
  sdp_clean_degree_job <- read_dta('data/sdp_clean/Staff_Degrees_Job_Codes_Clean.dta')
  
  my_file <- staff_degrees_job_codes_clean
  my_file$degree <- as.numeric(my_file$degree)
  
  compare(my_file, sdp_clean_degree_job, allowAll = TRUE)

 

# Write to file. ------------------------------------------------------------------------------
  save(staff_degrees_job_codes_clean, file = 'data/clean/staff_degrees_job_codes_clean.rda')
  write.csv(staff_degrees_job_codes_clean, 'data/clean/staff_degrees_job_codes_clean.csv')
 