###################################################################################################
##  File name: Student_Task1.R                                                                   ##
##  Author:    Nathan Trenholm                                                                   ##
##  Created:   12/13/2015                                                                        ##
##                                                                                               ##
##  Description: This script will take the 'Student_Demographics_Raw' file and generate the      ##
##               'Student_Attribute_Clean' file.                                                 ##
##                                                                                               ##
##               1. Create consistent gender indicators for students across years.               ##
##               2. Create consistent race/ethnicity values for students across years.           ##
##                                                                                               ##
##  Inputs:    Student_Demographics_Raw                                                          ##
##                                                                                               ##
##  Outputs:   student_attributes_clean.csv                                                      ##
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

  stu_demo <- read_dta('data/raw/Student_Demographics_Raw.dta') %>% as_tibble()
  
  
# Step 1. create one consistent value for gender for each student across years. ---------------

  stu_demo$male <- ifelse(stu_demo$gender == 'Male', 1, 0)
  
  stu_demo %<>% group_by(sid) %>% 
               mutate(nvals_male = n_distinct(male)) %>% 
               ungroup() 
  
  table(stu_demo$nvals_male)
  # No student was assigned more than one gender.             
  

  