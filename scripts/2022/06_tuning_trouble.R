
# INTRO -------------------------------------------------------------------

# Title:  06 Tuning Trouble
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)
library(slider)

# load data
puzzle <- read_lines(file = paste0("data/202206.txt"))



# DATA PREP ---------------------------------------------------------------

# split puzzle input into separate digits and convert to vector
v_puz <- str_split(string = puzzle, pattern = "") %>% unlist()



# PART ONE ----------------------------------------------------------------

# start with puzzle vector
v_puz %>% 
  
  # create slide window (with current record and 3 before), to check all four values unique.
  slider::slide_lgl(.f = ~ length(unique(.x)) == 4, .before = 3) %>% 
  
  # extract position of TRUE values in vector
  which() %>% 
  
  # get position of first true value
  head(1)



# PART TWO ----------------------------------------------------------------

# start with puzzle vector
v_puz %>% 
  
  # create slide window (with current record and 3 before), to check all four values unique.
  slider::slide_lgl(.f = ~ length(unique(.x)) == 14, .before = 13) %>% 
  
  # extract position of TRUE values in vector
  which() %>% 
  
  # get position of first true value
  head(1)

