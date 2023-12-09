
# INTRO -------------------------------------------------------------------

# Title:  04 Camp Cleanup
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202204.txt"))



# DATA PREP ---------------------------------------------------------------

puzzle %>% 
  
  str_extract_all(pattern = "\\d+") %>% 
  
  map(.f = as.double) %>%
  
  force() -> lst_puzzle



# PART ONE ----------------------------------------------------------------

lst_puzzle %>% 
  
  map_lgl(.f = ~ (.x[1] <= .x[3] & .x[2] >= .x[4]) | (.x[3] <= .x[1] & .x[4] >= .[2])) %>% 
  
  sum()



# PART TWO  ---------------------------------------------------------------

lst_puzzle %>% 
  
  map_lgl(.f = ~ !((.x[3] > .x[2]) | (.x[1] > .x[4]))) %>% 
  
  sum()

