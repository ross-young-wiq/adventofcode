
# INTRO -------------------------------------------------------------------

# Title:  03 Mull It Over
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202403.txt"))



# DATA PREP ---------------------------------------------------------------

# combine into a single string
x <- paste0(puzzle, collapse = "")



# PART ONE ----------------------------------------------------------------

# start with input string
x %>% 
  
  # extract text that matches the pattern (e.g. "mul(123,345)")
  str_extract_all(pattern = "mul\\(\\d{1,3},\\d{1,3}\\)", simplify = TRUE) %>% 
  
  # extract numerical digits within each pattern match
  str_extract_all(pattern = "\\d+") %>% 
  
  # convert text to numeric and multiply
  map_dbl(.f = ~ .x %>% as.numeric() %>% prod()) %>% 
  
  # add multiplication results together
  sum()
  


# PART TWO  ---------------------------------------------------------------

# enable the program with "do()" at the start of the string
paste0("do()", x) %>% 
  
  # split string into separate pieces based on "don't" as a delimiter
  str_split(pattern = "don\\'t\\(\\)", simplify = TRUE) %>% 
  
  # within each piece, only keep from "do()" until end of piece
  str_extract(pattern = "do\\(\\)(.*)$") %>% 
  
  # keep pieces that have at least one "do()" entry
  keep(.p = ~ !is.na(.x)) %>% 
  
  # combine remaining pieces together
  paste0(collapse = "") %>% 
  
  # repeat actions from part 1
  str_extract_all(pattern = "mul\\(\\d{1,3},\\d{1,3}\\)", simplify = TRUE) %>% 
  str_extract_all(pattern = "\\d+") %>% 
  map_dbl(.f = ~ .x %>% as.numeric() %>% prod()) %>% 
  sum()
