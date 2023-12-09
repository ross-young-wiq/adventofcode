
# INTRO -------------------------------------------------------------------

# Title:  02 Rock Paper Scissors
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202202.txt"))



# DATA PREP ---------------------------------------------------------------

mat_result <- matrix(
  data = c(
    c( 0, +1, -1),
    c(-1,  0, +1),
    c(+1, -1,  0)
  ),
  byrow = TRUE,
  ncol = 3
)


# PART ONE ----------------------------------------------------------------

# start with puzzle input
puzzle %>% 
  
  # convert to data.table
  data.table(input = .) %>% 
  
  # split into separate columns
  .[, c("call", "response") := tstrsplit(x = input, " ")] %>% 
  
  # convert to score
  .[, call_score := match(call,     c("A", "B", "C"))] %>% 
  .[, resp_score := match(response, c("X", "Y", "Z"))] %>% 
  
  # get result
  .[, result := map2_dbl(.x = call_score, .y = resp_score, .f = ~ mat_result[.x, .y])] %>% 
  
  # get score
  .[, score := (3 * result + 3) + resp_score] %>% 
  
  # sum total score
  .[, sum(score)]



# PART TWO  ---------------------------------------------------------------


# start with puzzle input
puzzle %>% 
  
  # convert to data.table
  data.table(input = .) %>% 
  
  # split into separate columns
  .[, c("call", "response") := tstrsplit(x = input, " ")] %>% 
  
  # convert to score
  .[, call_score := match(call,     c("A", "B", "C"))] %>% 
  .[, resp_score := match(response, c("X", "Y", "Z")) - 2] %>% 
  
  # convert to required value
  .[, fix_response := map2_dbl(.x = call_score, .y = resp_score, .f = ~ match(.y, mat_result[.x,]))] %>% 
  
  # get score
  .[, score := (3 * resp_score + 3) + fix_response] %>% 
  
  # sum total score
  .[, sum(score)]
  
