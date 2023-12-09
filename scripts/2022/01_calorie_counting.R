
# INTRO -------------------------------------------------------------------

# Title:  01 Calorie Counting
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202201.txt"))



# DATA PREP ---------------------------------------------------------------

# start with puzzle input
puzzle %>% 
  
  # convert to data.table
  data.table(input = .) %>% 
  
  # add elf id
  .[, elf_id := cumsum(input == "") + 1] %>% 
  
  # remove missing rows
  .[input != ""] %>% 
  
  # convert input to numeric
  .[, input := as.double(input)] %>% 
  
  # sum calories per elf
  .[, lapply(.SD, sum), by = .(elf_id)] %>% 
  
  # sort from most calories to least
  .[order(-input)] %>% 
  
  # save data.table as variable
  force() -> dt
  


# PART ONE ----------------------------------------------------------------

# get calories from elf with most
dt[1, input]


# PART TWO  ---------------------------------------------------------------

# get calories from top 3 elves with most
dt[1:3, sum(input)]
