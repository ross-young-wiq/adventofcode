
# INTRO -------------------------------------------------------------------

# Title:  04 Scratchcards
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202304.txt"))



# DATA PREP ---------------------------------------------------------------

# start with puzzle input
puzzle %>% 
  
  # convert to data.table
  data.table(input = .) %>% 
  
  # separate into three columns (separator is colon : or pipe |)
  .[, c("card", "winners", "nbrs") := tstrsplit(input, "[:|]")] %>% 
  
  # convert card to integer
  .[, card := as.integer(str_extract(string = card, pattern = "\\d+"))] %>% 
  
  # convert winners and nbrs into vector of integers
  .[, c("winners", "nbrs") := lapply(.SD, str_extract_all, "\\d+"), .SDcols = c("winners", "nbrs")] %>% 
  
  # add column with common numbers
  .[, common_nbrs := map2(.x = winners, .y = nbrs, .f = intersect)] %>% 
  
  # get count of common numbers
  .[, count_common := map_int(.x = common_nbrs, .f = length)] %>% 
  
  # save variable as data.table
  force() -> dt



# PART ONE ----------------------------------------------------------------

# start with data.table
dt %>% 
  
  # points are 2 to the power of the count of common numbers less 1 (except for 0)
  .[, points := fifelse(count_common == 0, 0, 2^(count_common - 1))] %>% 
  
  # sum points
  .[, sum(points, na.rm = TRUE)]



# PART TWO  ---------------------------------------------------------------

# start with data.table
dt %>% 
  
  # retain required columns
  .[, .(card, count_common)] %>% 
  
  # save variable as data.table
  force() -> dt_short


# try a brute force approach with a for loop

# get the number of cards to start with
x <- rep(1, times = nrow(dt_short))

# loop for every card id
for (i in 1:nrow(dt_short)) {
  
  # create a new vector (y) to hold the bonus cards won
  y <- c(
    rep(0, times = i),                             # repeat 0 up to and including the loop_id
    rep(x[i], times = dt_short[i, count_common]),  # repeat the number of cards for the loop_id, by the number of count_common
    rep(0, times = nrow(dt_short))                 # repeat 0 to overfill the rest of the vector
  )
  
  # abbreviate the vector to be the right length
  y <- y[1:nrow(dt_short)]
  
  # add holding vector (y) to initial vector (x)
  x <- x + y
  
}

# return sum of cards at end of loop
sum(x)

