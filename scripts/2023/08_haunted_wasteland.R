
# INTRO -------------------------------------------------------------------

# Title:  08 Haunted Wasteland
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202308.txt"))

# set options to remove scientific notation (given part 2 returns a large number)
options(scipen = 999)


# DATA PREP ---------------------------------------------------------------

# start with puzzle input
puzzle %>% 
  
  # take first line
  head(1) %>% 
  
  # split string into LR instructions
  str_split(pattern = "") %>% 
  
  # convert from list to vector
  unlist() %>% 
  
  # save vector as variable
  force() -> v_RL


# start with puzzle input
puzzle %>% 
  
  # remove first two rows (already handled LR instructions)
  tail(-2) %>% 
  
  # extract word components from each line (also include numbers for the test puzzles)
  str_extract_all(pattern = "([A-Z0-9]+)", simplify = TRUE) %>% 
  
  # save matrix as variable
  force() -> mat_RL


# create function to get new start value
my_fun <- function(i0, v_start0, mat = mat_RL, v = v_RL) {
  
  # need to use modulo of i so that instructions repeat
  # note: subtract 1 and then add 1 trick enables sequence to run 1:263 without any zeroes.
  j <- ((i0 - 1) %% length(v)) + 1
  
  # get matrix coordinates of next location based on current location
  x0 <- match(v_start0, mat[,1])
  y0 <- (v[j] == "R") + 2
  
  # return location string
  return(mat[x0, y0])
  
}



# PART ONE ----------------------------------------------------------------

# set parameters
v_start <- "AAA"
v_end   <- "ZZZ"
i       <- 1

# run loop until result is found
repeat {
  
  v_start <- my_fun(i0 = i, v_start0 = v_start)
  if (v_start == v_end) {break}
  i <- i + 1

}

# return count of steps
print(i)



# PART TWO  ---------------------------------------------------------------

# get all starting nodes where string ends in "A"
v_nodes <- mat_RL[str_detect(string = mat_RL[,1], pattern = "..A"), 1]

# run loop across all starting nodes
map_dbl(
  .x = v_nodes,
  .f = function(x) {
    
    # set parameters
    v_start <- x
    i       <- 1
    
    # run loop until result is found
    repeat {
      
      v_start <- my_fun(i0 = i, v_start0 = v_start)
      if (str_detect(string = v_start, pattern = "..Z") == TRUE) {break}
      i <- i + 1
      
    }
    
    return(i)
    
  }
) %>% 
  
  # NOTE: an interesting observation --> the step count pattern is cyclical. 
  #       it will take the same number of steps to make the journey from origin to destination
  #       e.g. it takes 17621 steps to get from AAA to ZZZ, and then 17621 steps to do it again
  #       this condition holds for every starting node.
  
  # hence, we just need to get the LOWEST COMMON MULTIPLE across the step count from every starting node
  reduce(.f = numbers::LCM)

