
# INTRO -------------------------------------------------------------------

# Title:  13 Point of Incidence
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202313.txt"))



# DATA PREP ---------------------------------------------------------------

# start with puzzle input
puzzle %>% 
  
  # break into separate mirror pieces based on blank row ("") separator
  split(x = ., f = 1 + cumsum(. == "")) %>% 
  
  # for each mirror piece, remove the blank row ("")
  map(.f = function(x) {discard(.x = x, .p = function(y) {y == ""})}) %>% 
  
  # save list as variable
  force() -> lst_puz
  

# write function to tranpose mirror (turn columns into rows) so that we can check for dupes in the same way
my_fun <- function(x) {
  
  # split into separate pieces, convert to matrix and transpose
  str_split(string = x, pattern = "", simplify = TRUE) %>% t() %>% 
    
    # concatenate elements by row back into a string
    apply(MARGIN = 1, FUN = paste0, collapse = "")
  
}



# PART ONE ----------------------------------------------------------------

# start with puzzle list (containing every mirror piece)
map_dbl(
  .x = lst_puz,
  .f = function(LST) {
    
    # loop once for the mirror piece, and once for the transposed mirror piece
    imap_dbl(
      .x = list(LST, my_fun(LST)),
      .f = function(MIRROR, MIRROR_ID) {
        
        # start with mirror piece
        MIRROR %>% 
          
          # run-length-encoding to find where there is a duplicated value
          rleid() %>% 
          
          # convert duplicate values into TRUE/FALSE
          duplicated() %>% 
          
          # return elements where the duplication starts
          {which(.) - 1} %>% 
          
          # save output as variable
          force() -> xx
        
        # get multiplier based on row or column match
        yy <- if (MIRROR_ID == 1) {100} else {1}
        
        # if no matches are found then return 0
        if (length(xx) == 0) {NA_real_} else {
          
          # loop over each possible match
          map_dbl(
            .x = xx,
            .f = function(x) {
              
              {
                # if match occurs before the midpoint, start from left, otherwise right
                if (x < length(MIRROR) / 2) {
                  all(MIRROR[seq(x)] == MIRROR[seq(2 * x, x + 1)])
                } else {
                  all(MIRROR[tail(seq(x), length(MIRROR) - x)] == MIRROR[seq(length(MIRROR), x + 1)])
                }
                
              } %>% 
                
                # if match fulfils condition, then return the match value, otherwise NULL
                {if (. == TRUE) {yy * x} else {NA_real_}}
            }
          ) %>% 
            
            # reduce duplicates
            {if (all(is.na(.))) {NA_real_} else {max(., na.rm = TRUE)}}
          
        }
      }
    ) %>% 
      
      # remove NA
      discard(.p = ~ is.na(.x))
  }
) %>% 
  
  # save as output
  force() -> outpu

# add values
sum(output)



# PART TWO ----------------------------------------------------------------


