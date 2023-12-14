
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
  
  # break into separate mirror pieces based on blank row ("")
  split(x = ., f = 1 + cumsum(. == "")) %>% 
  
  # for each mirror, remove the blank row ("")
  map(.f = function(x) {discard(.x = x, .p = function(y) {y == ""})}) %>% 
  
  # save list as variable
  force() -> lst_puz
  

# start with list
lst_puz %>% 
  
  # modify list to turn columns into rows, so that we can run the same check function.
  purrr::modify(
    .f = ~ .x %>% 
      # split into separate pieces, convert to matrix and transpose
      str_split(pattern = "", simplify = TRUE) %>% t() %>% 
      # concatenate elements by row back into a string
      apply(MARGIN = 1, FUN = function(x) {paste0(x, collapse = "")})
  ) %>% 
  
  # save transposed list as variable
  force() -> lst_puz_t
  


# PART ONE ----------------------------------------------------------------

# run loop for each list
list(
  lst_puz,
  lst_puz_t
) %>% 
  
  # run loop over each mirror within list
  map(
    .f = function(LST) {
      
      # start with list element
      LST %>% 
        
        # run-length-encoding to find where there is a duplicated value
        map(.f = rleid) %>% 
        # convert duplicate values into TRUE/FALSE
        map(.f = duplicated) %>% 
        # return elements where the duplication starts
        map(.f = ~ which(.x) - 1) %>% 
        # only keep elements that have at least one possible match
        keep(.p = ~ length(.x) > 0) %>% 
        
        # for each mirror containing a match, run a loop
        imap(
          .f = function(x, y) {
            
            # extract mirror to examine
            xm <- LST[[y]]
            
            # run loop over each possible match
            map(
              .x = x,
              .f = function(z) {
                
                # if match occurs before the midpoint start from left, otherwise right
                {
                  if (z < length(xm) / 2) {
                    all(xm[seq(z)] == xm[seq(2 * z, z + 1)])
                  } else {
                    all(xm[tail(seq(z), length(xm) - z)] == xm[seq(length(xm), z + 1)])
                  }
                } %>% 
                  
                  # if match fulfils condition, then return the match value, otherwise NULL
                  {if (.) {z} else {NULL}}
                
              }
            ) %>% 
              
              # add mirror ID back onto the output
              purrr::set_names(nm = y)
            
          }
        ) %>%
        
        # get all list elements onto the same level
        flatten() %>% 
        
        # remove any list elements containing NULL
        compact() %>% 
        
        # convert from list to vector
        unlist() %>%
        
        # add values
        sum()
      
    }
  ) %>% 
  
  # convert into a vector with two numbers (horizontal matches and vertical matches)
  # multiply horizontal matches by 100
  {flatten_dbl(.) * c(100, 1)} %>% 
  
  # add values
  sum()



# PART TWO ----------------------------------------------------------------


