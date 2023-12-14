
# INTRO -------------------------------------------------------------------

# Title:  12 Hot Springs
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202312.txt"))



# DATA PREP ---------------------------------------------------------------

# start with puzzle input
puzzle %>% 
  
  # convert to data.table
  data.table(input = .) %>% 
  
  # split string into separate components
  .[, c("condition", "damage") := tstrsplit(input, " ")] %>% 
  
  # shorten condition (replace multiple . with a single .)
  .[, condition_short := condition %>% 
        str_replace_all(pattern = "\\.+", replacement = "\\.") %>% 
        str_remove_all(pattern = "\\.$") %>% 
        str_remove_all(pattern = "^\\.")
  ] %>% 
  
  # convert damage into a vector
  .[, damage := damage %>% 
        str_split(pattern = ",") %>% 
        map(.f = as.double)
  ] %>% 
  
  # save data.table as variable
  force() -> dt
 

# write function to create combinations for pattern-match
my_fun <- function(damage0, condition_short0) {
  
  # get condition length
  cond_len <- str_length(condition_short0)
  
  # get count and total of damaged items
  damage_ct  <- length(damage0)
  
  # get undamaged items
  undamage_ct <- cond_len - sum(damage0)
  min_gap     <- undamage_ct
  min_gap0    <- min_gap - (damage_ct - 1)
  
  # get combinations of spaces in between damaged parts
  c(
    list(0:min_gap0),
    rep(list(seq(min_gap)), damage_ct - 1),
    list(0:min_gap0)
  ) %>% 
    
    # cross join, convert to matrix and keep rows that meet the required size
    expand.grid() %>% 
    as.data.table() %>% 
    {.[rowSums(.) == undamage_ct, ]} %>%
    
    # combine gap vector with damage vector, and interleave elements
    apply(
      MARGIN = 1, 
      FUN = function(x) {
        c(
          "[\\?|\\.]{",
          rbind(
            x[seq(damage_ct)],
            paste0("}[#|//?]{", damage0, "}[\\?|\\.]{")
          ),
          x[damage_ct+1],
          "}"
        )
      }
    ) %>% 
    
    # collapse into a single string
    apply(MARGIN = 2, FUN = paste0, collapse = "") %>% 
    
    # check matching
    str_detect(string = condition_short0, pattern = .) %>% 
    
    # sum values that fit
    sum()
  
}



# PART ONE ----------------------------------------------------------------

# start with data.table
dt %>% 
  
  # create copy (to avoid overwrite)
  data.table::copy() %>% 
  
  # run function for each row to count matches
  .[, match_ct := pmap_dbl(.l = list(damage0 = damage, condition_short0 = condition_short), .f = my_fun)] %>%
  
  # sum total matches
  .[, sum(match_ct)]


# Note: this takes about 10 seconds to run --> need to optimise in order to run with part 2



# PART TWO ----------------------------------------------------------------

# start with data.table
dt %>% 
  
  # create copy (to avoid overwrite)
  data.table::copy() %>% 
  
  # duplicate damage and condition by 5
  .[, damage          := map(damage,          .f = ~ rep(.x, 5))] %>% 
  .[, condition_short := map(condition_short, .f = ~ rep(.x, 5) %>% paste0(collapse = "?"))] %>% 
  
  # save data.table as variable
  force() -> dt2

