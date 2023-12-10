
# INTRO -------------------------------------------------------------------

# Title:  10 Pipe Maze
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202310.txt"))



# DATA PREP ---------------------------------------------------------------

# start with puzzle
puzzle %>% 
  
  # split into separate components
  str_split(pattern = "") %>% 
  
  # convert to vector
  unlist() %>% 
  
  # save vector as variable
  force() -> v_puz


# get number of rows and columns from the input set
len_row <- length(puzzle)
len_col <- str_length(puzzle[1])


# start with the puzzle vector
v_puz %>% 
  
  # convert to matrix
  matrix(data = ., nrow = len_row, byrow = TRUE) %>% 
  
  # save matrix as variable
  force() -> mat_puz


# create lookup table for directions
tribble(
  ~move  , ~x, ~y,
  "left" ,  0, -1,
  "right",  0, +1,
  "down" , +1,  0,
  "up"   , -1,  0
) %>% 
  
  # convert to data.table
  as.data.table() %>% 
  
  # save data.table as variable
  force() -> dt_lkup


# get starting coordinates of SNAKE
start_x <- which(v_puz == "S") %/% len_col + 1
start_y <- which(v_puz == "S") %%  len_col


# set parameters
i <- 0
m <- c()
movement <- "left"



# PART ONE ----------------------------------------------------------------

# run loop
repeat {
  
  start_x <- start_x + dt_lkup[move == movement, x]
  start_y <- start_y + dt_lkup[move == movement, y]
  
  xx <- mat_puz[start_x, start_y]
  
  movement <- if (xx %chin% c("|", "-")) {movement} else
        if (xx == "L" & movement == "down") {"right"} else
          if (xx == "L" & movement == "left") {"up"} else
            if (xx == "7" & movement == "right") {"down"} else
              if (xx == "7" & movement == "up") {"left"} else
                if (xx == "F" & movement == "up") {"right"} else
                  if (xx == "F" & movement == "left") {"down"} else
                    if (xx == "J" & movement == "right") {"up"} else
                      if (xx == "J" & movement == "down") {"left"} else
                        if (xx == ".") {"error"} else {"end"}
  
  m <- c(m, start_x, start_y)
  
  if (movement == "end") {break}
  
  i <- i + 1
  
}

# return result
(i + 1 ) / 2
