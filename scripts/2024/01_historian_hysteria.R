
# INTRO -------------------------------------------------------------------

# Title:  01 Historian Hysteria
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202401.txt"))



# DATA PREP ---------------------------------------------------------------

# split input into two pieces
x <- str_split(string = puzzle, pattern = "   ", n = 2, simplify = TRUE)

# create a frequency table with values in second column
y <- table(x[, 2])



# PART ONE ----------------------------------------------------------------

# sort each column independently, take sum of absolute difference
sum(abs(sort(as.numeric(x[, 1])) - sort(as.numeric(x[, 2]))))



# PART TWO  ---------------------------------------------------------------

# multiply value in first column by frequency in second column, then take sum
sum(map_dbl(.x = x[, 1], .f = ~ coalesce(as.numeric(.x) * y[.x], 0)))

