
# INTRO -------------------------------------------------------------------

# Title:  07 Camel Cards
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202307.txt"))



# DATA PREP ---------------------------------------------------------------

# order of card value (from lowest to highest)
v_cards   <- c(2:9, "T", "J", "Q", "K", "A") # part 1
v_cards_j <- c("J", 2:9, "T", "Q", "K", "A") # part 2

# order of hand value (from lowest to highest)
v_hands   <- c("11111", "1112", "122", "113", "23", "14", "5")

# create matrix of how much a jack improves a hand power
# x corresponds with the number of jacks
# y corresponds to the hand (see v_hands)
# e.g. 2 jacks improves "1112" to "113", which is two spots higher. hence mat_jacks[2, 2] = +2
mat_jacks <- matrix(
  data = c(
    c(1, 2, 2, 2, 0, 1, 0),
    c(0, 2, 3, 0, 2, 0, 0),
    c(0, 0, 0, 2, 2, 0, 0),
    c(0, 0, 0, 0, 0, 1, 0),
    c(0, 0, 0, 0, 0, 0, 0)
  ),
  byrow = TRUE,
  ncol = 7
)


# start with puzzle input
puzzle %>% 
  
  # convert to data.table
  data.table(input = .) %>% 
  
  # split into separate columns
  .[, c("hand", "bid") := tstrsplit(x = input, " ")] %>% 
  
  # convert bid to numeric
  .[, bid := as.double(bid)] %>% 
  
  # count number of occurrences of each card in the hard
  .[, count_cards := map(.x = hand, .f = ~ str_count(string = .x, pattern = v_cards))] %>% 
  
  # convert count into a shorthand (to match against v_hands)
  .[, count_cards_short := map(.x = count_cards, .f = ~ sort(.x[.x > 0]) %>% paste0(collapse = ""))] %>% 
  
  # convert shorthand into hand value
  .[, hand_power := match(count_cards_short, v_hands)] %>% 
  
  # create rank for hand power
  .[, hand_power_rank := rank(hand_power)] %>%
  
  # count jacks
  .[, count_jacks := str_count(string = hand, pattern = "J")] %>%
  
  # determine jack power (how much the hand is improved)
  .[, jack_power := map2_dbl(.x = count_jacks, .y = hand_power, .f = ~ if(.x == 0) {0} else {mat_jacks[.x, .y]})] %>% 
  
  # find hand jack power
  .[, hand_jack_power := hand_power + jack_power] %>% 
  
  # create rank for hand power
  .[, hand_jack_power_rank := rank(hand_jack_power)] %>%
  
  # convert cards into power, then convert to an alphabetic string (which can be sorted one digit at a time)
  # this is what we need to determine which card value is better sequentially
  .[, card_power := 
      map_chr(
        .x = hand, 
        .f = ~ str_split(string = .x, pattern = "") %>%
          map(.f = match, v_cards) %>% 
          map(.f = ~ LETTERS[.x]) %>% 
          map_chr(paste0, collapse = "")
        
      )] %>% 
  
  # create rank for card power
  .[, card_power_rank := rank(card_power)] %>% 
  
  # convert cards into power again, but using v_cards_j
  .[, card_jack_power := 
      map_chr(
        .x = hand, 
        .f = ~ str_split(string = .x, pattern = "") %>%
          map(.f = match, v_cards_j) %>% 
          map(.f = ~ LETTERS[.x]) %>% 
          map_chr(paste0, collapse = "")
        
      )] %>% 
  
  # create rank for hand jack power
  .[, card_jack_power_rank := rank(card_jack_power)] %>% 
  
  # add row_id after sorting by (1) hand_power_rank, and (2) card_power_rank
  .[order(hand_power_rank, card_power_rank), row_id := .I] %>%
    
  # add row_id after sorting by (1) hand_jack_power_rank, and (2) card_hand_power_rank
  .[order(hand_jack_power_rank, card_jack_power_rank), row_id_j := .I] %>% 
  
  # save data.table as variable
  force() -> dt
  


# PART ONE ----------------------------------------------------------------

# sum winnings by standard hand power
dt[, sum(row_id * bid)]



# PART TWO  ---------------------------------------------------------------

# sum winnings by jack power
dt[, sum(row_id_j * bid)]
