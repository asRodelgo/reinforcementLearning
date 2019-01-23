# Reinforcment Learning Tute cards game
#
## set up
library(tidyverse)
source("tute_helper_functions.R") # load helper_functions
cards_df <- define_cards() # Define cards, values and their order
cards_order <- data.frame(card = c(1,3,"rey","caballo","sota",7,6,5,4,2), order = seq(1,10,1), stringsAsFactors = FALSE)
##
# Play games
games <- data.frame(reward = 0, stringsAsFactors = FALSE)
num_games <- 10
for (g in 1:num_games) {
  this_game <- play_tute(smartPlay = FALSE)
  print(paste0(this_game[1]," - ",this_game[2]))
  games[g,1] <- this_game[1]-this_game[2]
}


