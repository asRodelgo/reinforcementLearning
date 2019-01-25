# Reinforcment Learning Tute cards game
#
## set up
library(tidyverse)
source("tute_helper_functions.R") # load helper_functions
cards_df <- define_cards() # Define cards, values and their order
cards_order <- data.frame(card = c(1,3,"rey","caballo","sota",7,6,5,4,2), order = seq(1,10,1), stringsAsFactors = FALSE)
##
# play 1 game
this_game <- play_tute(smartPlay = TRUE)
#
# Play multiple games. Generate data for model training
games <- data.frame()
num_games <- 300
for (g in 1:num_games) {
  this_game <- play_tute(smartPlay = TRUE)
  if (nrow(games) > 0) games <- bind_rows(games, this_game) else games <- this_game
  if (g %in% seq(0,num_games,50)) print(paste0("games played: ",g))
}
###########################################

##### Train model 
# Define reinforcement learning parameters
control <- list(alpha = 0.2, gamma = 0.4, epsilon = 0.1)
# Perform reinforcement learning
model <- ReinforcementLearning(games, s = "State", a = "Action", r = "Reward", 
                               s_new = "NewState", iter = 1, control = control)
# Print optimal policy
policy(model)
summary(model)




