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
num_games <- 2000
for (g in 1:num_games) {
  this_game <- play_tute(smartPlay = FALSE)
  if (nrow(games) > 0) games <- bind_rows(games, this_game) else games <- this_game
  if (g %in% seq(0,num_games,50)) print(paste0("games played: ",g))
}
###########################################
# some stats
cards_stats <- group_by(games, Action) %>%
  summarise(avgReward = mean(Reward), medianReward = median(Reward))
#
#games2 <- mutate(games, HandA = state2cards_vector(State)[[1]])
#
##### Train model 
# Define reinforcement learning parameters
control <- list(alpha = 1, gamma = 1, epsilon = 0.1)
# Perform reinforcement learning
model <- ReinforcementLearning(games, s = "State", a = "Action", r = "Reward", 
                               s_new = "NewState", iter = 10, control = control)
# Print optimal policy
table(policy(model))
policy(model)
summary(model)
plot(model)

####
# write a function that compares predicted action and handA: see how many actions fall outside of 
# possible hands. Somehow I need my model to predict actions within the possible cards.
####
## figure out why espadas_1 is the default pick for the model
esp1 <- policy(model)[which(policy(model)=="espadas_1")]
esp1 <- as.data.frame(esp1, stringsAsFactors = FALSE)



