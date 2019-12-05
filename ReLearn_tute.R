# Reinforcment Learning Tute cards game
#
## set up
library(tidyverse)
library(ReinforcementLearning)
source("tute_helper_functions.R") # load helper_functions
cards_df <- define_cards() # Define cards, values and their order
cards_order <- data.frame(card = c(1,3,"rey","caballo","sota",7,6,5,4,2), order = seq(1,10,1), stringsAsFactors = FALSE)
##
# play 1 game
this_game <- play_tute_smart(smartPlay = TRUE)
this_game2 <- play_tute_smart(smartPlay = FALSE)
#
# Play multiple games. Generate data for model training
games <- data.frame()
num_games <- 2000
for (g in 1:num_games) {
  this_game <- play_tute_smart(smartPlay = FALSE)
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

match_acc <- as.data.frame(policy(model)[which(policy(model)=="espadas_sota")], stringsAsFactors = FALSE) %>%
  mutate(State = gsub(".",",",row.names(.),fixed = TRUE)) %>%
  group_by(State) %>%
  mutate(HandA = paste(state2cards(State)[[1]],collapse = ",")) %>%
  mutate(match = ifelse(grepl("espadas_1",HandA),1,0)) %>%
  ungroup() %>%
  summarise(accuracy = sum(match)/n())

# esp1 <- policy(model)[which(policy(model)=="espadas_1")]
# esp1 <- as.data.frame(esp1, stringsAsFactors = FALSE) %>%
#   mutate(State = gsub(".",",",row.names(.),fixed = TRUE)) %>%
#   group_by(State) %>%
#   mutate(HandA = paste(state2cards(State)[[1]],collapse = ","))
# esp1 <- mutate(esp1, match = ifelse(grepl("espadas_1",HandA),1,0))
# sum(esp1$match)/nrow(esp1)

####
# Use environment function "actionReward" to create samples on which to train the model
# to feed up the samples, we'll take a set of states generated from "play_tute" function
#
# generate a few thousand states
games <- data.frame()
num_games <- 2000
for (g in 1:num_games) {
  this_game <- play_tute_smart(smartPlay = FALSE)
  if (nrow(games) > 0) games <- bind_rows(games, this_game) else games <- this_game
  if (g %in% seq(0,num_games,50)) print(paste0("games played: ",g))
}
#save(games, file = "sample_games.RData")
load("sample_games.RData")
states <- unique(games$State)
actions <- cards_df$card
# sample data
data <- sampleExperience(N = 20000, env = actionReward, states = states, actions = actions)
#
## Perform reinforcement learning
# Define reinforcement learning parameters
control <- list(alpha = 0.1, gamma = 0.5, epsilon = 0.1)
# Perform reinforcement learning
model <- ReinforcementLearning(data, s = "State", a = "Action", r = "Reward", 
                               s_new = "NextState", control = control)

# Print result
print(model)
policy(model)
# The brute force approach is unmanageable so next step I need to simplify the states. Somehow find an invariant
# function that reduces the number of possible states.
# Example: 




