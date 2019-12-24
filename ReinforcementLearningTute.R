#### Reinforcement Learning Tute. Create my own code
## Based on: https://www.robert-hickman.eu/post/r-inforcement_learning_one/
#
####
# Key ideas:
# A computer will learn what is the card it should play (the optimal value) at each stage of the game 
# Each stage can be represented by a state: dealt cards, pinta, cards in hand, etc.
# At each point, player A will have to pick one card from the available hand.
# The computer will learn an optimal choice given a state. 
#
# Training the computer:
# Each state will be initially given a value of 0. That is, cards in hand for player A 
# will have the same value. Moving forward, once the computer is being trained, these values will change
# This means, at any given point, the computer will have 3 options: select the card with the max value,
# select at random or select the card with max value with a probaility of EPSILON. 
# We can define EPSILON (e) as the greedy parameter. If e = 1, algorithm is greedy (always maximizes).
# To help the algorithm value as many states as possible, it may be a good idea to use an 0 < e < 1
#
# The computer will update the values in the following way:
# Play the complete game, if player A wins, it will assign Reward = 1, if loses, = -1, 0 if a tie. 
# This Reward is the value for the final state of the game. We can assign Vstate20' = Reward and 
# back-propagate the rest of the updated values:
#
# Vstate19' = Vstate19 + lr*(gamma*Vstate20' - Vstate19)
# Vstate18' = Vstate18 + lr*(gamma*Vstate19' - Vstate18)
# and so on ...
#
# Where 0 < lr < 1 is the learning rate: How fast the values get updated with new information. 
# If lr = 1, it will change V every single time the computer sees a new case, if closer to 0, 
# it will need a high number of samples to reach to the optimal value for that state.
# And 0 < gamma < 1 reduces the reward as you go back in time, that is, gamma values close to 0, 
# will make early hands will most likely have no effect in the final result (reward). 
#
# Start with e = 0.7, lr = 0.4, gamma = 0.95
#
# For reference: tictactoe's player1_hashes (vector of <=9 states after each move) is equivalent to 
# states (vector of 20 states after each move in tute)
#
# Steps: 
# Play game (play_tute) and return outcome of it: 1 (A wins), -1 (B wins) or 0 (tie) and vector of states [1:20]
# Update values for each of the states in the sequence
# Add states and respective values to the dictionary of states-values

library(tidyverse)
source("tute_helper_functions.R") # load helper_functions
cards_df <- define_cards() # Define cards, values and their order
cards_order <- data.frame(card = c(1,3,"rey","caballo","sota",7,6,5,4,2), order = seq(1,10,1), stringsAsFactors = FALSE)

### play first game to initialize dictionary and values

# play game
this_game <- play_tute(p1_epsilon = 0.5, p2_epsilon = 0.5)
Reward <- sum(this_game$Reward)
Sfrom <- this_game$State
Sto <- this_game$NewState

# update values
values0 <- rep(0, 20) # initial values
values_new <- sort(abs(backfeed_Reward(values = values0, reward = Reward, learning_rate = 0.4, gamma = 0.9)))*sign(Reward)

# update dictionary (create it for the first time on this instance)
dictionary <- data.frame(Sfrom = Sfrom, Sto = Sto, Value = values_new, stringsAsFactors = FALSE)

## play second game
this_game <- play_tute(epsilon = 0.5)
Reward <- sum(this_game$Reward)
Sfrom <- this_game$State
Sto <- this_game$NewState

# update values
this_game_values <- this_game %>% # initial values
  left_join(dictionary, by = c("State"="Sfrom","NewState"="Sto")) %>%
  mutate(Value = replace_na(Value,0))
values0 <- this_game_values$Value
values_new <- sort(abs(backfeed_Reward(values = values0, reward = Reward, learning_rate = 0.4, gamma = 0.9)))*sign(Reward)

this_game <- mutate(this_game, Value = values_new)

# update dictionary
dictionary <- full_join(dictionary, select(this_game, State, NewState, Value), by = c("Sfrom"="State","Sto"="NewState")) %>%
  mutate(Value = ifelse(!is.na(Value.y), Value.y, Value.x)) %>%
  select(-contains("."))

# iterate from here recursively but first modify play_tute to incorporate the decision based on updated values
iter_max <- 100
i <- 1
while (i <= iter_max) {
  this_game <- play_tute(epsilon = 0.5)
  Reward <- sum(this_game$Reward)
  Sfrom <- this_game$State
  Sto <- this_game$NewState
  
  # update values
  this_game_values <- this_game %>% # initial values
    left_join(dictionary, by = c("State"="Sfrom","NewState"="Sto")) %>%
    mutate(Value = replace_na(Value,0))
  values0 <- this_game_values$Value
  values_new <- sort(abs(backfeed_Reward(values = values0, reward = Reward, learning_rate = 0.4, gamma = 0.9)))*sign(Reward)
  
  this_game <- mutate(this_game, Value = values_new)
  
  # update dictionary
  dictionary <- full_join(dictionary, select(this_game, State, NewState, Value), by = c("Sfrom"="State","Sto"="NewState")) %>%
    mutate(Value = ifelse(!is.na(Value.y), Value.y, Value.x)) %>%
    select(-contains("."))
}





