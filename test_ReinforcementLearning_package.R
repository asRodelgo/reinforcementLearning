## Testing ReinforcementLearning package
## From: https://cran.r-project.org/web/packages/ReinforcementLearning/ReinforcementLearning.pdf
# 
#install.packages("ReinforcementLearning")
library(tidyverse)
library(ReinforcementLearning)
#
#data("tictactoe")
#head(tictactoe, 5)
# 
## Example:
# Define state and action sets
states <- c("s1", "s2", "s3", "s4")
actions <- c("up", "down", "left", "right")
# Load built-in environment function for 2x2 gridworld
# inputs: state, action
# outputs: list(NextState, Reward)
env <- gridworldEnvironment
#
# Sample N = 1000 random sequences from the environment
data <- sampleExperience(N = 1000,
                         env = env,
                         states = states,
                         actions = actions)
head(data)
# Define reinforcement learning parameters
control <- list(alpha = 0.1, gamma = 0.5, epsilon = 0.1)
# Perform reinforcement learning
model <- ReinforcementLearning(data,
                               s = "State",
                               a = "Action",
                               r = "Reward",
                               s_new = "NextState",
                               control = control)
# Print policy
policy(model)
# Print state-action function
print(model)
# Print summary statistics
summary(model)
#
# Applying policy to unseen data
data_unseen <- data.frame(State = c("s1", "s2", "s1"),
                          stringsAsFactors = FALSE)
# Pick optimal action
data_unseen$OptimalAction <- predict(model, data_unseen$State)
data_unseen
#
## Update an existing policy with new observational data
# Sample N = 1000 sequences from the environment
# using epsilon-greedy action selection
data_new <- sampleExperience(N = 1000,
                             env = env,
                             states = states,
                             actions = actions,
                             actionSelection = "epsilon-greedy",
                             model = model,
                             control = control)
# Update the existing policy using new training data
model_new <- ReinforcementLearning(data_new,
                                   s = "State",
                                   a = "Action",
                                   r = "Reward",
                                   s_new = "NextState",
                                   control = control,
                                   model = model)
# Print result
print(model_new)
# Plot reinforcement learning curve
plot(model_new)
#
#
############################################
# Learning tic-tac-toe
############################################
#
# Load dataset
data("tictactoe")

# Define reinforcement learning parameters
control <- list(alpha = 0.2, gamma = 0.4, epsilon = 0.1)

# Perform reinforcement learning
model <- ReinforcementLearning(tictactoe[1:1000,], s = "State", a = "Action", r = "Reward", 
                               s_new = "NextState", iter = 1, control = control)

# Print optimal policy
policy(model)


