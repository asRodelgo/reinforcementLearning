## Rcpp helper functions
library(Rcpp)
#
#function to feed reward back to the agent based on results
cppFunction('NumericVector backfeed_rewardCpp(NumericVector values, double reward, double learning_rate, double gamma) {
  int states = values.size();
  NumericVector new_values = NumericVector::create();
  
  //go from last state backwards
  for( int state = states-1; state >= 0; state--) {
    double new_value = values[state] + learning_rate * ((gamma * reward) - values[state]);
    new_values.push_back(new_value);
    //recurse the reward
    reward = new_value;
  }
  return new_values;
}')

### Version in R until I figure out how to properly install C++ libraries
backfeed_Reward <- function(values, reward, learning_rate, gamma) {
  
  new_values <- numeric()
  for (s in 19:0) {
    new_value <- values[s] + learning_rate*((gamma*reward)-values[s])
    new_values <- c(new_values, new_value)
  }
  return(new_values)
}
#function to choose and implement computer moves
computer_move <- function(piece, board, epsilon) {
  #get potential moves
  potential_move_hashes <- get_next_hashes(board, piece)
  #get the values of the potential moves
  potential_move_vals <- moves_df$value[
    unlist(lapply(potential_move_hashes, function(x) which(moves_df$hash == x)))]
  #choose move based on rewards
  player_move <- choose_moveCpp(epsilon, potential_move_vals)
  #update the board with the new move
  updated_board <- unlist(moves_df$board[
    moves_df$hash == unlist(potential_move_hashes)[player_move]])
  return(updated_board)
}

#function to get the values for each state based on the reward
update_move_vals <- function(player1_reward, player2_reward, 
                             player1_hashes, player2_hashes,
                             learning_rate,gamma) {
  player1_newvals <- backfeed_rewardCpp(moves_df$value[
    unlist(lapply(player1_hashes, function(x) which(moves_df$hash == x)))],
    player1_reward, learning_rate, gamma)
  player2_newvals <- backfeed_rewardCpp(moves_df$value[
    unlist(lapply(player2_hashes, function(x) which(moves_df$hash == x)))],
    player2_reward, learning_rate, gamma)
  new_vals <- list(player1_newvals, player2_newvals)
  return(new_vals)
}



