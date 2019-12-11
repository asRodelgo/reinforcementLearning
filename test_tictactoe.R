board_cols = 3
board_rows = 3
squares <- board_cols * board_rows
#
thisBoard <- c(1,0,0,-1,0,0,0,0,1)
pieced <- rep("", length(thisBoard))
pieced[which(string == 1)] <- "x"
pieced[which(string == -1)] <- "o"
pieced[which(string == 0)] <- "*"

#get a unique hash for each board
calc_hash <- function(board) {
  hash <- 0
  for(piece in seq(squares)) {
    hash <- (hash*board_cols) + board[piece] + 1
  }
  return(hash)
}
calc_hash(thisBoard)
#find all possible next moves
get_next_hashes <- function(board, piece) {
  unused <- which(board == 0)
  
  next_boards <- lapply(unused, function(x, piece) {
    board[x] <- piece
    return(board)
  }, piece = piece)
  #get the hashes of the next boards
  hashes <- lapply(next_boards, calc_hash)
  return(hashes)
}

get_next_hashes(thisBoard,1)
#
thisBoard <- rep(0, squares)
player1_hashes <- c()
player1_hashes <- append(calc_hash(thisBoard), player1_hashes)



