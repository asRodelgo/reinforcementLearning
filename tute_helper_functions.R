### helper_functions driving Tute game
#
# Define cards and values
define_cards <- function() {
  
  cards_df <- data.frame()
  i <- 1
  for (p in c("oros","copas","espadas","bastos")) {
    for (n in c(seq(1,7,1),"sota","caballo","rey")){
      cards_df[i,1] <- paste0(p,"_",n)
      cards_df[i,2] <- switch(n,
                              "1" = 11,
                              "3" = 10,
                              "sota" = 2,
                              "caballo" = 3,
                              "rey" = 4,
                              0)
      i <- i + 1
    }
  }
  names(cards_df) <- c("card","value")
  
  return(cards_df)
}
#
# return value order for a given card
order <- function(c){
  o <- filter(cards_order, card == c)$order
  return(as.numeric(o))
}
#
# vectorize it
order.vector <- Vectorize(order)
#
# return the suit of a given card
card_suit <- function(card) {
  return(str_split(card,"_")[[1]][1]) 
}
#
# return the value of a given card
card_value <- function(c) {
  v <- filter(cards_df, card == c)$value[1]
  return(v)
}
#
# Make a better than random card pick for stage 1 of the game
smart_pick <- function(hand, known_cards, pinta_suit, playFirst = TRUE, played_card=NULL) {
  unkwown <- filter(cards_df, !(card %in% c(hand,known_cards)))
  # known_cards: besides the input hand these cards are known to all players at this point
  penalty_df <- data.frame(card = hand, penalty = rep(0,length(hand)), stringsAsFactors = FALSE) # Initialize reward. The lowest penalty option is chosen as the pick
  if (playFirst) { # player opens round
    for (c in 1:length(hand)) { # evaluate each card in hand
      cardValue <- card_value(penalty_df$card[c])
      if (grepl("caballo", penalty_df$card[c])) {
        if (paste0(card_suit(penalty_df$card[c]),"_rey") %in% hand) {penalty_df$penalty[c] <- -20 
        } else if (paste0(card_suit(penalty_df$card[c]),"_rey") %in% known_cards) {penalty_df$penalty[c] <- -3
        } else penalty_df$penalty[c] <- -10
      }
      if (grepl("rey", penalty_df$card[c])) {
        if (paste0(card_suit(penalty_df$card[c]),"_caballo") %in% hand) {penalty_df$penalty[c] <- -20 
        } else if (paste0(card_suit(penalty_df$card[c]),"_caballo") %in% known_cards) {penalty_df$penalty[c] <- -4
        } else penalty_df$penalty[c] <- -10    
      }  
      # for each card, compute how many can still beat it
      # same suit, higher value
      # pinta suit, any value unless card has pinta suit
      if (card_suit(penalty_df$card[c]) == pinta_suit) {
        penalty_df$penalty[c] <- penalty_df$penalty[c] - 10 # monte +10 potential value
        penalty_df$penalty[c] <- penalty_df$penalty[c] + 2*sum(str_count(unkwown$card, "caballo|rey")) - 16
        penalty_df$penalty[c] <- penalty_df$penalty[c] - length(filter(unkwown, grepl(pinta_suit,card), value > cardValue)$value)
      } else {
        penalty_df$penalty[c] <- penalty_df$penalty[c] - length(filter(unkwown, grepl(card_suit(penalty_df$card[c]),card), value > cardValue)$value)
        penalty_df$penalty[c] <- penalty_df$penalty[c] - length(filter(unkwown, grepl(pinta_suit,card))$value)
        penalty_df$penalty[c] <- penalty_df$penalty[c] - card_value(penalty_df$card[c])
      }
    }
  } else {
    for (c in 1:length(hand)) { # evaluate each card in hand
      cardValue <- card_value(penalty_df$card[c])
      if (grepl("caballo", penalty_df$card[c])) {
        if (paste0(card_suit(penalty_df$card[c]),"_rey") %in% hand) {penalty_df$penalty[c] <- -20 
        } else if (paste0(card_suit(penalty_df$card[c]),"_rey") %in% known_cards) {penalty_df$penalty[c] <- -3
        } else penalty_df$penalty[c] <- -10
      }
      if (grepl("rey", penalty_df$card[c])) {
        if (paste0(card_suit(penalty_df$card[c]),"_caballo") %in% hand) {penalty_df$penalty[c] <- -20 
        } else if (paste0(card_suit(penalty_df$card[c]),"_caballo") %in% known_cards) {penalty_df$penalty[c] <- -4
        } else penalty_df$penalty[c] <- -10    
      }
      if (card_suit(played_card) == pinta_suit) penalty_df$penalty[c] <- penalty_df$penalty[c] - 11 - card_value(penalty_df$card[c]) # monte +10 potential value
      if (card_suit(played_card) == card_suit(penalty_df$card[c])) {
        penalty_df$penalty[c] <- penalty_df$penalty[c] + card_value(penalty_df$card[c]) - card_value(played_card)
      } else if (card_suit(penalty_df$card[c]) == pinta_suit) {
        penalty_df$penalty[c] <- penalty_df$penalty[c] + card_value(penalty_df$card[c])
      } else {
        penalty_df$penalty[c] <- penalty_df$penalty[c] - card_value(penalty_df$card[c])
      }
    }
  }  
  #playThisCard <- filter(penalty_df, penalty == max(penalty))$card[1]
  return(penalty_df)
}
#
# Make a better than random card pick for stage 2 of the game
smart_pick2 <- function(hand, known_cards, pinta_suit, playFirst = TRUE, played_card=NULL) {
  unkwown <- filter(cards_df, !(card %in% c(hand,known_cards)))
  # known_cards: besides the input hand these cards are known to all players at this point
  penalty_df <- data.frame(card = hand, penalty = rep(0,length(hand)), stringsAsFactors = FALSE) # Initialize reward. The lowest penalty option is chosen as the pick
  if (playFirst) { # player opens round
    for (c in 1:length(hand)) { # evaluate each card in hand
      cardValue <- card_value(penalty_df$card[c])
      for (c2 in 1:length(unkwown$card)) {
        if (card_suit(unkwown$card[c2])==card_suit(penalty_df$card[c])) {
          if (order(str_split(unkwown$card[c2],"_")[[1]][2]) < order(str_split(penalty_df$card[c],"_")[[1]][2])) {
            penalty_df$penalty[c] <- penalty_df$penalty[c] - cardValue - unkwown$value[c2]
          } else {
            penalty_df$penalty[c] <- penalty_df$penalty[c] + cardValue + unkwown$value[c2]
          }
        } else if (card_suit(unkwown$card[c2])==pinta_suit){
          penalty_df$penalty[c] <- penalty_df$penalty[c] - cardValue - unkwown$value[c2]
        } else {
          penalty_df$penalty[c] <- penalty_df$penalty[c] + cardValue + unkwown$value[c2]
        }
      }
    }
  } else {
    for (c in 1:length(hand)) { # evaluate each card in hand
      cardValue <- card_value(penalty_df$card[c])
      if (card_suit(played_card) == card_suit(penalty_df$card[c])) {
        if (order(str_split(played_card,"_")[[1]][2]) < order(str_split(penalty_df$card[c],"_")[[1]][2])) {
          penalty_df$penalty[c] <- penalty_df$penalty[c] - cardValue
        } else {
          penalty_df$penalty[c] <- penalty_df$penalty[c] + cardValue
        }
      } else if (card_suit(penalty_df$card[c]) == pinta_suit) {
        penalty_df$penalty[c] <- penalty_df$penalty[c] + card_value(penalty_df$card[c])
      } else {
        penalty_df$penalty[c] <- penalty_df$penalty[c] - card_value(penalty_df$card[c])
      }
    }
  }  
  #playThisCard <- filter(penalty_df, penalty == max(penalty))$card[1]
  return(penalty_df)
}


