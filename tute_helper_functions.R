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
# return value order for a given card number
order <- function(c){
  o <- filter(cards_order, card == c)$order
  return(as.numeric(o))
}
#
# vectorize it
order.vector <- Vectorize(order)
#
# return value order for a given card
card_order <- function(card){
  card_number <- card_number(card)
  order <- filter(cards_order, card == card_number)$order
  return(as.numeric(order))
}
# vectorize
card_order.vector <- Vectorize(card_order)
# return the suit of a given card
card_suit <- function(card) {
  return(str_split(card,"_")[[1]][1]) 
}
# vectorize
card_suit.vector <- Vectorize(card_suit)
# return the number of a given card
card_number <- function(card) {
  return(str_split(card,"_")[[1]][2]) 
}
#
# return the value of a given card
card_value <- function(c) {
  v <- filter(cards_df, card == c)$value
  return(v)
}
#
# transform State sequence into cards
state2cards <- function(state) {
  
  # state <- games$State[10]
  # state <- this_game$State[10]
  
  state_df <- data.frame(State = str_split(state,",")[[1]], stringsAsFactors = FALSE)
  turn <- as.numeric(filter(state_df, State %in% seq(1,20,1)))
  play_first <- as.character(filter(state_df, grepl("W",State)))
  state_df <- filter(state_df, !(State %in% seq(1,20,1)), !(grepl("W",State)))
  state_p <- bind_cols(cards_df, state_df)
  
  pinta <- str_split(filter(state_p, grepl("P",State))$card[1], "_")[[1]][1]
  handA <- filter(state_p, grepl("A",State))$card
  known_cards <- filter(state_p, grepl("K",State))$card
  
  return(list(handA=handA,pinta=pinta,known_cards=known_cards,turn=turn,play_first=play_first))
} 
# vectorize it
state2cards_vector <- Vectorize(state2cards)
#
# Transform cards situation to state sequence
cards2state <- function(handA, pinta_suit, known_cards, turn, play_first = "A") {
  
  cards_state <- mutate(cards_df, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
    mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State)) %>%
    select(-value)
  state = paste0(paste(cards_state$State, collapse = ","),",",turn,",W",play_first)
  
  return(state)
} 

# Calculate the risk of an opponent calling a cante or tute
computeCanteRisk <- function(play_card = card, unknown = unknown, pinta_suit = pinta_suit) {
  
  suits <- c("oros","copas","espadas","bastos")
  N <- nrow(unknown)
  unknown <- mutate(unknown, order = card_order.vector(card))
  E_risk <- 0
  points <- 0
  K <- data.frame()
  for (s in suits) {
    if (s == pinta_suit) { # if suit is pinta
      if (card_suit(play_card) == pinta_suit) {
        if (nrow(K) > 0) {
          K <- bind_rows(K, filter(unknown, grepl(card_suit(play_card),card), order > card_order(play_card)))
        } else K <- filter(unknown, grepl(card_suit(play_card),card), order > card_order(play_card))
      }
    } else {
      if (card_suit(play_card) == s) {
        if (nrow(K) > 0) {
          K <- bind_rows(K, filter(unknown, grepl(s,card), order > card_order(play_card)))
        } else K <- filter(unknown, grepl(s,card), order > card_order(play_card))
      } else {
        if (nrow(K) > 0) {
          K <- bind_rows(K, filter(unknown, grepl(s,card)))
        } else K <- filter(unknown, grepl(s,card))
      }
    }
  }
  # K = unknwon cards playerA beats with this card
  # antiK = unknwon cards that beat this card
  antiK <- filter(unknown, !(card %in% K$card))
  #
  for (s in suits) {
    # potential cante
    if ((paste0(s,"_caballo") %in% unknown$card) & (paste0(s,"_rey") %in% unknown$card)) { # potential cante
      prob_cante <- dhyper(x = 2, m = 2, n = N-2, k = 6) # prob of player B holding this suit's cante in his hand
      antiK <- filter(antiK, !(card %in% c(paste0(s,"_caballo"), paste0(s,"_rey"))))
      if (nrow(antiK) > 0) { # player B can play winning cards without using cante cards
        #risk <- max(risk, risk + 40)
        cond_prob_winner <- sum(dhyper(x = 1:nrow(antiK), m = nrow(antiK), n = N-2, k = 4)) # conditional prob of player B having winning cards besides the cante cards (sample = 6-2 = 4)
        # times prob of drawing cante gives:
        prob <- cond_prob_winner*prob_cante
        # expected risk
        if (s == pinta_suit) E_risk <- E_risk + prob*40 else E_risk <- E_risk + prob*20
      }
    }
  }
  # potential Tute
  # for (figure in c("caballo","rey")) {
  #   if (sum(str_count(unknown$card,figure)) == 4) { # tute de reyes
  #     prob_tute <- dhyper(x = 4, m = 4, n = N-4, k = 6) # conditional prob of player B having winning cards besides the cante cards (sample = 6-2 = 4)
  #     antiK <- filter(antiK, !(grepl(figure,card)))
  #     if (nrow(antiK) > 0) {
  #       cond_prob_winner <- sum(dhyper(x = 1:nrow(antiK), m = nrow(antiK), n = N-4, k = 2)) # conditional prob of player B having winning cards besides the cante cards (sample = 6-2 = 4)
  #       # times prob of drawing cante gives:
  #       prob <- cond_prob_winner*prob_tute
  #       # expected risk
  #       E_risk <- E_risk + prob*200
  #     }
  #   }
  # }
  return(E_risk)
}

#
# Calculate the expected gained value out from player B of a played card by player A. Assuming Player B 
# plays smart, i.e., maximizes his reward. Without this assumption, it's just averaging the
# value of unknown cards that can beat player A's card.
expectedValueAdded <- function(play_card = card, unknown = unknown, pinta_suit = pinta_suit) {
  
  suits <- c("oros","copas","espadas","bastos")
  N <- nrow(unknown)
  unknown <- mutate(unknown, order = card_order.vector(card))
  E_value <- 0
  if (card_suit(play_card) == pinta_suit) { # if suit is pinta
    K <- filter(unknown, grepl(card_suit(play_card),card), order > card_order(play_card))
    if (nrow(K) > 0) {
      E_value <- E_value + mean(K$value)
      # for (i in 1:nrow(K)) {
      #   # expected winning value
      #   E_odds <- E_odds + dhyper(x = 1, m = 1, n = nrow(K)-1, k = 6)*K$value[i]
      # }
    }
  } else {
    K <- bind_rows(filter(unknown, grepl(card_suit(play_card),card), order > card_order(play_card)),
                   filter(unknown, grepl(pinta_suit,card)))
    if (nrow(K) > 0) {
      E_value <- E_value + mean(K$value)
      # for (i in 1:nrow(K)) {
      # # expected winning value
      #   E_odds <- E_odds + dhyper(x = 1, m = 1, n = K-1, k = 6)*K$value[i]
      # }
    }
  }
  return(E_value)
}
#
# Expected value of player A playing a particular card
expectedValue <- function(hand = hand, play_card = card, unknown = unknown, pinta_suit = pinta_suit) {
  
  suits <- c("oros","copas","espadas","bastos")
  N <- nrow(unknown)
  unknown <- mutate(unknown, order = card_order.vector(card))
  hand_f <- hand[-which(hand == play_card)]
  E_value <- 0
  points <- 0
  K <- data.frame()
  for (s in suits) {
    if (s == pinta_suit) { # if suit is pinta
      if (card_suit(play_card) == pinta_suit) {
        if (nrow(K) > 0) { 
          K <- bind_rows(K, filter(unknown, grepl(card_suit(play_card),card), order > card_order(play_card)))
        } else K <- filter(unknown, grepl(card_suit(play_card),card), order > card_order(play_card))
      } 
    } else {
      if (card_suit(play_card) == s) {
        if (nrow(K) > 0) { 
          K <- bind_rows(K, filter(unknown, grepl(s,card), order > card_order(play_card)))
        } else K <- filter(unknown, grepl(s,card), order > card_order(play_card))
      } else {
        if (nrow(K) > 0) {
          K <- bind_rows(K, filter(unknown, grepl(s,card)))
        } else K <- filter(unknown, grepl(s,card))
      }
    }
  }
  # K = unknwon cards playerA beats with this card
  # antiK = unknwon cards that beat this card
  antiK <- filter(unknown, !(card %in% K$card))
  points <- sum(K$value) + nrow(K)*card_value(play_card) - sum(antiK$value) - nrow(antiK)*card_value(play_card)
  E_value <- points/nrow(unknown)
  #if (nrow(K) > 0) prob_win <- sum(dhyper(x = 1:6, m = nrow(K), n = N-nrow(K), k = 6)) else prob_win <- 0
  #E_value <- E_value + card_value(play_card)*prob_win
  #
  # factor in cantes
  for (s in suits) {
    if ((paste0(s,"_caballo") == play_card) & (paste0(s,"_rey") %in% hand_f) & !(paste0(s,"_rey") %in% unknown$card)) { # potential cante
      if (s == pinta_suit) E_value <- E_value - 40 else E_value <- E_value - 20
    } else if ((paste0(s,"_rey") == play_card) & (paste0(s,"_caballo") %in% hand_f) & !(paste0(s,"_caballo") %in% unknown$card)) {
      if (s == pinta_suit) E_value <- E_value - 40 else E_value <- E_value - 20
    } else if ((paste0(s,"_caballo") %in% hand_f) & (paste0(s,"_rey") %in% hand_f)  & !(paste0(s,"_rey") %in% unknown$card)) {
      prob_cantepair <- nrow(K)/N # assuming payer A wins the hand
      if (s == pinta_suit) E_value <- E_value + prob_cantepair*40 else E_value <- E_value + prob_cantepair*20
    } else if (paste0(s,"_caballo") %in% hand_f) {
      K <- filter(unknown, card == paste0(s,"_rey"))
      prob_cantepair <- nrow(K)/N # assuming payer A wins the hand
      if (s == pinta_suit) E_value <- E_value + prob_cantepair*40 else E_value <- E_value + prob_cantepair*20
    } else if (paste0(s,"_rey") %in% hand_f) {
      K <- filter(unknown, card == paste0(s,"_caballo"))
      prob_cantepair <- nrow(K)/N
      if (s == pinta_suit) E_value <- E_value + prob_cantepair*40 else E_value <- E_value + prob_cantepair*20 
    } else if (paste0(s,"_caballo") == play_card) {
      K <- filter(unknown, card == paste0(s,"_rey"))
      prob_cantepair <- nrow(K)/N # assuming payer A wins the hand
      if (s == pinta_suit) E_value <- E_value - prob_cantepair*40 else E_value <- E_value - prob_cantepair*20
    } else if (paste0(s,"_rey") == play_card) {
      K <- filter(unknown, card == paste0(s,"_caballo"))
      prob_cantepair <- nrow(K)/N
      if (s == pinta_suit) E_value <- E_value - prob_cantepair*40 else E_value <- E_value - prob_cantepair*20 
    }
  }
  ### factor in 10 de monte
  # how many cards does this card beat? The more cards it can beat, the higher the penalty (the bigger incentive to keep it)
  pot_monte <- 10*nrow(K)/N
  E_value <- E_value - pot_monte
  #### factor in tutes: For later
  # for (figure in c("caballo","rey")) {
  #   if (sum(str_count(unknown$card,figure)) == 4) { # tute de reyes
  #     prob_tute <- dhyper(x = 4, m = 4, n = N-4, k = 6) # conditional prob of player B having winning cards besides the cante cards (sample = 6-2 = 4)  
  #     unknown_f <- filter(unknown, !(grepl(figure,card)))
  ####
  
  return(E_value)
}
#
# Make a better than random card pick for stage 1 of the game
smart_pick <- function(hand, known_cards, pinta_suit, playFirst = TRUE, played_card=NULL, actionCard = NULL) {
  # known_cards: besides the input hand these cards are known to all players at this point
  unkwown <- filter(cards_df, !(card %in% c(hand,known_cards)))
  # if a specific card must be evaluated instead of the whole hand
  if (!is.null(actionCard)) cardPool <- actionCard else cardPool <- hand
  penalty_df <- data.frame(card = cardPool, penalty = rep(0,length(cardPool)), stringsAsFactors = FALSE) # Initialize reward. The lowest penalty option is chosen as the pick
  if (playFirst) { # player opens round
    for (c in 1:length(cardPool)) { # evaluate each card in hand or selected action card
      cardValue <- card_value(penalty_df$card[c])
      
      # check whether player B can produce cante or tute if he wins the hand
      canteRisk <- computeCanteRisk(play_card = penalty_df$card[c], unknown = unknown, pinta_suit = pinta_suit)
      penalty_df$penalty[c] <- penalty_df$penalty[c] - canteRisk
      #
      if (grepl("caballo", penalty_df$card[c])) {
        if ((paste0(card_suit(penalty_df$card[c]),"_rey") %in% hand) & 
            !((paste0(card_suit(penalty_df$card[c]),"_rey") %in% known_cards) | (paste0(card_suit(penalty_df$card[c]),"_caballo") %in% known_cards))) {
          if (card_suit(penalty_df$card[c]) == pinta_suit) { # pinta
            penalty_df$penalty[c] <- penalty_df$penalty[c] - 40 # got the cante pair and I have not yet called this cante
          } else {
            penalty_df$penalty[c] <- penalty_df$penalty[c] - 20 # got the cante pair and I have not yet called this cante
          }
        } else if (paste0(card_suit(penalty_df$card[c]),"_rey") %in% known_cards) {
          penalty_df$penalty[c] <- penalty_df$penalty[c] - 3 # I know I can't call this cante
        } else penalty_df$penalty[c] <- penalty_df$penalty[c] # I don't have the cante pair. 
      }
      if (grepl("rey", penalty_df$card[c])) {
        if ((paste0(card_suit(penalty_df$card[c]),"_caballo") %in% hand) & 
            !((paste0(card_suit(penalty_df$card[c]),"_caballo") %in% known_cards) | (paste0(card_suit(penalty_df$card[c]),"_rey") %in% known_cards))) {
          penalty_df$penalty[c] <- -20  # got the cante pair and I have not yet called this cante
        } else if (paste0(card_suit(penalty_df$card[c]),"_caballo") %in% known_cards) {
          penalty_df$penalty[c] <- -4  # I know I can't call this cante
        } else penalty_df$penalty[c]  # I don't have the cante pair.   
      }  
      # for each card, compute how many can still beat it
      # same suit, higher value
      # pinta suit, any value unless card has pinta suit
      if (card_suit(penalty_df$card[c]) == pinta_suit) {
        penalty_df$penalty[c] <- penalty_df$penalty[c] - 10 # risk of losing monte +10 points 
        penalty_df$penalty[c] <- penalty_df$penalty[c] + 2*sum(str_count(unkwown$card, "caballo|rey")) - 16
        penalty_df$penalty[c] <- penalty_df$penalty[c] - length(filter(unkwown, grepl(pinta_suit,card), value > cardValue)$value)
      } else {
        penalty_df$penalty[c] <- penalty_df$penalty[c] - length(filter(unkwown, grepl(card_suit(penalty_df$card[c]),card), value > cardValue)$value)
        penalty_df$penalty[c] <- penalty_df$penalty[c] - length(filter(unkwown, grepl(pinta_suit,card))$value)
        penalty_df$penalty[c] <- penalty_df$penalty[c] - card_value(penalty_df$card[c])
      }
    }
  } else {
    for (c in 1:length(cardPool)) { # evaluate each card in hand
      cardValue <- card_value(penalty_df$card[c])
      if (grepl("caballo", penalty_df$card[c])) {
        if ((paste0(card_suit(penalty_df$card[c]),"_rey") %in% hand) & 
            !((paste0(card_suit(penalty_df$card[c]),"_rey") %in% known_cards) | (paste0(card_suit(penalty_df$card[c]),"_caballo") %in% known_cards))) {
          penalty_df$penalty[c] <- -20 
        } else if (paste0(card_suit(penalty_df$card[c]),"_rey") %in% known_cards) {penalty_df$penalty[c] <- -3
        } else penalty_df$penalty[c] <- -10
      }
      if (grepl("rey", penalty_df$card[c])) {
        if ((paste0(card_suit(penalty_df$card[c]),"_caballo") %in% hand) & 
            !((paste0(card_suit(penalty_df$card[c]),"_caballo") %in% known_cards) | (paste0(card_suit(penalty_df$card[c]),"_rey") %in% known_cards))) {
          penalty_df$penalty[c] <- -20 
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
smart_pick2 <- function(hand, known_cards, pinta_suit, playFirst = TRUE, played_card=NULL, actionCard = NULL) {
  # known_cards: besides the input hand these cards are known to all players at this point
  unkwown <- filter(cards_df, !(card %in% c(hand,known_cards)))
  # if a specific card must be evaluated instead of the whole hand
  if (!is.null(actionCard)) cardPool <- actionCard else cardPool <- hand
  penalty_df <- data.frame(card = cardPool, penalty = rep(0,length(cardPool)), stringsAsFactors = FALSE) # Initialize reward. The lowest penalty option is chosen as the pick
  if (playFirst) { # player opens round
    for (c in 1:length(cardPool)) { # evaluate each card in hand
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
    for (c in 1:length(cardPool)) { # evaluate each card in hand
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
#
# Play a game of Tute and return final score for Player A and Player B (Player A always starts)
play_tute_smart <- function(smartPlay = FALSE, verbose = FALSE){
  
  # initial deck of cards
  deck <- cards_df$card
  # deal initial hands
  handA <- sample(deck,6)
  #handA <- c("bastos_caballo", "copas_caballo","oros_caballo","espadas_caballo","oros_4","copas_7" )
  #handA <- c("bastos_caballo", "bastos_rey","oros_4","espadas_caballo","oros_5","copas_7" )
  deck_left <- deck[-which(deck %in% handA)]
  handB <- sample(deck_left,6)
  deck_left <- deck_left[-which(deck_left %in% handB)]
  pinta <- sample(deck_left,1)
  deck_left <- deck_left[-which(deck_left %in% pinta)]
  #
  ####
  #
  # start play: Loop while deck_left is not empty
  total_pointsA <- 0
  total_pointsB <- 0
  #
  pinta_suit <- str_split(pinta,"_")[[1]][1]
  pinta_number <- str_split(pinta,"_")[[1]][2]
  pinta_value <- filter(cards_df, card == pinta)$value
  pinta2 <- paste0(pinta_suit,"_2")
  pinta7 <- paste0(pinta_suit,"_7")
  #
  winA <- TRUE # player A starts
  cantes <- c() # keep count of cantes by their suits
  known_cards <- pinta # keep count of cards played
  keep_on <- TRUE
  ##
  # initialize Re-learn actions and status
  cards_state <- mutate(cards_df, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
    mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State)) %>%
    select(-value)
  data_rele <- data.frame(State = paste0(paste(cards_state$State, collapse = ","),",",1,",","WA"), 
                          Action = "",
                          Reward = 0,
                          NewState = "",
                          Details = "",
                          HandA = paste(handA, collapse = ","),
                          HandB = paste(handB, collapse = ","),
                          stringsAsFactors = FALSE)
  # Action counter
  act <- 1
  while(length(deck_left)>0 & keep_on) {
    pointsA <- 0
    pointsB <- 0
    # Check for tute or cantes
    # If tute, game is over
    if (length(deck_left)<26) { # at least 1 hand has been played
      if (winA) {
        if (sum(str_count(handA, "caballo"))==4 | sum(str_count(handA, "rey"))==4) {
          pointsA <- 200
          data_rele$NewState[act] <- data_rele$State[act]
          #data_rele$Action[act] <- "tute" # end of game
          card_pool <- unique(filter(cards_state, grepl("A",State))$card)
          if (sum(str_count(handA, "caballo"))==4) { # pick any of the non-tute cards
            data_rele$Action[act] <- sample(card_pool[-which(grepl("caballo",card_pool))],1) 
          } else {
            data_rele$Action[act] <- sample(card_pool[-which(grepl("rey",card_pool))],1) 
          }  
          #data_rele$Reward[act] <- pointsA - pointsB
          data_rele$Reward[act-1] <- data_rele$Reward[act-1] + pointsA - pointsB # tute/cante points are assigned to last play
          data_rele$Details[act-1] <- paste(data_rele$Details[act-1],"tute_playerA", collapse = ";")
        }
        # else check for cantes
        if (nrow(tuteA)>0) {
          if (verbose) print(paste0("Player A cante in :",tuteA$suit[1]," ",10*tuteA$tute[1]))
          cantes <- c(cantes,tuteA$suit[1])
          known_cards <- c(known_cards, paste0(tuteA$suit[1],"_caballo"),paste0(tuteA$suit[1],"_rey"))
          #pointsA <- pointsA + 10*tuteA$tute[1]
          data_rele$Reward[act-1] <- data_rele$Reward[act-1] + 10*tuteA$tute[1] # tute/cante points are assigned to last play
          data_rele$Details[act-1] <- paste(data_rele$Details[act-1],paste0("canteA_",tuteA$suit[1]), collapse = ";")
          cards_state_cantes <- mutate(cards_state, State = ifelse(card %in% handA, "A", "")) %>%
            mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State)) %>%
            mutate(State = ifelse(card %in% known_cards, paste0(State,"K"), State))
          data_rele$NewState[act-1] <- paste0(paste(cards_state_cantes$State, collapse = ","),",",act,",W",prev_hand_winner)
          tuteA$tute[1] <- 0
          tuteA <- filter(tuteA, tute > 0)
          # cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
          #   mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State))
          # data_rele$NewState[act] <- paste(cards_state$State, collapse = ",")
          # data_rele$Action[act] <- paste0("cante_",tuteA$suit[1])
          # data_rele$Reward[act] <- pointsA - pointsB - data_rele$Reward[act-1]
          # act <- act + 1
          # data_rele[act,]$State <- data_rele$NewState[act-1]
        }
        
      } else {
        if (sum(str_count(handB, "caballo"))==4 | sum(str_count(handB, "rey"))==4) {
          pointsB <- 200
          data_rele$NewState[act] <- data_rele$State[act]
          #data_rele$Action[act] <- "tute" # end of game
          data_rele$Action[act] <- sample(unique(filter(cards_state, grepl("A",State))$card),1) # pick any, the game is over. PlayerB wins
          #data_rele$Reward[act] <- pointsA - pointsB
          data_rele$Reward[act-1] <- data_rele$Reward[act-1] + pointsA - pointsB # tute/cante points are assigned to last play
          data_rele$Details[act-1] <- paste(data_rele$Details[act-1],"tute_playerB", collapse = ";")
        }
        if (nrow(tuteB)>0) {
          if (verbose) print(paste0("Player B cante in :",tuteB$suit[1],10*tuteB$tute[1]))
          cantes <- c(cantes,tuteB$suit[1])
          known_cards <- c(known_cards, paste0(tuteA$suit[1],"_caballo"),paste0(tuteA$suit[1],"_rey"))
          #pointsB <- pointsB + 10*tuteB$tute[1]
          data_rele$Reward[act-1] <- data_rele$Reward[act-1] - 10*tuteB$tute[1] # tute/cante points are assigned to last play
          data_rele$Details[act-1] <- paste(data_rele$Details[act-1],paste0("canteB_",tuteB$suit[1]), collapse = ";")
          cards_state_cantes <- mutate(cards_state, State = ifelse(card %in% handA, "A", "")) %>%
            mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State)) %>%
            mutate(State = ifelse(card %in% known_cards, paste0(State,"K"), State))
          data_rele$NewState[act-1] <- paste0(paste(cards_state_cantes$State, collapse = ","),",",act,",W",prev_hand_winner)
          tuteB$tute[1] <- 0
          tuteB <- filter(tuteB, tute > 0)
          # cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
          #   mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State))
          # data_rele$NewState[act] <- paste(cards_state$State, collapse = ",")
          # data_rele$Action[act] <- paste0("cante_",tuteB$suit[1])
          # data_rele$Reward[act] <- pointsA - pointsB - data_rele$Reward[act-1]
          # act <- act + 1
          # data_rele[act,]$State <- data_rele$NewState[act-1]
        }
      }
    }
    #
    # # # rule 1: switch pinta when 2 or 7
    # if (pinta_number %in% c(2,4,5,6)) {
    #   if (pinta2 %in% handA) {
    #     handA <- handA[-which(handA == pinta2)]
    #     handA <- c(handA,pinta)
    #     pinta <- pinta2
    #     known_cards <- c(known_cards,pinta)
    #     # cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
    #     #   mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State))
    #     # data_rele$NewState[act] <- paste(cards_state$State, collapse = ",")
    #     # data_rele$Action[act] <- "pinta_2"
    #     # data_rele$Reward[act] <- 0
    #     # act <- act + 1
    #     # data_rele[act,]$State <- data_rele$NewState[act-1]
    #     data_rele$Details[act] <- paste(data_rele$Details[act],"pintaA_2", collapse = ";")
    #   }
    #   if (pinta2 %in% handB) {
    #     handB <- handB[-which(handB == pinta2)]
    #     handB <- c(handB,pinta)
    #     pinta <- pinta2
    #     known_cards <- c(known_cards,pinta)
    #     # cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
    #     #   mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State))
    #     # data_rele$NewState[act] <- paste(cards_state$State, collapse = ",")
    #     # data_rele$Action[act] <- "pinta_2"
    #     # data_rele$Reward[act] <- 0
    #     # act <- act + 1
    #     # data_rele[act,]$State <- data_rele$NewState[act-1]
    #     data_rele$Details[act] <- paste(data_rele$Details[act],"pintaB_2", collapse = ";")
    #   }
    # } else {
    #   if (pinta7 %in% handA) {
    #     handA <- handA[-which(handA == pinta7)]
    #     handA <- c(handA,pinta)
    #     pinta <- pinta7
    #     known_cards <- c(known_cards,pinta)
    #     # cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
    #     #   mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State))
    #     # data_rele$NewState[act] <- paste(cards_state$State, collapse = ",")
    #     # data_rele$Action[act] <- "pinta_7"
    #     # data_rele$Reward[act] <- 0
    #     # act <- act + 1
    #     # data_rele[act,]$State <- data_rele$NewState[act-1]
    #     data_rele$Details[act] <- paste(data_rele$Details[act],"pintaA_7", collapse = ";")
    #   }
    #   if (pinta7 %in% handB) {
    #     handB <- handB[-which(handB == pinta7)]
    #     handB <- c(handB,pinta)
    #     pinta <- pinta7
    #     known_cards <- c(known_cards,pinta)
    #     # cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
    #     #   mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State))
    #     # data_rele$NewState[act] <- paste(cards_state$State, collapse = ",")
    #     # data_rele$Action[act] <- "pinta_7"
    #     # data_rele$Reward[act] <- 0
    #     # act <- act + 1
    #     # data_rele[act,]$State <- data_rele$NewState[act-1]
    #     data_rele$Details[act] <- paste(data_rele$Details[act],"pintaB_7", collapse = ";")
    #   }
    # }
    #
    # rule 2: call tute or cante
    tuteA <- t(data.frame(str_split(handA, "_"))) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      rename(suit = X1, number = X2) %>%
      filter(!(suit %in% cantes)) %>% # don't consider cantes already scored
      mutate(tute = ifelse(number %in% c("caballo","rey"),1,0)) %>%
      group_by(suit) %>%
      summarise(tute=sum(tute)) %>%
      ungroup() %>%
      filter(tute >=2) %>%
      mutate(tute = ifelse(suit == pinta_suit,tute*2,tute)) %>%
      arrange(desc(tute))
    tuteB <- t(data.frame(str_split(handB, "_"))) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      rename(suit = X1, number = X2) %>%
      filter(!(suit %in% cantes)) %>% # don't consider cantes already scored
      mutate(tute = ifelse(number %in% c("caballo","rey"),1,0)) %>%
      group_by(suit) %>%
      summarise(tute=sum(tute)) %>%
      ungroup() %>%
      filter(tute >=2) %>%
      mutate(tute = ifelse(suit == pinta_suit,tute*2,tute)) %>%
      arrange(desc(tute))
    #
    # rule 3: Play card
    if (winA) {
      if (smartPlay) {  # evaluate the best available play
        #eval_handA <- smart_pick(handA, known_cards, pinta_suit, playFirst = TRUE) 
        #playA <- filter(eval_handA, penalty == max(penalty))$card[1]
        # actionReward
        this_state <- cards2state(handA = handA, pinta_suit = pinta_suit, known_cards = known_cards, turn = act, play_first = "A")
        max_reward <- -100
        playA <- handA[1]
        print("A plays first:")
        for (c in handA) {
          thisReward <- actionReward(state = this_state, action = c)$Reward
          print(paste0(c," ",thisReward))
          if (thisReward > max_reward) {
            max_reward <- thisReward
            playA <- c
          }
        }
      } else playA <- sample(handA,1)
      
      suitA <- str_split(playA,"_")[[1]][1]
      numberA <- str_split(playA,"_")[[1]][2]
      valueA <- filter(cards_df, card == playA)$value
      handA <- handA[-which(handA == playA)]
      # player B responds. Need to take into account the card played by player A
      if (smartPlay) {
        #eval_handB <- smart_pick(handB, known_cards, pinta_suit, playFirst = FALSE, played_card = playA) 
        #playB <- filter(eval_handB, penalty == max(penalty))$card[1]
        # actionReward
        # this_state from the point of view of player B
        this_state <- cards2state(handA = handB, pinta_suit = pinta_suit, known_cards = known_cards, turn = act, play_first = "B")
        max_reward <- -100
        playB <- handB[1]
        print("B responds:")
        for (c in handB) {
          thisReward <- actionReward(state = this_state, action = c, played_card = playA)$Reward
          print(paste0(c," ",thisReward))
          if (thisReward > max_reward) {
            max_reward <- thisReward
            playB <- c
          }
        }
      } else playB <- sample(handB,1)
      
      handB <- handB[-which(handB == playB)]
      suitB <- str_split(playB,"_")[[1]][1]
      numberB <- str_split(playB,"_")[[1]][2]
      valueB <- filter(cards_df, card == playB)$value
      known_cards <- c(known_cards,playA,playB)
    } else { # winA = FALSE
      #
      if (smartPlay) {
        #eval_handB <- smart_pick(handB, known_cards, pinta_suit, playFirst = TRUE) 
        #playB <- filter(eval_handB, penalty == max(penalty))$card[1]
        # actionReward
        this_state <- cards2state(handA = handB, pinta_suit = pinta_suit, known_cards = known_cards, turn = act, play_first = "A")
        max_reward <- -100
        playB <- handB[1]
        print("B plays first:")
        for (c in handB) {
          thisReward <- actionReward(state = this_state, action = c)$Reward
          print(paste0(c," ",thisReward))
          if (thisReward > max_reward) {
            max_reward <- thisReward
            playB <- c
          }
        }
      } else playB <- sample(handB,1)
      
      handB <- handB[-which(handB == playB)]
      suitB <- str_split(playB,"_")[[1]][1]
      numberB <- str_split(playB,"_")[[1]][2]
      valueB <- filter(cards_df, card == playB)$value
      #
      if (smartPlay) {
        #eval_handA <- smart_pick(handA, known_cards, pinta_suit, playFirst = FALSE, played_card = playB) 
        #playA <- filter(eval_handA, penalty == max(penalty))$card[1]
        # actionReward
        this_state <- cards2state(handA = handA, pinta_suit = pinta_suit, known_cards = known_cards, turn = act, play_first = "B")
        max_reward <- -100
        playA <- handA[1]
        print("A responds:")
        for (c in handA) {
          thisReward <- actionReward(state = this_state, action = c, played_card = playB)$Reward
          print(paste0(c," ",thisReward))
          if (thisReward > max_reward) {
            max_reward <- thisReward
            playA <- c
          }
        }
      } else playA <- sample(handA,1)
      
      suitA <- str_split(playA,"_")[[1]][1]
      numberA <- str_split(playA,"_")[[1]][2]
      valueA <- filter(cards_df, card == playA)$value
      handA <- handA[-which(handA == playA)]
      known_cards <- c(known_cards,playA,playB)
    }
    known_cards <- unique(known_cards)
    data_rele$Details[act] <- paste(data_rele$Details[act],paste0("playA_",playA),paste0("playB_",playB), collapse = ";")
    
    if (verbose) {
      print(eval_handA)
      print(eval_handB)
    }
    #
    if (winA) {
      if (suitA == suitB) { # same suit
        if (order(numberA) < order(numberB)) {
          pointsA <- pointsA + valueA + valueB
          winA <- TRUE
        } else {
          pointsB <- pointsB + valueA + valueB
          winA <- FALSE
        }
      } else {
        if (suitA == pinta_suit) { # different suit
          pointsA <- pointsA + valueA + valueB
          winA <- TRUE
        } else if (suitB == pinta_suit) { # different suit
          pointsB <- pointsB + valueA + valueB
          winA <- FALSE
        } else {
          pointsA <- pointsA + valueA + valueB
          winA <- TRUE
        }
      }
    } else {
      if (suitA == suitB) { # same suit
        if (order(numberB) < order(numberA)) {
          pointsB <- pointsB + valueA + valueB
          winA <- FALSE
        } else {
          pointsA <- pointsA + valueA + valueB
          winA <- TRUE
        }
      } else {
        if (suitB == pinta_suit) { # different suit
          pointsB <- pointsB + valueA + valueB
          winA <- FALSE
        } else if (suitA == pinta_suit) { # different suit
          pointsA <- pointsA + valueA + valueB
          winA <- TRUE
        } else {
          pointsB <- pointsB + valueA + valueB
          winA <- FALSE
        }
      }
    }
    #
    if (verbose) print(paste0("playA: ",playA," (",pointsA,") -- playB: ",playB," (",pointsB,")"))
    #
    # rule 4: Draw card
    if (winA) {
      prev_hand_winner <- "A"
      drawA <- sample(deck_left,1)
      deck_left <- deck_left[-which(deck_left == drawA)]
      handA <- c(handA, drawA)
      if (length(deck_left)==0) { # last card drawn. Player B takes the pinta
        handB <- c(handB, pinta)
      } else {
        drawB <- sample(deck_left,1)
        deck_left <- deck_left[-which(deck_left == drawB)]
        handB <- c(handB, drawB)
      }
    } else {
      prev_hand_winner <- "B"
      drawB <- sample(deck_left,1)
      deck_left <- deck_left[-which(deck_left == drawB)]
      handB <- c(handB, drawB)
      if (length(deck_left)==0) { # last card drawn. Player B takes the pinta
        handA <- c(handA, pinta)
      } else {
        drawA <- sample(deck_left,1)
        deck_left <- deck_left[-which(deck_left == drawA)]
        handA <- c(handA, drawA)
      }
    }
    #
    cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", "")) %>%
      mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State)) %>%
      mutate(State = ifelse(card %in% known_cards, paste0(State,"K"), State))
    data_rele$NewState[act] <- paste0(paste(cards_state$State, collapse = ","),",",act+1,",W",prev_hand_winner)
    data_rele$Action[act] <- playA
    data_rele$Reward[act] <- pointsA - pointsB
    act <- act + 1
    data_rele[act,]$State <- data_rele$NewState[act-1]
    data_rele$Details[act] <- paste("",paste0("drawA_",drawA),paste0("drawB_",drawB), collapse = ";")
    data_rele$HandA[act] <- paste(handA, collapse = ",")
    data_rele$HandB[act] <- paste(handB, collapse = ",")
    #
    #continue <- readline(prompt = "Continue? y/n")
    #if (continue == "y") keep_on <- TRUE else keep_on <- FALSE
    if (verbose) print(paste0("playerA draws ",drawA," Player B draws ",drawB))
    #
    total_pointsA <- total_pointsA + pointsA
    total_pointsB <- total_pointsB + pointsB
    if (verbose) print(known_cards)
  }
  #print(paste0("player A: ", pointsA, " - player B: ", pointsB, ". Winner A: ",winA))
  #
  ############################################
  ############################################
  #
  # Final stage of the game
  print("---------- Final stage ------------")
  winA2 <- winA # A win boolean for final stage of the game
  while (length(handA) > 0) {
    pointsA <- 0
    pointsB <- 0
    if (winA2) { # if player A won the last hand
      #
      if (smartPlay) {
        #eval_handA <- smart_pick2(handA, known_cards, pinta_suit, playFirst = TRUE) 
        #playA <- filter(eval_handA, penalty == max(penalty))$card[1]
        # actionReward
        play_first_aux <- "A" # in reality play_first is B but here B acts as A
        known_cards_aux <- unique(known_cards[-which(handB %in% known_cards)]) # There are no more cards in the deck so all cards are known to this player
        this_state <- cards2state(handA = handA, pinta_suit = pinta_suit, known_cards = known_cards_aux, turn = act, play_first = play_first_aux)
        max_reward <- -100
        playA <- handA[1]
        print("A plays first:")
        for (c in handA) {
          thisReward <- actionReward(state = this_state, action = c)$Reward
          print(paste0(c," ",thisReward))
          if (thisReward > max_reward) {
            max_reward <- thisReward
            playA <- c
          }
        }
      } else playA <- sample(handA,1)
      
      suitA <- str_split(playA,"_")[[1]][1]
      numberA <- str_split(playA,"_")[[1]][2]
      valueA <- filter(cards_df, card == playA)$value
      handA <- handA[-which(handA == playA)]
      # Player B must follow suit or play pinta card
      if (grepl(suitA,paste(handB,collapse = " "))) { # player B can follow suit
        suitedB <- handB[which(grepl(suitA,handB))]
        playablesB <- data.frame(card = suitedB, stringsAsFactors = FALSE) %>%
          group_by(card) %>%
          mutate(order = order(str_split(card,"_")[[1]][2])) %>%
          ungroup()
        max_card <- filter(playablesB, order == min(order))
        if (max_card$order < order(numberA)) {
          playB <- max_card$card
          valueB <- filter(cards_df, card == playB)$value
          pointsB <- pointsB + valueA + valueB
          winA2 <- FALSE # player B wins this hand
          print("B responds:")
          print(paste0(playB, " ",pointsB))
        } else {
          #
          if (smartPlay) {
            #eval_handB <- smart_pick2(playablesB$card, known_cards, pinta_suit, playFirst = FALSE, played_card = playA) 
            #playB <- filter(eval_handB, penalty == max(penalty))$card[1]
            # actionReward
            play_first_aux <- "B" # in reality play_first is B but here B acts as A
            known_cards_aux <- unique(known_cards[-which(handA %in% known_cards)]) # There are no more cards in the deck so all cards are known to this player
            this_state <- cards2state(handA = handB, pinta_suit = pinta_suit, known_cards = known_cards_aux, turn = act, play_first = play_first_aux)
            max_reward <- -100
            playB <- handB[1]
            print("B responds:")
            for (c in handB) {
              thisReward <- actionReward(state = this_state, action = c)$Reward
              print(paste0(c," ",thisReward))
              if (thisReward > max_reward) {
                max_reward <- thisReward
                playB <- c
              }
            }
          } else playB <- sample(playablesB$card,1)
          
          valueB <- filter(cards_df, card == playB)$value
          pointsA <- pointsA + valueA + valueB
          winA2 <- TRUE
        }
      } else if (grepl(pinta_suit,paste(handB,collapse = " "))) { # player B has pinta cards
        pintasB <- handB[which(grepl(pinta_suit,handB))]
        playablesB <- data.frame(card = pintasB, stringsAsFactors = FALSE) %>%
          group_by(card) %>%
          mutate(order = order(str_split(card,"_")[[1]][2])) %>%
          ungroup() 
        #
        if (smartPlay) {
          #eval_handB <- smart_pick2(playablesB$card, known_cards, pinta_suit, playFirst = FALSE, played_card = playA) 
          #playB <- filter(eval_handB, penalty == max(penalty))$card[1]
          # actionReward
          play_first_aux <- "B" # in reality play_first is B but here B acts as A
          known_cards_aux <- unique(known_cards[-which(handA %in% known_cards)]) # There are no more cards in the deck so all cards are known to this player
          this_state <- cards2state(handA = handB, pinta_suit = pinta_suit, known_cards = known_cards_aux, turn = act, play_first = play_first_aux)
          max_reward <- -100
          playB <- handB[1]
          print("B responds:")
          for (c in playablesB$card) {
            thisReward <- actionReward(state = this_state, action = c)$Reward
            print(paste0(c," ",thisReward))
            if (thisReward > max_reward) {
              max_reward <- thisReward
              playB <- c
            }
          }
        } else playB <- sample(playablesB$card,1)
        
        valueB <- filter(cards_df, card == playB)$value
        pointsB <- pointsB + valueA + valueB
        winA2 <- FALSE
      } else { # can't follow suit and don't have pinta card
        #
        if (smartPlay) {
          #eval_handB <- smart_pick2(handB, known_cards, pinta_suit, playFirst = FALSE, played_card = playA) 
          #playB <- filter(eval_handB, penalty == max(penalty))$card[1]
          # actionReward
          play_first_aux <- "B" # in reality play_first is B but here B acts as A
          known_cards_aux <- unique(known_cards[-which(handA %in% known_cards)]) # There are no more cards in the deck so all cards are known to this player
          this_state <- cards2state(handA = handB, pinta_suit = pinta_suit, known_cards = known_cards_aux, turn = act, play_first = play_first_aux)
          max_reward <- -100
          playB <- handB[1]
          print("B responds:")
          for (c in handB) {
            thisReward <- actionReward(state = this_state, action = c)$Reward
            print(paste0(c," ",thisReward))
            if (thisReward > max_reward) {
              max_reward <- thisReward
              playB <- c
            }
          }
        } else playB <- sample(handB,1)
        
        valueB <- filter(cards_df, card == playB)$value
        pointsA <- pointsA + valueA + valueB
        winA2 <- TRUE
      }
      handB <- handB[-which(handB == playB)]
      # player B won the last hand  
    } else { # winA2 = FALSE
      #
      if (smartPlay) {
        #eval_handB <- smart_pick2(handB, known_cards, pinta_suit, playFirst = TRUE) 
        #playB <- filter(eval_handB, penalty == max(penalty))$card[1]
        # actionReward
        play_first_aux <- "A" # in reality play_first is B but here B acts as A
        known_cards_aux <- unique(known_cards[-which(handA %in% known_cards)]) # There are no more cards in the deck so all cards are known to this player
        # got to make sure handA coincides with unknown cards (cantes may have happened during phase 1)
        this_state <- cards2state(handA = handB, pinta_suit = pinta_suit, known_cards = known_cards_aux, turn = act, play_first = play_first_aux)
        max_reward <- -100
        playB <- handB[1]
        print("B plays first:")
        for (c in handB) {
          thisReward <- actionReward(state = this_state, action = c)$Reward
          print(paste0(c," ",thisReward))
          if (thisReward > max_reward) {
            max_reward <- thisReward
            playB <- c
          }
        }
      } else playB <- sample(handB,1)
      
      suitB <- str_split(playB,"_")[[1]][1]
      numberB <- str_split(playB,"_")[[1]][2]
      valueB <- filter(cards_df, card == playB)$value
      handB <- handB[-which(handB == playB)]
      # Player A must follow suit or play pinta card
      if (grepl(suitB,paste(handA,collapse = " "))) { # player A can follow suit
        suitedA <- handA[which(grepl(suitB,handA))]
        playablesA <- data.frame(card = suitedA, stringsAsFactors = FALSE) %>%
          group_by(card) %>%
          mutate(order = order(str_split(card,"_")[[1]][2])) %>%
          ungroup()
        max_card <- filter(playablesA, order == min(order))
        if (max_card$order < order(numberB)) {
          playA <- max_card$card
          valueA <- filter(cards_df, card == playA)$value
          pointsA <- pointsA + valueA + valueB
          winA2 <- TRUE
          print("A responds:")
          print(paste0(playA, " ",pointsA))
        } else {
          #
          if (smartPlay) {
            #eval_handA <- smart_pick2(playablesA$card, known_cards, pinta_suit, playFirst = FALSE, played_card = playB) 
            #playA <- filter(eval_handA, penalty == max(penalty))$card[1]
            # actionReward
            play_first_aux <- "B" # in reality play_first is B but here B acts as A
            known_cards_aux <- unique(known_cards[-which(handB %in% known_cards)]) # There are no more cards in the deck so all cards are known to this player
            this_state <- cards2state(handA = handA, pinta_suit = pinta_suit, known_cards = known_cards_aux, turn = act, play_first = play_first_aux)
            max_reward <- -100
            playA <- handA[1]
            print("A responds:")
            for (c in handA) {
              thisReward <- actionReward(state = this_state, action = c, played_card = playB)$Reward
              print(paste0(c," ",thisReward))
              if (thisReward > max_reward) {
                max_reward <- thisReward
                playA <- c
              }
            }
          } else playA <- sample(playablesA$card,1)
          
          valueA <- filter(cards_df, card == playA)$value
          pointsB <- pointsB + valueA + valueB
          winA2 <- FALSE
        }
      } else if (grepl(pinta_suit,paste(handA,collapse = " "))) { # player A has pinta cards
        pintasA <- handA[which(grepl(pinta_suit,handA))]
        playablesA <- data.frame(card = pintasA, stringsAsFactors = FALSE) %>%
          group_by(card) %>%
          mutate(order = order(str_split(card,"_")[[1]][2])) %>%
          ungroup() 
        #
        if (smartPlay) {
          #eval_handA <- smart_pick2(playablesA$card, known_cards, pinta_suit, playFirst = FALSE, played_card = playB) 
          #playA <- filter(eval_handA, penalty == max(penalty))$card[1]
          # actionReward
          play_first_aux <- "B" # in reality play_first is B but here B acts as A
          known_cards_aux <- unique(known_cards[-which(handB %in% known_cards)]) # There are no more cards in the deck so all cards are known to this player
          this_state <- cards2state(handA = handA, pinta_suit = pinta_suit, known_cards = known_cards_aux, turn = act, play_first = play_first_aux)
          max_reward <- -100
          playA <- handA[1]
          print("A responds:")
          for (c in playablesA$card) {
            thisReward <- actionReward(state = this_state, action = c, played_card = playB)$Reward
            print(paste0(c," ",thisReward))
            if (thisReward > max_reward) {
              max_reward <- thisReward
              playA <- c
            }
          }
        } else playA <- sample(playablesA$card,1)
        
        valueA <- filter(cards_df, card == playA)$value
        pointsA <- pointsA + valueA + valueB
        winA2 <- TRUE
      } else { # can't follow suit and don't have pinta card
        #
        if (smartPlay) {
          #eval_handA <- smart_pick2(handA, known_cards, pinta_suit, playFirst = FALSE, played_card = playB)
          #playA <- filter(eval_handA, penalty == max(penalty))$card[1]
          # actionReward
          play_first_aux <- "B" # in reality play_first is B but here B acts as A
          known_cards_aux <- unique(known_cards[-which(handB %in% known_cards)]) # There are no more cards in the deck so all cards are known to this player
          this_state <- cards2state(handA = handA, pinta_suit = pinta_suit, known_cards = known_cards, turn = act, play_first = play_first_aux)
          max_reward <- -100
          playA <- handA[1]
          print("A responds:")
          for (c in handA) {
            thisReward <- actionReward(state = this_state, action = c, played_card = playB)$Reward
            print(paste0(c," ",thisReward))
            if (thisReward > max_reward) {
              max_reward <- thisReward
              playA <- c
            }
          }
        } else playA <- sample(handA,1)
        valueA <- filter(cards_df, card == playA)$value
        pointsB <- pointsB + valueA + valueB
        winA2 <- FALSE
      }
      handA <- handA[-which(handA == playA)]
    }
    if (winA2) prev_hand_winner <- "A" else prev_hand_winner <- "B"
    cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
      mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State))
    data_rele$NewState[act] <- paste0(paste(cards_state$State, collapse = ","),",",act+1,",W",prev_hand_winner)
    data_rele$Action[act] <- playA
    data_rele$Reward[act] <- pointsA - pointsB
    data_rele$Details[act] <- paste(data_rele$Details[act], paste0("playA_",playA), paste0("playB_",playB), collapse = ";")
    if (act < 20) {
      act <- act + 1
      data_rele[act,]$State <- data_rele$NewState[act-1]
      data_rele$Details[act] <- ""
      data_rele$HandA[act] <- paste(handA, collapse = ",")
      data_rele$HandB[act] <- paste(handB, collapse = ",")
    }
    if (verbose) {
      print(paste0("playA: ",playA," playB: ",playB))
      print(paste0("player A: ", pointsA, " - player B: ", pointsB, ". Winner A: ",winA2))
    }
  }
  #
  # Final points: 10 de monte
  if (winA2) total_pointsA <- total_pointsA + 10 else total_pointsB <- total_pointsB + 10
  cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
    mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State))
  #data_rele$NewState[act] <- paste(cards_state$State, collapse = ",")
  #data_rele$Action[act] <- "monte"
  if (winA2) {
    data_rele$Reward[act] <- data_rele$Reward[act] + 10 
    data_rele$Details[act] <- paste(data_rele$Details[act], "monte_A", collapse = ";")
  } else {
    data_rele$Reward[act] <- data_rele$Reward[act] - 10 
    data_rele$Details[act] <- paste(data_rele$Details[act], "monte_B", collapse = ";")
  }
  #
  total_pointsA <- total_pointsA + pointsA
  total_pointsB <- total_pointsB + pointsB
  if (verbose) print(paste0("Final score> Player A: ",total_pointsA, " Player B: ",total_pointsB))
  
  #return(c(pointsA,pointsB))
  return(data_rele)
  
}
#
# Action reward for player A. 
# If player A responds to a card played by player B then playB = "played_card"
# In this case both playA = action and playB are known
actionReward <- function(state, action, played_card = NULL) {
  
  suits <- c("oros","copas","espadas","bastos") 
  # state <- this_game$State[2]
  input_cards <- state2cards(state)
  handA <- input_cards$handA
  # action <- handA[5]
  playA <- action
  #
  if (!(playA %in% handA)) { # unauthorized move: action card not in handA
    reward <- -100
    next_state <- state
    out <- list(NextState = next_state, Reward = reward)
  } else { # authorized move
    pinta_suit <- input_cards$pinta
    known_cards <- input_cards$known_cards
    known_cards <- unique(c(known_cards,playA))
    unknown <- filter(cards_df, !(card %in% c(handA,known_cards)))
    turn <- input_cards$turn
    play_first <- gsub("W","",input_cards$play_first)
    #
    # compute reward for a given action: Consider tutes, cantes, etc.
    suitA <- card_suit(playA)
    numberA <- card_number(playA)
    valueA <- card_value(playA)
    handA <- handA[-which(handA == playA)]
    #
    # For player A, I have complete information on tutes/cantes
    lost_cantes_suits <- as.character(unique(card_suit.vector(known_cards[which(grepl("caballo|rey",known_cards))]))) # suits for which cantes are not available
    tuteA <- t(data.frame(str_split(handA, "_"))) %>%
      data.frame(stringsAsFactors = FALSE)
    if (ncol(tuteA)>0) {
      tuteA <- tuteA %>%
        rename(suit = X1, number = X2) %>%
        filter(!(suit %in% lost_cantes_suits)) %>% # don't consider cantes already scored or imposible to score
        mutate(tute = ifelse(number %in% c("caballo","rey"),1,0)) %>%
        group_by(suit) %>%
        summarise(tute=sum(tute)) %>%
        ungroup() %>%
        filter(tute >=2) %>%
        mutate(tute = ifelse(suit == pinta_suit,tute*2,tute)) %>%
        arrange(desc(tute))
    }
    #
    if (!is.null(played_card) & (play_first == "B")) N <- 1 else N <- nrow(unknown)
    reward_df <- data.frame(cardA = rep(playA,N),cardB = rep("",N),reward = rep(0,N), stringsAsFactors = FALSE)
    if (turn < 15) { # Initial stage of the game. No need to follow through card suit
      # for all possible ways player B can respond to "action" card played by A, calculate reward for A.
      # return the card B plays that maximizes his reward (thus, minimizes player A reward) 
      # and update new_state with action and card played by B
      if (!is.null(played_card) & (play_first == "B")) { # player A responds to card played by B
        pointsA <- pointsB <- 0
        #playB <- unknown$card[1]
        # compute expected reward when player A plays "action" and player B plays "playB"
        playB <- played_card
        suitB <- card_suit(playB)
        numberB <- card_number(playB)
        valueB <- card_value(playB)
        known_cards_aux <- unique(c(known_cards,playB))
        if (suitA == suitB) { # same suit
          if (order(numberB) < order(numberA)) {
            pointsB <- pointsB + valueA + valueB
            winA <- FALSE
          } else {
            pointsA <- pointsA + valueA + valueB
            winA <- TRUE
          }
        } else {
          if (suitB == pinta_suit) { # different suit
            pointsB <- pointsB + valueA + valueB
            winA <- FALSE
          } else if (suitA == pinta_suit) { # different suit
            pointsA <- pointsA + valueA + valueB
            winA <- TRUE
          } else {
            pointsB <- pointsB + valueA + valueB
            winA <- FALSE
          }
        }
        # add cantes or tutes expected reward:
        # check for tute
        if (winA) { # only if A wins this hand can claim tute/cante
          if (sum(str_count(handA, "caballo"))==4 | sum(str_count(handA, "rey"))==4) {
            pointsA <- 200
          }
          # else check for cantes
          if (nrow(tuteA)>0) {
            known_cards_aux <- c(known_cards_aux, paste0(tuteA$suit[1],"_caballo"),paste0(tuteA$suit[1],"_rey"))
          }
        } else { # only if B wins
          ## For player B, I compute probabilities of cantes and tutes given playB and unknown cards
          unknown_aux <- filter(unknown, !(card == playB))
          # cantes
          for (s in suits[which(!(suits %in% lost_cantes_suits))]) {
            if ((paste0(s,"_caballo") %in% unknown_aux$card) & (paste0(s,"_rey") %in% unknown_aux$card)) { # potential cante
              prob_cante <- dhyper(x = 2, m = 2, n = nrow(unknown_aux)-2, k = 5) # prob of player B holding this suit's cante in his hand
              if (s == pinta_suit) pointsB <- pointsB + prob_cante*40 else pointsB <- pointsB + prob_cante*20
            }
          }
          # tutes
          for (figure in c("caballo","rey")) {
            if (sum(str_count(unknown_aux$card,figure)) == 4) { # tute is available
              prob_tute <- dhyper(x = 4, m = 4, n = nrow(unknown_aux)-4, k = 5) # conditional prob of player B having winning cards besides the cante cards (sample = 6-2 = 4)
              pointsB <- pointsB + prob_tute*200
            }
          }
        }
        reward_df$cardB[1] <- playB
        reward_df$reward[1] <- pointsA - pointsB
        
      } else { # player A plays first
        iter <- 1
        for (playB in unknown$card) {
          pointsA <- pointsB <- 0
          #playB <- unknown$card[1]
          # compute expected reward when player A plays "action" and player B plays "playB"
          suitB <- card_suit(playB)
          numberB <- card_number(playB)
          valueB <- card_value(playB)
          known_cards_aux <- unique(c(known_cards,playB))
          #
          if (play_first == "A") { # player A takes 1st turn
            if (suitA == suitB) { # same suit
              if (order(numberA) < order(numberB)) {
                pointsA <- pointsA + valueA + valueB
                winA <- TRUE
              } else {
                pointsB <- pointsB + valueA + valueB
                winA <- FALSE
              }
            } else {
              if (suitA == pinta_suit) { # different suit
                pointsA <- pointsA + valueA + valueB
                winA <- TRUE
              } else if (suitB == pinta_suit) { # different suit
                pointsB <- pointsB + valueA + valueB
                winA <- FALSE
              } else {
                pointsA <- pointsA + valueA + valueB
                winA <- TRUE
              }
            }
          } else { # player B plays first
            if (suitA == suitB) { # same suit
              if (order(numberB) < order(numberA)) {
                pointsB <- pointsB + valueA + valueB
                winA <- FALSE
              } else {
                pointsA <- pointsA + valueA + valueB
                winA <- TRUE
              }
            } else {
              if (suitB == pinta_suit) { # different suit
                pointsB <- pointsB + valueA + valueB
                winA <- FALSE
              } else if (suitA == pinta_suit) { # different suit
                pointsA <- pointsA + valueA + valueB
                winA <- TRUE
              } else {
                pointsB <- pointsB + valueA + valueB
                winA <- FALSE
              }
            }
          }  
          # add cantes or tutes expected reward:
          # check for tute
          if (winA) { # only if A wins this hand can claim tute/cante
            if (sum(str_count(handA, "caballo"))==4 | sum(str_count(handA, "rey"))==4) {
              pointsA <- 200
            }
            # else check for cantes
            if (nrow(tuteA)>0) {
              known_cards_aux <- c(known_cards_aux, paste0(tuteA$suit[1],"_caballo"),paste0(tuteA$suit[1],"_rey"))
            }
          } else { # only if B wins
            ## For player B, I compute probabilities of cantes and tutes given playB and unknown cards
            unknown_aux <- filter(unknown, !(card == playB))
            # cantes
            for (s in suits[which(!(suits %in% lost_cantes_suits))]) {
              if ((paste0(s,"_caballo") %in% unknown_aux$card) & (paste0(s,"_rey") %in% unknown_aux$card)) { # potential cante
                prob_cante <- dhyper(x = 2, m = 2, n = nrow(unknown_aux)-2, k = 5) # prob of player B holding this suit's cante in his hand
                if (s == pinta_suit) pointsB <- pointsB + prob_cante*40 else pointsB <- pointsB + prob_cante*20
              }
            }
            # tutes
            for (figure in c("caballo","rey")) {
              if (sum(str_count(unknown_aux$card,figure)) == 4) { # tute is available
                prob_tute <- dhyper(x = 4, m = 4, n = nrow(unknown_aux)-4, k = 5) # conditional prob of player B having winning cards besides the cante cards (sample = 6-2 = 4)
                pointsB <- pointsB + prob_tute*200
              }
            }
          }
          reward_df$cardB[iter] <- playB
          reward_df$reward[iter] <- pointsA - pointsB
          iter <- iter + 1
          
        } # end of playB loop
      } # end of player A plays first
      drawA <- sample(unknown$card,1)
      handA <- c(handA,drawA)
      unknown <- filter(unknown,  !(card == drawA))
      drawB <- sample(unknown$card,1)
      known_cards <- c(known_cards, action, playB, drawA)
      #  
    } else { # Second stage. Follow through is required. No cantes allowed anymore
      if (!is.null(played_card) & (play_first == "B")) { # player A responds to card played by B
        pointsA <- pointsB <- 0
        # compute expected reward when player A plays "action" and player B plays "playB"
        playB <- played_card
        suitB <- card_suit(playB)
        numberB <- card_number(playB)
        valueB <- card_value(playB)
        known_cards_aux <- unique(c(known_cards,playB))
        # Player A must follow suit or play pinta card
        if (suitB==suitA) { # player A can follow suit
          if (order(numberA) < order(numberB)) {
            pointsA <- pointsA + valueA + valueB
            winA <- TRUE
          } else {
            pointsB <- pointsB + valueA + valueB
            winA <- FALSE
          }
        } else if (pinta_suit == suitA) { # playB is pinta
          pointsA <- pointsA + valueA + valueB
          winA <- TRUE
        } else { # can't follow suit and don't have pinta card
          pointsB <- pointsB + valueA + valueB
          winA <- FALSE
        }
        reward_df$cardB[1] <- playB
        reward_df$reward[1] <- pointsA - pointsB
        
      } else { # player A plays first
        iter <- 1
        for (playB in unknown$card) {
          pointsA <- pointsB <- 0
          # compute expected reward when player A plays "action" and player B plays "playB"
          suitB <- card_suit(playB)
          numberB <- card_number(playB)
          valueB <- card_value(playB)
          known_cards_aux <- unique(c(known_cards,playB))
          #
          if (play_first == "A") { # player A takes 1st turn
            # Player B must follow suit or play pinta card
            if (suitA == suitB) { # player B can follow suit
              if (order(numberB) < order(numberA)) {
                pointsB <- pointsB + valueA + valueB
                winA <- FALSE # player B wins this hand
              } else {
                pointsA <- pointsA + valueA + valueB
                winA <- TRUE
              }
            } else if (pinta_suit == suitB) { # playB is pinta
              pointsB <- pointsB + valueA + valueB
              winA <- FALSE
            } else { # can't follow suit and don't have pinta card
              pointsA <- pointsA + valueA + valueB
              winA <- TRUE
            }
          } else { # player B won the last hand  
            # Player A must follow suit or play pinta card
            if (suitB==suitA) { # player A can follow suit
              if (order(numberA) < order(numberB)) {
                pointsA <- pointsA + valueA + valueB
                winA <- TRUE
              } else {
                pointsB <- pointsB + valueA + valueB
                winA <- FALSE
              }
            } else if (pinta_suit == suitA) { # playB is pinta
              pointsA <- pointsA + valueA + valueB
              winA <- TRUE
            } else { # can't follow suit and don't have pinta card
              pointsB <- pointsB + valueA + valueB
              winA <- FALSE
            }
          }
          reward_df$cardB[iter] <- playB
          reward_df$reward[iter] <- pointsA - pointsB
          iter <- iter + 1
        } # end of playB loop
      } # end of player A plays first 
      known_cards <- c(known_cards, action, playB)
    } # end of stage 2
    
    # compute Next State. Random on the player B pick? What about calling cantes, etc? Do I use a seed to replicate same outcome?
    # For every action, i.e., card played by player A, there are as many possible outcomes as unknown cards, each with different probabilites and rewards
    # Consider who played last at each state as it affects the next_state probabilities
    reward <- mean(reward_df$reward)
    # factor in 10 de monte: how many cards does this card beat? The more cards it can beat, the higher the penalty (the bigger incentive to keep it)
    if (grepl(pinta_suit, playA)) valueA <- valueA + 20
    K <- mutate(cards_df, value = ifelse(grepl(pinta_suit, card), value + 20, value)) %>%
      filter(valueA > value)
    pot_monte <- 10*nrow(K)/40
    reward <- reward - pot_monte
    #
    playB <- filter(reward_df, reward == min(reward_df$reward))$cardB[1]
    unknown <- filter(unknown,  !(card == playB))
    turn <- turn + 1
    if (winA) play_first <- "A" else play_first <- "B"
    #
    next_state <- cards2state(handA = handA, pinta_suit = pinta_suit, known_cards = known_cards, turn = turn, play_first = play_first) 
    #
    out <- list(NextState = next_state, Reward = min(reward_df$reward)-pot_monte)
    #out <- list(NextState = next_state, Reward_df = reward_df)
    #out <- list(NextState = next_state, Min_Reward = min(reward_df$reward)-pot_monte,Max_Reward = max(reward_df$reward)-pot_monte, Avg_Reward = mean(reward_df$reward)-pot_monte)
  } 
  
  return(out)
}
#
# actionReward solves the game if we calculate the optimal action as that card in playA hand that maximizes
# actionReward, i.e. A(s) = max{actionReward(s,c), c in handA}
# We can then simulate games where A plays against itself following this strategy:
play_tute2 <- function() {
  # initial deck of cards
  deck <- cards_df$card
  # deal initial hands
  handA <- sample(deck,6)
  deck_left <- deck[-which(deck %in% handA)]
  handB <- sample(deck_left,6)
  deck_left <- deck_left[-which(deck_left %in% handB)]
  pinta <- sample(deck_left,1)
  deck_left <- deck_left[-which(deck_left %in% pinta)]
  #
  # start play: Loop while deck_left is not empty
  total_pointsA <- 0
  total_pointsB <- 0
  #
  pinta_suit <- str_split(pinta,"_")[[1]][1]
  pinta_number <- str_split(pinta,"_")[[1]][2]
  pinta_value <- filter(cards_df, card == pinta)$value
  pinta2 <- paste0(pinta_suit,"_2")
  pinta7 <- paste0(pinta_suit,"_7")
  #
  winA <- TRUE # player A starts
  cantes <- c() # keep count of cantes by their suits
  known_cards <- pinta # keep count of cards played
  keep_on <- TRUE
  #
  
  
}

# Play a game of Tute and return final score for Player A and Player B (Player A always starts)
# output = 'reward' (>0 if player A wins, <0 if B wins, = 0 if tied) or 'plays' (detail of each of 20 plays)
play_tute <- function(p1_epsilon = 0.5, p2_epsilon = 0.5, output = 'plays', verbose = FALSE){
  
  num_matches <- 0 # count number of states that match existing state in dictionary
  # initial deck of cards
  deck <- cards_df$card
  # deal initial hands
  handA <- sample(deck,6)
  #handA <- c("bastos_caballo", "copas_caballo","oros_caballo","espadas_caballo","oros_4","copas_7" )
  #handA <- c("bastos_caballo", "bastos_rey","oros_4","espadas_caballo","oros_5","copas_7" )
  deck_left <- deck[-which(deck %in% handA)]
  handB <- sample(deck_left,6)
  deck_left <- deck_left[-which(deck_left %in% handB)]
  pinta <- sample(deck_left,1)
  deck_left <- deck_left[-which(deck_left %in% pinta)]
  #
  ####
  #
  # start play: Loop while deck_left is not empty
  total_pointsA <- 0
  total_pointsB <- 0
  #
  pinta_suit <- str_split(pinta,"_")[[1]][1]
  pinta_number <- str_split(pinta,"_")[[1]][2]
  pinta_value <- filter(cards_df, card == pinta)$value
  pinta2 <- paste0(pinta_suit,"_2")
  pinta7 <- paste0(pinta_suit,"_7")
  #
  winA <- TRUE # player A starts
  cantes <- c() # keep count of cantes by their suits
  known_cards <- pinta # keep count of cards played
  keep_on <- TRUE
  ##
  # initialize Re-learn actions and status
  cards_state <- mutate(cards_df, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
    mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State)) %>%
    select(-value)
  cards_stateB <- mutate(cards_df, State = ifelse(card %in% handB, "B", ifelse(card %in% known_cards, "K", ""))) %>%
    mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State)) %>%
    select(-value)
  data_rele <- data.frame(State = c(paste0(paste(cards_state$State, collapse = ","),",",1,",","WA"),rep("",19)), 
                          Action = rep("",20),
                          Reward = rep(0,20),
                          NewState = rep("",20),
                          Details = rep("",20),
                          StateB = rep("",20),
                          NewStateB = rep("",20),
                          HandA = c(paste(handA, collapse = ","),rep("",19)),
                          HandB = c(paste(handB, collapse = ","),rep("",19)),
                          stringsAsFactors = FALSE)
  # Action counter
  act <- 1
  while(length(deck_left)>0 & keep_on) {
    pointsA <- 0
    pointsB <- 0
    # Check for tute or cantes
    # If tute, game is over
    if (length(deck_left)<26) { # at least 1 hand has been played
      if (winA) {
        if (sum(str_count(handA, "caballo"))==4 | sum(str_count(handA, "rey"))==4) {
          pointsA <- 200
          data_rele$NewState[act] <- data_rele$State[act]
          #data_rele$Action[act] <- "tute" # end of game
          card_pool <- unique(filter(cards_state, grepl("A",State))$card)
          if (sum(str_count(handA, "caballo"))==4) { # pick any of the non-tute cards
            data_rele$Action[act] <- sample(card_pool[-which(grepl("caballo",card_pool))],1) 
          } else {
            data_rele$Action[act] <- sample(card_pool[-which(grepl("rey",card_pool))],1) 
          }  
          #data_rele$Reward[act] <- pointsA - pointsB
          data_rele$Reward[act-1] <- data_rele$Reward[act-1] + pointsA - pointsB # tute/cante points are assigned to last play
          data_rele$Details[act-1] <- paste(data_rele$Details[act-1],"tute_playerA", collapse = ";")
        }
        # else check for cantes
        if (nrow(tuteA)>0) {
          if (verbose) print(paste0("Player A cante in :",tuteA$suit[1]," ",10*tuteA$tute[1]))
          cantes <- c(cantes,tuteA$suit[1])
          known_cards <- c(known_cards, paste0(tuteA$suit[1],"_caballo"),paste0(tuteA$suit[1],"_rey"))
          #pointsA <- pointsA + 10*tuteA$tute[1]
          data_rele$Reward[act-1] <- data_rele$Reward[act-1] + 10*tuteA$tute[1] # tute/cante points are assigned to last play
          data_rele$Details[act-1] <- paste(data_rele$Details[act-1],paste0("canteA_",tuteA$suit[1]), collapse = ";")
          cards_state_cantes <- mutate(cards_state, State = ifelse(card %in% handA, "A", "")) %>%
            mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State)) %>%
            mutate(State = ifelse(card %in% known_cards, paste0(State,"K"), State))
          data_rele$NewState[act-1] <- paste0(paste(cards_state_cantes$State, collapse = ","),",",act,",W",prev_hand_winner)
          tuteA$tute[1] <- 0
          tuteA <- filter(tuteA, tute > 0)
          # cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
          #   mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State))
          # data_rele$NewState[act] <- paste(cards_state$State, collapse = ",")
          # data_rele$Action[act] <- paste0("cante_",tuteA$suit[1])
          # data_rele$Reward[act] <- pointsA - pointsB - data_rele$Reward[act-1]
          # act <- act + 1
          # data_rele[act,]$State <- data_rele$NewState[act-1]
        }
        
      } else {
        if (sum(str_count(handB, "caballo"))==4 | sum(str_count(handB, "rey"))==4) {
          pointsB <- 200
          data_rele$NewState[act] <- data_rele$State[act]
          #data_rele$Action[act] <- "tute" # end of game
          data_rele$Action[act] <- sample(unique(filter(cards_state, grepl("A",State))$card),1) # pick any, the game is over. PlayerB wins
          #data_rele$Reward[act] <- pointsA - pointsB
          data_rele$Reward[act-1] <- data_rele$Reward[act-1] + pointsA - pointsB # tute/cante points are assigned to last play
          data_rele$Details[act-1] <- paste(data_rele$Details[act-1],"tute_playerB", collapse = ";")
        }
        if (nrow(tuteB)>0) {
          if (verbose) print(paste0("Player B cante in :",tuteB$suit[1],10*tuteB$tute[1]))
          cantes <- c(cantes,tuteB$suit[1])
          known_cards <- c(known_cards, paste0(tuteA$suit[1],"_caballo"),paste0(tuteA$suit[1],"_rey"))
          #pointsB <- pointsB + 10*tuteB$tute[1]
          data_rele$Reward[act-1] <- data_rele$Reward[act-1] - 10*tuteB$tute[1] # tute/cante points are assigned to last play
          data_rele$Details[act-1] <- paste(data_rele$Details[act-1],paste0("canteB_",tuteB$suit[1]), collapse = ";")
          cards_state_cantes <- mutate(cards_state, State = ifelse(card %in% handA, "A", "")) %>%
            mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State)) %>%
            mutate(State = ifelse(card %in% known_cards, paste0(State,"K"), State))
          data_rele$NewState[act-1] <- paste0(paste(cards_state_cantes$State, collapse = ","),",",act,",W",prev_hand_winner)
          tuteB$tute[1] <- 0
          tuteB <- filter(tuteB, tute > 0)
          # cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
          #   mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State))
          # data_rele$NewState[act] <- paste(cards_state$State, collapse = ",")
          # data_rele$Action[act] <- paste0("cante_",tuteB$suit[1])
          # data_rele$Reward[act] <- pointsA - pointsB - data_rele$Reward[act-1]
          # act <- act + 1
          # data_rele[act,]$State <- data_rele$NewState[act-1]
        }
      }
    }
    #
    # # # rule 1: switch pinta when 2 or 7
    # if (pinta_number %in% c(2,4,5,6)) {
    #   if (pinta2 %in% handA) {
    #     handA <- handA[-which(handA == pinta2)]
    #     handA <- c(handA,pinta)
    #     pinta <- pinta2
    #     known_cards <- c(known_cards,pinta)
    #     # cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
    #     #   mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State))
    #     # data_rele$NewState[act] <- paste(cards_state$State, collapse = ",")
    #     # data_rele$Action[act] <- "pinta_2"
    #     # data_rele$Reward[act] <- 0
    #     # act <- act + 1
    #     # data_rele[act,]$State <- data_rele$NewState[act-1]
    #     data_rele$Details[act] <- paste(data_rele$Details[act],"pintaA_2", collapse = ";")
    #   }
    #   if (pinta2 %in% handB) {
    #     handB <- handB[-which(handB == pinta2)]
    #     handB <- c(handB,pinta)
    #     pinta <- pinta2
    #     known_cards <- c(known_cards,pinta)
    #     # cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
    #     #   mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State))
    #     # data_rele$NewState[act] <- paste(cards_state$State, collapse = ",")
    #     # data_rele$Action[act] <- "pinta_2"
    #     # data_rele$Reward[act] <- 0
    #     # act <- act + 1
    #     # data_rele[act,]$State <- data_rele$NewState[act-1]
    #     data_rele$Details[act] <- paste(data_rele$Details[act],"pintaB_2", collapse = ";")
    #   }
    # } else {
    #   if (pinta7 %in% handA) {
    #     handA <- handA[-which(handA == pinta7)]
    #     handA <- c(handA,pinta)
    #     pinta <- pinta7
    #     known_cards <- c(known_cards,pinta)
    #     # cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
    #     #   mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State))
    #     # data_rele$NewState[act] <- paste(cards_state$State, collapse = ",")
    #     # data_rele$Action[act] <- "pinta_7"
    #     # data_rele$Reward[act] <- 0
    #     # act <- act + 1
    #     # data_rele[act,]$State <- data_rele$NewState[act-1]
    #     data_rele$Details[act] <- paste(data_rele$Details[act],"pintaA_7", collapse = ";")
    #   }
    #   if (pinta7 %in% handB) {
    #     handB <- handB[-which(handB == pinta7)]
    #     handB <- c(handB,pinta)
    #     pinta <- pinta7
    #     known_cards <- c(known_cards,pinta)
    #     # cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
    #     #   mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State))
    #     # data_rele$NewState[act] <- paste(cards_state$State, collapse = ",")
    #     # data_rele$Action[act] <- "pinta_7"
    #     # data_rele$Reward[act] <- 0
    #     # act <- act + 1
    #     # data_rele[act,]$State <- data_rele$NewState[act-1]
    #     data_rele$Details[act] <- paste(data_rele$Details[act],"pintaB_7", collapse = ";")
    #   }
    # }
    #
    # rule 2: call tute or cante
    tuteA <- t(data.frame(str_split(handA, "_"))) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      rename(suit = X1, number = X2) %>%
      filter(!(suit %in% cantes)) %>% # don't consider cantes already scored
      mutate(tute = ifelse(number %in% c("caballo","rey"),1,0)) %>%
      group_by(suit) %>%
      summarise(tute=sum(tute)) %>%
      ungroup() %>%
      filter(tute >=2) %>%
      mutate(tute = ifelse(suit == pinta_suit,tute*2,tute)) %>%
      arrange(desc(tute))
    tuteB <- t(data.frame(str_split(handB, "_"))) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      rename(suit = X1, number = X2) %>%
      filter(!(suit %in% cantes)) %>% # don't consider cantes already scored
      mutate(tute = ifelse(number %in% c("caballo","rey"),1,0)) %>%
      group_by(suit) %>%
      summarise(tute=sum(tute)) %>%
      ungroup() %>%
      filter(tute >=2) %>%
      mutate(tute = ifelse(suit == pinta_suit,tute*2,tute)) %>%
      arrange(desc(tute))
    #
    # rule 3: Play card
    if (winA) { # A plays first
      #set.seed(123)
      this_state <- cards2state(handA = handA, pinta_suit = pinta_suit, known_cards = known_cards, turn = act, play_first = "A")
      stateA <- this_state
      if (p1_epsilon > runif(1)) {  # use max values
        max_value <- -100
        if (verbose) print("A plays first:")
        for (c in handA) {
          new_state_aux <- cards2state(handA = handA[-which(handA == c)], pinta_suit = pinta_suit, known_cards = c(known_cards,c), turn = act, play_first = "A")
          thisValue <- filter(dictionary, Sfrom == this_state, Sto == new_state_aux)$Value
          if (length(thisValue)==0) thisValue <- 0  else num_matches = num_matches + 1
            #stop(paste0("Found match: ",this_state," ",new_state_aux," ",thisValue)) # in case this state is not yet registered in the dictionary 
          if (verbose) print(paste0(c," ",thisValue))
          if (thisValue > max_value) {
            max_value <- thisValue
            playA <- c
            this_new_state <- new_state_aux
          }
        }
      } else {
        playA <- sample(handA,1)
        this_new_state <- cards2state(handA = handA[-which(handA == playA)], pinta_suit = pinta_suit, known_cards = c(known_cards,playA), turn = act, play_first = "A")
      }
      new_stateA <- this_new_state
      suitA <- str_split(playA,"_")[[1]][1]
      numberA <- str_split(playA,"_")[[1]][2]
      valueA <- filter(cards_df, card == playA)$value
      handA <- handA[-which(handA == playA)]
      known_cards <- c(known_cards,playA)
      # player B responds. Need to take into account the card played by player A
      this_state <- cards2state(handA = handB, pinta_suit = pinta_suit, known_cards = known_cards, turn = act, play_first = "A")
      stateB <- this_state
      if (p2_epsilon > runif(1)) {
        # this_state from the point of view of player B
        max_value <- -100
        playB <- handB[1]
        if (verbose) print("B responds:")
        for (c in handB) {
          new_state_aux <- cards2state(handA = handB[-which(handB == c)], pinta_suit = pinta_suit, known_cards = c(known_cards,c), turn = act, play_first = "A")
          thisValue <- filter(dictionary, Sfrom == this_state, Sto == new_state_aux)$Value
          if (length(thisValue)==0) thisValue <- 0  else num_matches = num_matches + 1 
          if (verbose) print(paste0(c," ",thisValue))
          if (thisValue > max_value) {
            max_value <- thisValue
            playB <- c
            this_new_state <- new_state_aux
          }
        }
      } else {
        playB <- sample(handB,1)
        this_new_state <- cards2state(handA = handB[-which(handB == playB)], pinta_suit = pinta_suit, known_cards = c(known_cards,playB), turn = act, play_first = "A")
      }
      #new_stateB <- gsub("A,","B,",this_new_state)
      new_stateB <- this_new_state
      handB <- handB[-which(handB == playB)]
      suitB <- str_split(playB,"_")[[1]][1]
      numberB <- str_split(playB,"_")[[1]][2]
      valueB <- filter(cards_df, card == playB)$value
      known_cards <- c(known_cards,playB)
    } else { # winA = FALSE, B plays first
      this_state <- cards2state(handA = handB, pinta_suit = pinta_suit, known_cards = known_cards, turn = act, play_first = "B")
      stateB <- this_state
      if (p2_epsilon > runif(1)) {
        max_value <- -100
        playB <- handB[1]
        if (verbose) print("B plays first:")
        for (c in handB) {
          new_state_aux <- cards2state(handA = handB[-which(handB == c)], pinta_suit = pinta_suit, known_cards = c(known_cards,c), turn = act, play_first = "B")
          thisValue <- filter(dictionary, Sfrom == this_state, Sto == new_state_aux)$Value
          if (length(thisValue)==0) thisValue <- 0  else num_matches = num_matches + 1 
          if (verbose) print(paste0(c," ",thisValue))
          if (thisValue > max_value) {
            max_value <- thisValue
            playB <- c
            this_new_state <- new_state_aux
          }
        }
      } else  {
        playB <- sample(handB,1)
        this_new_state <- cards2state(handA = handB[-which(handB == playB)], pinta_suit = pinta_suit, known_cards = c(known_cards,playB), turn = act, play_first = "B")
      }
      new_stateB <- this_new_state
      handB <- handB[-which(handB == playB)]
      suitB <- str_split(playB,"_")[[1]][1]
      numberB <- str_split(playB,"_")[[1]][2]
      valueB <- filter(cards_df, card == playB)$value
      known_cards <- c(known_cards,playB)
      #
      this_state <- cards2state(handA = handA, pinta_suit = pinta_suit, known_cards = known_cards, turn = act, play_first = "B")
      stateA <- this_state
      if (p1_epsilon > runif(1)) { # player A responds
        max_value <- -100
        playA <- handA[1]
        if (verbose) print("A responds:")
        for (c in handA) {
          new_state_aux <- cards2state(handA = handA[-which(handA == c)], pinta_suit = pinta_suit, known_cards = c(known_cards,c), turn = act, play_first = "B")
          thisValue <- filter(dictionary, Sfrom == this_state, Sto == new_state_aux)$Value
          if (length(thisValue)==0) thisValue <- 0  else num_matches = num_matches + 1 
          if (verbose) print(paste0(c," ",thisValue))
          if (thisValue > max_value) {
            max_value <- thisValue
            playA <- c
            this_new_state <- new_state_aux
          }
        }
      } else {
        playA <- sample(handA,1)
        this_new_state <- cards2state(handA = handA[-which(handA == playA)], pinta_suit = pinta_suit, known_cards = c(known_cards,playA), turn = act, play_first = "B")
      }
      new_stateA <- this_new_state
      suitA <- str_split(playA,"_")[[1]][1]
      numberA <- str_split(playA,"_")[[1]][2]
      valueA <- filter(cards_df, card == playA)$value
      handA <- handA[-which(handA == playA)]
      known_cards <- c(known_cards,playA)
    }
    known_cards <- unique(known_cards)
    data_rele$Details[act] <- paste(data_rele$Details[act],paste0("playA_",playA),paste0("playB_",playB), collapse = ";")
    data_rele$HandA[act] <- paste(handA, collapse = ",")
    data_rele$HandB[act] <- paste(handB, collapse = ",")
    stateB <- gsub("A,","B,",stateB)
    if (verbose) {
      if (verbose) print(eval_handA)
      if (verbose) print(eval_handB)
    }
    # compute points after this play and who gets to start next round
    if (winA) {
      if (suitA == suitB) { # same suit
        if (order(numberA) < order(numberB)) {
          pointsA <- pointsA + valueA + valueB
          winA <- TRUE
        } else {
          pointsB <- pointsB + valueA + valueB
          winA <- FALSE
        }
      } else {
        if (suitA == pinta_suit) { # different suit
          pointsA <- pointsA + valueA + valueB
          winA <- TRUE
        } else if (suitB == pinta_suit) { # different suit
          pointsB <- pointsB + valueA + valueB
          winA <- FALSE
        } else {
          pointsA <- pointsA + valueA + valueB
          winA <- TRUE
        }
      }
    } else {
      if (suitA == suitB) { # same suit
        if (order(numberB) < order(numberA)) {
          pointsB <- pointsB + valueA + valueB
          winA <- FALSE
        } else {
          pointsA <- pointsA + valueA + valueB
          winA <- TRUE
        }
      } else {
        if (suitB == pinta_suit) { # different suit
          pointsB <- pointsB + valueA + valueB
          winA <- FALSE
        } else if (suitA == pinta_suit) { # different suit
          pointsA <- pointsA + valueA + valueB
          winA <- TRUE
        } else {
          pointsB <- pointsB + valueA + valueB
          winA <- FALSE
        }
      }
    }
    #
    if (verbose) print(paste0("playA: ",playA," (",pointsA,") -- playB: ",playB," (",pointsB,")"))
    #
    # rule 4: Draw card
    if (winA) {
      prev_hand_winner <- "A"
      drawA <- sample(deck_left,1)
      deck_left <- deck_left[-which(deck_left == drawA)]
      handA <- c(handA, drawA)
      if (length(deck_left)==0) { # last card drawn. Player B takes the pinta
        handB <- c(handB, pinta)
      } else {
        drawB <- sample(deck_left,1)
        deck_left <- deck_left[-which(deck_left == drawB)]
        handB <- c(handB, drawB)
      }
    } else {
      prev_hand_winner <- "B"
      drawB <- sample(deck_left,1)
      deck_left <- deck_left[-which(deck_left == drawB)]
      handB <- c(handB, drawB)
      if (length(deck_left)==0) { # last card drawn. Player B takes the pinta
        handA <- c(handA, pinta)
      } else {
        drawA <- sample(deck_left,1)
        deck_left <- deck_left[-which(deck_left == drawA)]
        handA <- c(handA, drawA)
      }
    }
    data_rele$Details[act] <- paste(data_rele$Details[act],paste0("drawA_",drawA),paste0("drawB_",drawB), collapse = ";")
    #
    cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", "")) %>%
      mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State)) %>%
      mutate(State = ifelse(card %in% known_cards, paste0(State,"K"), State))
    data_rele$State[act] <- stateA
    data_rele$NewState[act] <- new_stateA
      #paste0(paste(cards_state$State, collapse = ","),",",act+1,",W",prev_hand_winner)
    data_rele$StateB[act] <- stateB
    data_rele$NewStateB[act] <- new_stateB
      #paste0(paste(cards_stateB$State, collapse = ","),",",act+1,",W",prev_hand_winner)
    data_rele$Action[act] <- playA
    data_rele$Reward[act] <- pointsA - pointsB
    act <- act + 1
    #data_rele[act,]$State <- data_rele$NewState[act-1]
    #data_rele$NewState[act-1] <- new_stateA
    #data_rele$StateB[act-1] <- stateB
    #data_rele$NewStateB[act-1] <- new_stateB
    #data_rele$Details[act] <- paste("",paste0("drawA_",drawA),paste0("drawB_",drawB), collapse = ";")
    #data_rele$HandA[act] <- paste(handA, collapse = ",")
    #data_rele$HandB[act] <- paste(handB, collapse = ",")
    #
    #continue <- readline(prompt = "Continue? y/n")
    #if (continue == "y") keep_on <- TRUE else keep_on <- FALSE
    if (verbose) print(paste0("playerA draws ",drawA," Player B draws ",drawB))
    #
    total_pointsA <- total_pointsA + pointsA
    total_pointsB <- total_pointsB + pointsB
    if (verbose) print(known_cards)
    
  } # end of this turn
  
  #
  ############################################
  ############################################
  #
  # Final stage of the game
  if (verbose) print("---------- Final stage ------------")
  winA2 <- winA # A win boolean for final stage of the game
  while (length(handA) > 0) {
    pointsA <- 0
    pointsB <- 0
    if (winA2) { # if player A won the last hand
      play_first_aux <- "A" 
      #known_cards_aux <- unique(known_cards[-which(handB %in% known_cards)]) # There are no more cards in the deck so all cards are known to this player
      this_state <- cards2state(handA = handA, pinta_suit = pinta_suit, known_cards = known_cards, turn = act, play_first = play_first_aux)
      stateA <- this_state
      if (p1_epsilon > runif(1)) {
        max_value <- -100
        if (verbose) print("A plays first:")
        for (c in handA) {
          new_state_aux <- cards2state(handA = handA[-which(handA == c)], pinta_suit = pinta_suit, known_cards = c(known_cards,c), turn = act, play_first = play_first_aux)
          thisValue <- filter(dictionary, Sfrom == this_state, Sto == new_state_aux)$Value
          if (length(thisValue)==0) thisValue <- 0  else num_matches = num_matches + 1 
          if (verbose) print(paste0(c," ",thisValue))
          if (thisValue > max_value) {
            max_value <- thisValue 
            playA <- c
            this_new_state <- new_state_aux
          }
        }
      } else {
        playA <- sample(handA,1)
        this_new_state <- cards2state(handA = handA[-which(handA == playA)], pinta_suit = pinta_suit, known_cards = c(known_cards,playA), turn = act, play_first = play_first_aux)
      }
      new_stateA <- this_new_state
      suitA <- str_split(playA,"_")[[1]][1]
      numberA <- str_split(playA,"_")[[1]][2]
      valueA <- filter(cards_df, card == playA)$value
      handA <- handA[-which(handA == playA)]
      known_cards <- c(known_cards,playA)
      #known_cards_aux <- unique(known_cards[-which(handA %in% known_cards)]) # There are no more cards in the deck so all cards are known to this player
      # player B responds. Need to take into account the card played by player A
      this_state <- cards2state(handA = handB, pinta_suit = pinta_suit, known_cards = known_cards, turn = act, play_first = play_first_aux)
      stateB <- gsub("A,","B,",this_state)
      if (grepl(suitA,paste(handB,collapse = " "))) { # player B can follow suit
        suitedB <- handB[which(grepl(suitA,handB))]
        playablesB <- data.frame(card = suitedB, stringsAsFactors = FALSE) %>%
          group_by(card) %>%
          mutate(order = order(str_split(card,"_")[[1]][2])) %>%
          ungroup()
        max_card <- filter(playablesB, order == min(order))
        if (max_card$order < order(numberA)) { # player B wins
          playB <- max_card$card
          this_new_state <- cards2state(handA = handB[-which(handB == playB)], pinta_suit = pinta_suit, known_cards = c(known_cards,playB), turn = act, play_first = play_first_aux)
          thisValue <- filter(dictionary, Sfrom == this_state, Sto == this_new_state)$Value
          valueB <- filter(cards_df, card == playB)$value
          pointsB <- pointsB + valueA + valueB
          winA2 <- FALSE # player B wins this hand
          if (verbose) print("B responds:")
          if (verbose) print(paste0(playB, " ",thisValue))
        } else { # player B loses
          #
          if (p2_epsilon > runif(1)) {
            # actionReward
            #play_first_aux <- "B" # in reality play_first is B but here B acts as A
            #known_cards_aux <- unique(known_cards[-which(handA %in% known_cards)]) # There are no more cards in the deck so all cards are known to this player
            this_state <- cards2state(handA = handB, pinta_suit = pinta_suit, known_cards = known_cards, turn = act, play_first = play_first_aux)
            max_value <- -100
            if (verbose) print("B responds:")
            for (c in handB) {
              new_state_aux <- cards2state(handA = handB[-which(handB == c)], pinta_suit = pinta_suit, known_cards = c(known_cards,c), turn = act, play_first = play_first_aux)
              thisValue <- filter(dictionary, Sfrom == this_state, Sto == new_state_aux)$Value
              if (length(thisValue)==0) thisValue <- 0  else num_matches = num_matches + 1 
              if (verbose) print(paste0(c," ",thisValue))
              if (thisValue > max_value) {
                max_value <- thisValue
                playB <- c
                this_new_state <- new_state_aux
              }
            }
          } else {
            playB <- sample(playablesB$card,1)
            this_new_state <- cards2state(handA = handB[-which(handB == playB)], pinta_suit = pinta_suit, known_cards = c(known_cards,playB), turn = act, play_first = play_first_aux)
          }
          #new_stateB <- this_new_state
          valueB <- filter(cards_df, card == playB)$value
          pointsA <- pointsA + valueA + valueB
          winA2 <- TRUE
        }
      } else if (grepl(pinta_suit,paste(handB,collapse = " "))) { # player B has pinta cards and player A didn't play pinta card so player B wins
        pintasB <- handB[which(grepl(pinta_suit,handB))]
        playablesB <- data.frame(card = pintasB, stringsAsFactors = FALSE) %>%
          group_by(card) %>%
          mutate(order = order(str_split(card,"_")[[1]][2])) %>%
          ungroup() 
        #
        if (p2_epsilon > runif(1)) {
          #known_cards_aux <- unique(known_cards[-which(handA %in% known_cards)]) # There are no more cards in the deck so all cards are known to this player
          this_state <- cards2state(handA = handB, pinta_suit = pinta_suit, known_cards = known_cards, turn = act, play_first = play_first_aux)
          max_value <- -100
          if (verbose) print("B responds:")
          for (c in playablesB$card) {
            new_state_aux <- cards2state(handA = handB[-which(handB == c)], pinta_suit = pinta_suit, known_cards = c(known_cards,c), turn = act, play_first = play_first_aux)
            thisValue <- filter(dictionary, Sfrom == this_state, Sto == new_state_aux)$Value
            if (length(thisValue)==0) thisValue <- 0  else num_matches = num_matches + 1 
            if (verbose) print(paste0(c," ",thisValue))
            if (thisValue > max_value) {
              max_value <- thisValue
              playB <- c
              this_new_state <- new_state_aux
            }
          }
        } else {
          playB <- sample(playablesB$card,1)
          this_new_state <- cards2state(handA = handB[-which(handB == playB)], pinta_suit = pinta_suit, known_cards = c(known_cards,playB), turn = act, play_first = play_first_aux)
        }
        #new_stateB <- this_new_state
        valueB <- filter(cards_df, card == playB)$value
        pointsB <- pointsB + valueA + valueB
        winA2 <- FALSE
      } else { # can't follow suit and don't have pinta card, so player B loses
        #
        if (p2_epsilon > runif(1)) {
          #known_cards_aux <- unique(known_cards[-which(handA %in% known_cards)]) # There are no more cards in the deck so all cards are known to this player
          this_state <- cards2state(handA = handB, pinta_suit = pinta_suit, known_cards = known_cards, turn = act, play_first = play_first_aux)
          max_value <- -100
          if (verbose) print("B responds:")
          for (c in handB) {
            new_state_aux <- cards2state(handA = handB[-which(handB == c)], pinta_suit = pinta_suit, known_cards = c(known_cards,c), turn = act, play_first = play_first_aux)
            thisValue <- filter(dictionary, Sfrom == this_state, Sto == new_state_aux)$Value
            if (length(thisValue)==0) thisValue <- 0  else num_matches = num_matches + 1 
            if (verbose) print(paste0(c," ",thisValue))
            if (thisValue > max_value) {
              max_value <- thisValue
              playB <- c
              this_new_state <- new_state_aux
            }
          }
        } else {
          playB <- sample(handB,1)
          this_new_state <- cards2state(handA = handB[-which(handB == playB)], pinta_suit = pinta_suit, known_cards = c(known_cards,playB), turn = act, play_first = play_first_aux)
        }
        #new_stateB <- this_new_state
        valueB <- filter(cards_df, card == playB)$value
        pointsA <- pointsA + valueA + valueB
        winA2 <- TRUE
      }
      new_stateB <- this_new_state
      handB <- handB[-which(handB == playB)]
      suitB <- str_split(playB,"_")[[1]][1]
      numberB <- str_split(playB,"_")[[1]][2]
      valueB <- filter(cards_df, card == playB)$value
      known_cards <- c(known_cards,playB)
    
      ######
      ###### player B won the last hand  
      
    } else { # winA2 = FALSE
      play_first_aux <- "B" 
      #known_cards_aux <- unique(known_cards[-which(handA %in% known_cards)]) # There are no more cards in the deck so all cards are known to this player
      this_state <- cards2state(handA = handB, pinta_suit = pinta_suit, known_cards = known_cards, turn = act, play_first = play_first_aux)
      stateB <- gsub("A,","B,",this_state)
      if (p1_epsilon > runif(1)) {
        max_value <- -100
        if (verbose) print("B plays first:")
        for (c in handB) {
          new_state_aux <- cards2state(handA = handB[-which(handB == c)], pinta_suit = pinta_suit, known_cards = c(known_cards,c), turn = act, play_first = play_first_aux)
          thisValue <- filter(dictionary, Sfrom == this_state, Sto == new_state_aux)$Value
          if (length(thisValue)==0) thisValue <- 0  else num_matches = num_matches + 1 
          if (verbose) print(paste0(c," ",thisValue))
          if (thisValue > max_value) {
            max_value <- thisValue
            playB <- c
            this_new_state <- new_state_aux
          }
        }
      } else {
        playB <- sample(handB,1)
        this_new_state <- cards2state(handA = handB[-which(handB == playB)], pinta_suit = pinta_suit, known_cards = c(known_cards,playB), turn = act, play_first = play_first_aux)
      }
      new_stateB <- this_new_state
      suitB <- str_split(playB,"_")[[1]][1]
      numberB <- str_split(playB,"_")[[1]][2]
      valueB <- filter(cards_df, card == playB)$value
      handB <- handB[-which(handB == playB)]
      known_cards <- c(known_cards,playB)
      #known_cards_aux <- unique(known_cards[-which(handB %in% known_cards)]) # There are no more cards in the deck so all cards are known to this player
      # Player A responds: must follow suit or play pinta card
      this_state <- cards2state(handA = handA, pinta_suit = pinta_suit, known_cards = known_cards, turn = act, play_first = play_first_aux)
      stateA <- this_state
      if (grepl(suitB,paste(handA,collapse = " "))) { # player A can follow suit
        suitedA <- handA[which(grepl(suitB,handA))]
        playablesA <- data.frame(card = suitedA, stringsAsFactors = FALSE) %>%
          group_by(card) %>%
          mutate(order = order(str_split(card,"_")[[1]][2])) %>%
          ungroup()
        max_card <- filter(playablesA, order == min(order))
        if (max_card$order < order(numberB)) {
          playA <- max_card$card
          known_cards <- c(known_cards,playA)
          this_new_state <- cards2state(handA = handA[-which(handA == playA)], pinta_suit = pinta_suit, known_cards = known_cards, turn = act, play_first = play_first_aux)
          thisValue <- filter(dictionary, Sfrom == this_state, Sto == this_new_state)$Value
          valueA <- filter(cards_df, card == playA)$value
          pointsA <- pointsA + valueA + valueB
          winA2 <- TRUE
          if (verbose) print("A responds:")
          if (verbose) print(paste0(playA, " ",thisValue))
        } else {
          #
          if (p2_epsilon > runif(1)) {
            #known_cards_aux <- unique(known_cards[-which(handB %in% known_cards)]) # There are no more cards in the deck so all cards are known to this player
            this_state <- cards2state(handA = handA, pinta_suit = pinta_suit, known_cards = known_cards, turn = act, play_first = play_first_aux)
            max_value <- -100
            if (verbose) print("A responds:")
            for (c in handA) {
              new_state_aux <- cards2state(handA = handA[-which(handA == c)], pinta_suit = pinta_suit, known_cards = c(known_cards,c), turn = act, play_first = play_first_aux)
              thisValue <- filter(dictionary, Sfrom == this_state, Sto == new_state_aux)$Value
              if (length(thisValue)==0) thisValue <- 0  else num_matches = num_matches + 1 
              if (verbose) print(paste0(c," ",thisValue))
              if (thisValue > max_value) {
                max_value <- thisValue
                playA <- c
                this_new_state <- new_state_aux
              }
            }
          } else {
            playA <- sample(playablesA$card,1)
            this_new_state <- cards2state(handA = handA[-which(handA == playA)], pinta_suit = pinta_suit, known_cards = c(known_cards,playA), turn = act, play_first = play_first_aux)
          }
          #new_stateA <- this_new_state
          known_cards <- c(known_cards,playA)
          valueA <- filter(cards_df, card == playA)$value
          pointsB <- pointsB + valueA + valueB
          winA2 <- FALSE
        }
      } else if (grepl(pinta_suit,paste(handA,collapse = " "))) { # player A has pinta cards
        pintasA <- handA[which(grepl(pinta_suit,handA))]
        playablesA <- data.frame(card = pintasA, stringsAsFactors = FALSE) %>%
          group_by(card) %>%
          mutate(order = order(str_split(card,"_")[[1]][2])) %>%
          ungroup() 
        #
        if (p2_epsilon > runif(1)) {
          #known_cards_aux <- unique(known_cards[-which(handB %in% known_cards)]) # There are no more cards in the deck so all cards are known to this player
          this_state <- cards2state(handA = handA, pinta_suit = pinta_suit, known_cards = known_cards, turn = act, play_first = play_first_aux)
          max_value <- -100
          if (verbose) print("A responds:")
          for (c in playablesA$card) {
            new_state_aux <- cards2state(handA = handA[-which(handA == c)], pinta_suit = pinta_suit, known_cards = c(known_cards,c), turn = act, play_first = play_first_aux)
            thisValue <- filter(dictionary, Sfrom == this_state, Sto == new_state_aux)$Value
            if (length(thisValue)==0) thisValue <- 0  else num_matches = num_matches + 1 
            if (verbose) print(paste0(c," ",thisValue))
            if (thisValue > max_value) {
              max_value <- thisValue
              playA <- c
              this_new_state <- new_state_aux
            }
          }
        } else {
          playA <- sample(playablesA$card,1)
          this_new_state <- cards2state(handA = handA[-which(handA == playA)], pinta_suit = pinta_suit, known_cards = c(known_cards,playA), turn = act, play_first = play_first_aux)
        }
        #new_stateA <- this_new_state
        known_cards <- c(known_cards,playA)
        valueA <- filter(cards_df, card == playA)$value
        pointsA <- pointsA + valueA + valueB
        winA2 <- TRUE
      } else { # can't follow suit and don't have pinta card
        #
        if (p2_epsilon > runif(1)) {
          #known_cards_aux <- unique(known_cards[-which(handB %in% known_cards)]) # There are no more cards in the deck so all cards are known to this player
          this_state <- cards2state(handA = handA, pinta_suit = pinta_suit, known_cards = known_cards, turn = act, play_first = play_first_aux)
          max_value <- -100
          if (verbose) print("A responds:")
          for (c in handA) {
            new_state_aux <- cards2state(handA = handA[-which(handA == c)], pinta_suit = pinta_suit, known_cards = c(known_cards,c), turn = act, play_first = play_first_aux)
            thisValue <- filter(dictionary, Sfrom == this_state, Sto == new_state_aux)$Value
            if (length(thisValue)==0) thisValue <- 0  else num_matches = num_matches + 1 
            if (verbose) print(paste0(c," ",thisValue))
            if (thisValue > max_value) {
              max_value <- thisValue
              playA <- c
              this_new_state <- new_state_aux
            }
          }
        } else {
          playA <- sample(handA,1)
          this_new_state <- cards2state(handA = handA[-which(handA == playA)], pinta_suit = pinta_suit, known_cards = c(known_cards,playA), turn = act, play_first = play_first_aux)
        }
        known_cards <- c(known_cards,playA)
        valueA <- filter(cards_df, card == playA)$value
        pointsB <- pointsB + valueA + valueB
        winA2 <- FALSE
      }
      new_stateA <- this_new_state
      handA <- handA[-which(handA == playA)]
      suitA <- str_split(playA,"_")[[1]][1]
      numberA <- str_split(playA,"_")[[1]][2]
      valueA <- filter(cards_df, card == playA)$value
      known_cards <- c(known_cards,playA)
      
    }
    if (winA2) prev_hand_winner <- "A" else prev_hand_winner <- "B"
    cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", "")) %>%
      mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State)) %>%
      mutate(State = ifelse(card %in% known_cards, paste0(State,"K"), State))
    data_rele$State[act] <- stateA
    data_rele$NewState[act] <- new_stateA
      #paste0(paste(cards_state$State, collapse = ","),",",act+1,",W",prev_hand_winner)
    data_rele$StateB[act] <- stateB
    data_rele$NewStateB[act] <- new_stateB
    data_rele$Action[act] <- playA
    data_rele$Reward[act] <- pointsA - pointsB
    #data_rele$HandA[act+1] <- paste(handA, collapse = ",")
    #data_rele$HandB[act+1] <- paste(handB, collapse = ",")
    if (act < 20) {
      act <- act + 1
      #data_rele[act,]$State <- data_rele$NewState[act-1]
      #data_rele$NewState[act-1] <- new_stateA
      #data_rele$StateB[act-1] <- stateB
      #data_rele$NewStateB[act-1] <- new_stateB
      #data_rele$Details[act] <- ""
      #data_rele$HandA[act] <- paste(handA, collapse = ",")
      #data_rele$HandB[act] <- paste(handB, collapse = ",")
    }
    if (verbose) {
      if (verbose) print(paste0("playA: ",playA," playB: ",playB))
      if (verbose) print(paste0("player A: ", pointsA, " - player B: ", pointsB, ". Winner A: ",winA2))
    }
  }
  #
  # Final points: 10 de monte
  if (winA2) total_pointsA <- total_pointsA + 10 else total_pointsB <- total_pointsB + 10
  cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
    mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State))
  #data_rele$NewState[act] <- paste(cards_state$State, collapse = ",")
  #data_rele$Action[act] <- "monte"
  if (winA2) {
    data_rele$Reward[act] <- data_rele$Reward[act] + 10 
    data_rele$Details[act] <- paste(data_rele$Details[act], "monte_A", collapse = ";")
  } else {
    data_rele$Reward[act] <- data_rele$Reward[act] - 10 
    data_rele$Details[act] <- paste(data_rele$Details[act], "monte_B", collapse = ";")
  }
  data_rele$StateB[act] <- stateB
  data_rele$NewStateB[act] <- new_stateB
  #
  total_pointsA <- total_pointsA + pointsA
  total_pointsB <- total_pointsB + pointsB
  if (verbose) print(paste0("Final score> Player A: ",total_pointsA, " Player B: ",total_pointsB))
  
  if (num_matches > 0) print(paste0("Dictionary States matches: ",num_matches))
  #return(c(pointsA,pointsB))
  return(data_rele)
}

### Version in R until I figure out how to properly install C++ libraries
backfeed_Reward <- function(values, reward, learning_rate, gamma) {
  
  new_values <- numeric()
  for (s in length(values):1) {
    new_value <- values[s] + learning_rate*((gamma*reward)-values[s])
    new_values <- c(new_values, new_value)
    reward <- new_value
  }
  return(new_values)
}

# Each state has 4! identical states by permutations of the 4 different suits
compute_invariants <- function(state) {
  
  #state <- this_game$State[1]
  state_raw <- str_split(state,",")[[1]]
  inv <- list()
  inv[[1]] <- state_raw[1:10]
  inv[[2]] <- state_raw[11:20]
  inv[[3]] <- state_raw[21:30]
  inv[[4]] <- state_raw[31:40]
  turn_start <- state_raw[41:42]
  
  invStates <- list()
  
  count <- 1
  for (i in 1:4) {
    for (j in c(1:4)[-i]) {
      for (k in c(1:4)[-c(i,j)]) {
        l <- c(1:4)[-c(i,j,k)]
        #print(c(i,j,k,l))
        this_inv <- c(inv[[i]],inv[[j]],inv[[k]],inv[[l]],turn_start)
        this_inv <- paste(this_inv, collapse = ",")
        invStates[[count]] <- this_inv
        count <- count + 1
      }
    }
  }
  
  return(invStates)
}

Invariant <- Vectorize(compute_invariants)

# compute player B states
state_playerB <- function(stateA, handB) {
  
  # stateA <- this_game$State[1]
  # handB <- c(str_split(this_game$HandB[1],",")[[1]])
  cardsA <- state2cards(stateA)
  stateB <- cards2state(handA = handB, pinta_suit = cardsA$pinta[1], known_cards = cardsA$known_cards, turn = cardsA$turn[1], play_first = substr(cardsA$play_first[1],2,2))
  
  return(stateB)
}

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

