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
#
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
  state_df <- filter(state_df, !(State %in% seq(1,20,1)))
  state_p <- bind_cols(cards_df, state_df)
  
  pinta <- str_split(filter(state_p, grepl("P",State))$card[1], "_")[[1]][1]
  handA <- filter(state_p, grepl("A",State))$card
  known_cards <- filter(state_p, grepl("K",State))$card
  
  return(list(handA=handA,pinta=pinta,known_cards=known_cards,turn=turn))
} 
# vectorize it
state2cards_vector <- Vectorize(state2cards)
#
# Calculate the risk of an opponent calling a cante or tute
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
play_tute <- function(smartPlay = FALSE, verbose = FALSE){
  
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
  data_rele <- data.frame(State = paste0(paste(cards_state$State, collapse = ","),",",1), 
                          Action = "",
                          Reward = 0,
                          NewState = "",
                          Details = "",
                          Hand = paste(handA, collapse = ","),
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
          data_rele$NewState[act-1] <- paste(cards_state_cantes$State, collapse = ",")
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
          data_rele$NewState[act-1] <- paste(cards_state_cantes$State, collapse = ",")
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
    # # rule 1: switch pinta when 2 or 7
    if (pinta_number %in% c(2,4,5,6)) {
      if (pinta2 %in% handA) {
        handA <- handA[-which(handA == pinta2)]
        handA <- c(handA,pinta)
        pinta <- pinta2
        known_cards <- c(known_cards,pinta)
        # cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
        #   mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State))
        # data_rele$NewState[act] <- paste(cards_state$State, collapse = ",")
        # data_rele$Action[act] <- "pinta_2"
        # data_rele$Reward[act] <- 0
        # act <- act + 1
        # data_rele[act,]$State <- data_rele$NewState[act-1]
        data_rele$Details[act] <- paste(data_rele$Details[act],"pintaA_2", collapse = ";")
      }
      if (pinta2 %in% handB) {
        handB <- handB[-which(handB == pinta2)]
        handB <- c(handB,pinta)
        pinta <- pinta2
        known_cards <- c(known_cards,pinta)
        # cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
        #   mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State))
        # data_rele$NewState[act] <- paste(cards_state$State, collapse = ",")
        # data_rele$Action[act] <- "pinta_2"
        # data_rele$Reward[act] <- 0
        # act <- act + 1
        # data_rele[act,]$State <- data_rele$NewState[act-1]
        data_rele$Details[act] <- paste(data_rele$Details[act],"pintaB_2", collapse = ";")
      }
    } else {
      if (pinta7 %in% handA) {
        handA <- handA[-which(handA == pinta7)]
        handA <- c(handA,pinta)
        pinta <- pinta7
        known_cards <- c(known_cards,pinta)
        # cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
        #   mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State))
        # data_rele$NewState[act] <- paste(cards_state$State, collapse = ",")
        # data_rele$Action[act] <- "pinta_7"
        # data_rele$Reward[act] <- 0
        # act <- act + 1
        # data_rele[act,]$State <- data_rele$NewState[act-1]
        data_rele$Details[act] <- paste(data_rele$Details[act],"pintaA_7", collapse = ";")
      }
      if (pinta7 %in% handB) {
        handB <- handB[-which(handB == pinta7)]
        handB <- c(handB,pinta)
        pinta <- pinta7
        known_cards <- c(known_cards,pinta)
        # cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
        #   mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State))
        # data_rele$NewState[act] <- paste(cards_state$State, collapse = ",")
        # data_rele$Action[act] <- "pinta_7"
        # data_rele$Reward[act] <- 0
        # act <- act + 1
        # data_rele[act,]$State <- data_rele$NewState[act-1]
        data_rele$Details[act] <- paste(data_rele$Details[act],"pintaB_7", collapse = ";")
      }
    }
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
        eval_handA <- smart_pick(handA, known_cards, pinta_suit, playFirst = TRUE) 
        playA <- filter(eval_handA, penalty == max(penalty))$card[1]
      } else playA <- sample(handA,1)
      
      suitA <- str_split(playA,"_")[[1]][1]
      numberA <- str_split(playA,"_")[[1]][2]
      valueA <- filter(cards_df, card == playA)$value
      handA <- handA[-which(handA == playA)]
      #
      if (smartPlay) {
        eval_handB <- smart_pick(handB, known_cards, pinta_suit, playFirst = FALSE, played_card = playA) 
        playB <- filter(eval_handB, penalty == max(penalty))$card[1]
      } else playB <- sample(handB,1)
      
      handB <- handB[-which(handB == playB)]
      suitB <- str_split(playB,"_")[[1]][1]
      numberB <- str_split(playB,"_")[[1]][2]
      valueB <- filter(cards_df, card == playB)$value
      known_cards <- c(known_cards,playA,playB)
    } else {
      #
      if (smartPlay) {
        eval_handB <- smart_pick(handB, known_cards, pinta_suit, playFirst = TRUE) 
        playB <- filter(eval_handB, penalty == max(penalty))$card[1]
      } else playB <- sample(handB,1)
      
      handB <- handB[-which(handB == playB)]
      suitB <- str_split(playB,"_")[[1]][1]
      numberB <- str_split(playB,"_")[[1]][2]
      valueB <- filter(cards_df, card == playB)$value
      #
      if (smartPlay) {
        eval_handA <- smart_pick(handA, known_cards, pinta_suit, playFirst = FALSE, played_card = playB) 
        playA <- filter(eval_handA, penalty == max(penalty))$card[1]
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
    data_rele$NewState[act] <- paste(cards_state$State, collapse = ",")
    data_rele$Action[act] <- playA
    data_rele$Reward[act] <- pointsA - pointsB
    act <- act + 1
    data_rele[act,]$State <- paste0(data_rele$NewState[act-1],",",act)
    data_rele$Details[act] <- paste("",paste0("drawA_",drawA),paste0("drawB_",drawB), collapse = ";")
    data_rele$Hand[act] <- paste(handA, collapse = ",")
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
  winA2 <- winA # A win boolean for final stage of the game
  while (length(handA) > 0) {
    pointsA <- 0
    pointsB <- 0
    if (winA2) { # if player A won the last hand
      #
      if (smartPlay) {
        eval_handA <- smart_pick2(handA, known_cards, pinta_suit, playFirst = TRUE) 
        playA <- filter(eval_handA, penalty == max(penalty))$card[1]
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
        } else {
          #
          if (smartPlay) {
            eval_handB <- smart_pick2(playablesB$card, known_cards, pinta_suit, playFirst = FALSE, played_card = playA) 
            playB <- filter(eval_handB, penalty == max(penalty))$card[1]
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
          eval_handB <- smart_pick2(playablesB$card, known_cards, pinta_suit, playFirst = FALSE, played_card = playA) 
          playB <- filter(eval_handB, penalty == max(penalty))$card[1]
        } else playB <- sample(playablesB$card,1)
        
        valueB <- filter(cards_df, card == playB)$value
        pointsB <- pointsB + valueA + valueB
        winA2 <- FALSE
      } else { # can't follow suit and don't have pinta card
        #
        if (smartPlay) {
          eval_handB <- smart_pick2(handB, known_cards, pinta_suit, playFirst = FALSE, played_card = playA) 
          playB <- filter(eval_handB, penalty == max(penalty))$card[1]
        } else playB <- sample(handB,1)
        
        valueB <- filter(cards_df, card == playB)$value
        pointsA <- pointsA + valueA + valueB
        winA2 <- TRUE
      }
      handB <- handB[-which(handB == playB)]
      # player B won the last hand  
    } else {
      #
      if (smartPlay) {
        eval_handB <- smart_pick2(handB, known_cards, pinta_suit, playFirst = TRUE) 
        playB <- filter(eval_handB, penalty == max(penalty))$card[1]
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
        } else {
          #
          if (smartPlay) {
            eval_handA <- smart_pick2(playablesA$card, known_cards, pinta_suit, playFirst = FALSE, played_card = playB) 
            playA <- filter(eval_handA, penalty == max(penalty))$card[1]
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
          eval_handA <- smart_pick2(playablesA$card, known_cards, pinta_suit, playFirst = FALSE, played_card = playB) 
          playA <- filter(eval_handA, penalty == max(penalty))$card[1]
        } else playA <- sample(playablesA$card,1)
        
        valueA <- filter(cards_df, card == playA)$value
        pointsA <- pointsA + valueA + valueB
        winA2 <- TRUE
      } else { # can't follow suit and don't have pinta card
        #
        if (smartPlay) {
          eval_handA <- smart_pick2(handA, known_cards, pinta_suit, playFirst = FALSE, played_card = playB)
          playA <- filter(eval_handA, penalty == max(penalty))$card[1]
        } else playA <- sample(handA,1)
        valueA <- filter(cards_df, card == playA)$value
        pointsB <- pointsB + valueA + valueB
        winA2 <- FALSE
      }
      handA <- handA[-which(handA == playA)]
    }
    cards_state <- mutate(cards_state, State = ifelse(card %in% handA, "A", ifelse(card %in% known_cards, "K", ""))) %>%
      mutate(State = ifelse(grepl(pinta_suit,card), paste0("P",State), State))
    data_rele$NewState[act] <- paste(cards_state$State, collapse = ",")
    data_rele$Action[act] <- playA
    data_rele$Reward[act] <- pointsA - pointsB
    data_rele$Details[act] <- paste(data_rele$Details[act], paste0("playA_",playA), paste0("playB_",playB), collapse = ";")
    if (act < 20) {
      act <- act + 1
      data_rele[act,]$State <- paste0(data_rele$NewState[act-1],",",act)
      data_rele$Details[act] <- ""
      data_rele$Hand[act] <- paste(handA, collapse = ",")
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
# Action reward for player A
actionReward <- function(state, action) {
  
  input_cards <- state2cards(state)
  hand <- input_cards$handA
  pinta_suit <- input_cards$pinta
  known_cards <- input_cards$known_cards
  turn <- input_cards$turn
  
  # compute reward for a given action: Consider tutes, cantes, etc.
  # action <- hand[1]
  if (turn >= 15) { # second phase of the game
    reward <- smart_pick2(hand = hand, known_cards = known_cards, pinta_suit = pinta_suit, playFirst = TRUE, played_card = NULL, actionCard = action)$penalty
  } else { # initial stage where players can call cantes, etc.
    reward <- smart_pick(hand = hand, known_cards = known_cards, pinta_suit = pinta_suit, playFirst = TRUE, played_card = NULL, actionCard = action)$penalty
  }
  
  # compute Next State. Random. Do I use a seed to replicate same outcome?
  
  
  
  out <- list(NextState = next_state, Reward = reward)
  return(out)
}
#
