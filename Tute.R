#### Tute cards game
# simulates Tute for 2 players. 6 cards are drawn per player and a "pinta" is flipped face up to indicate
# suit that dominates. Players play and draw until the card deck is empty. Then regular Tute rules apply
# for the last 6 hands.
#
library(tidyverse)
#
#deck <- matrix(rep(0,40), nrow = 4, ncol = 10, dimnames = list(c("oros","copas","espadas","bastos"),
#                                                               c(seq(1,7,1),"sota","caballo","rey")))
#cards <- list(c("oros","copas","espadas","bastos"),c(seq(1,7,1),"sota","caballo","rey"))
#
# load helper_functions
source("tute_helper_functions.R")
#
# Define cards and values
cards_df <- define_cards()
#
cards_order <- data.frame(card = c(1,3,"rey","caballo","sota",7,6,5,4,2), order = seq(1,10,1), stringsAsFactors = FALSE)
#
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
########################################################
########################################################
#
# start play: Loop while deck_left is not empty
pointsA <- 0
pointsB <- 0
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
while(length(deck_left)>0 & keep_on) {

  #print(handA)
  #print(handB)
  
  # Check for tute or cantes
  # If tute, game is over
  if (length(deck_left)<26) { # at least 1 hand has been played
    if (winA) {
      if (sum(str_count(handA, "caballo"))==4 | sum(str_count(handA, "rey"))==4) {
        pointsA <- 500
      }
      # else check for cantes
      if (nrow(tuteA)>0) {
        print(paste0("Player A cante in :",tuteA$suit[1]," ",10*tuteA$tute[1]))
        cantes <- c(cantes,tuteA$suit[1])
        known_cards <- c(known_cards, paste0(tuteA$suit[1],"_caballo"),paste0(tuteA$suit[1],"_rey"))
        pointsA <- pointsA + 10*tuteA$tute[1]
        tuteA$tute[1] <- 0
        tuteA <- filter(tuteA, tute > 0)
      }
    } else {
      if (sum(str_count(handB, "caballo"))==4 | sum(str_count(handB, "rey"))==4) {
        pointsB <- 500
      }
      if (nrow(tuteB)>0) {
        print(paste0("Player B cante in :",tuteB$suit[1],10*tuteB$tute[1]))
        cantes <- c(cantes,tuteB$suit[1])
        known_cards <- c(known_cards, paste0(tuteA$suit[1],"_caballo"),paste0(tuteA$suit[1],"_rey"))
        pointsB <- pointsB + 10*tuteB$tute[1]
        tuteB$tute[1] <- 0
        tuteB <- filter(tuteB, tute > 0)
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
    }
    if (pinta2 %in% handB) {
      handB <- handB[-which(handB == pinta2)]
      handB <- c(handB,pinta)
      pinta <- pinta2
      known_cards <- c(known_cards,pinta)
    }
  } else {
    if (pinta7 %in% handA) {
      handA <- handA[-which(handA == pinta7)]
      handA <- c(handA,pinta)
      pinta <- pinta7
      known_cards <- c(known_cards,pinta)
    }
    if (pinta7 %in% handB) {
      handB <- handB[-which(handB == pinta7)]
      handB <- c(handB,pinta)
      pinta <- pinta7
      known_cards <- c(known_cards,pinta)
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
    #playA <- sample(handA,1) # evaluate the best available play right here
    eval_handA <- smart_pick(handA, known_cards, pinta_suit, playFirst = TRUE)
    playA <- filter(eval_handA, penalty == max(penalty))$card[1]
    suitA <- str_split(playA,"_")[[1]][1]
    numberA <- str_split(playA,"_")[[1]][2]
    valueA <- filter(cards_df, card == playA)$value
    handA <- handA[-which(handA == playA)]
    #playB <- sample(handB,1)
    eval_handB <- smart_pick(handB, known_cards, pinta_suit, playFirst = FALSE, played_card = playA)
    playB <- filter(eval_handB, penalty == max(penalty))$card[1]
    handB <- handB[-which(handB == playB)]
    suitB <- str_split(playB,"_")[[1]][1]
    numberB <- str_split(playB,"_")[[1]][2]
    valueB <- filter(cards_df, card == playB)$value
    known_cards <- c(known_cards,playA,playB)
  } else { # insert here reinforcement logic. For now, just random
    #playB <- sample(handB,1)
    eval_handB <- smart_pick(handB, known_cards, pinta_suit, playFirst = TRUE)
    playB <- filter(eval_handB, penalty == max(penalty))$card[1]
    handB <- handB[-which(handB == playB)]
    suitB <- str_split(playB,"_")[[1]][1]
    numberB <- str_split(playB,"_")[[1]][2]
    valueB <- filter(cards_df, card == playB)$value
    #playA <- sample(handA,1)
    eval_handA <- smart_pick(handA, known_cards, pinta_suit, playFirst = FALSE, played_card = playB)
    playA <- filter(eval_handA, penalty == max(penalty))$card[1]
    suitA <- str_split(playA,"_")[[1]][1]
    numberA <- str_split(playA,"_")[[1]][2]
    valueA <- filter(cards_df, card == playA)$value
    handA <- handA[-which(handA == playA)]
    known_cards <- c(known_cards,playA,playB)
  }
  known_cards <- unique(known_cards)
  print(eval_handA)
  print(eval_handB)
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
  print(paste0("playA: ",playA," (",pointsA,") -- playB: ",playB," (",pointsB,")"))
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
  #continue <- readline(prompt = "Continue? y/n")
  #if (continue == "y") keep_on <- TRUE else keep_on <- FALSE
  print(paste0("playerA draws ",drawA," Player B draws ",drawB))
  #
}
print(known_cards)
#print(paste0("player A: ", pointsA, " - player B: ", pointsB, ". Winner A: ",winA))
#
#####
#####
#
# Final stage of the game
winA2 <- winA # A win boolean for final stage of the game
while (length(handA) > 0) {
  
  if (winA2) { # if player A won the last hand
    #playA <- sample(handA,1)
    eval_handA <- smart_pick2(handA, known_cards, pinta_suit, playFirst = TRUE)
    playA <- filter(eval_handA, penalty == max(penalty))$card[1]
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
        #playB <- sample(playablesB$card,1)
        eval_handB <- smart_pick2(playablesB$card, known_cards, pinta_suit, playFirst = FALSE, played_card = playA)
        playB <- filter(eval_handB, penalty == max(penalty))$card[1]
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
      #playB <- sample(playablesB$card,1)
      eval_handB <- smart_pick2(playablesB$card, known_cards, pinta_suit, playFirst = FALSE, played_card = playA)
      playB <- filter(eval_handB, penalty == max(penalty))$card[1]
      valueB <- filter(cards_df, card == playB)$value
      pointsB <- pointsB + valueA + valueB
      winA2 <- FALSE
    } else { # can't follow suit and don't have pinta card
      #playB <- sample(handB,1)
      eval_handB <- smart_pick2(handB, known_cards, pinta_suit, playFirst = FALSE, played_card = playA)
      playB <- filter(eval_handB, penalty == max(penalty))$card[1]
      valueB <- filter(cards_df, card == playB)$value
      pointsA <- pointsA + valueA + valueB
      winA2 <- TRUE
    }
    handB <- handB[-which(handB == playB)]
  # player B won the last hand  
  } else {
    #playB <- sample(handB,1)
    eval_handB <- smart_pick2(handB, known_cards, pinta_suit, playFirst = TRUE)
    playB <- filter(eval_handB, penalty == max(penalty))$card[1]
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
        #playA <- sample(playablesA$card,1)
        eval_handA <- smart_pick2(playablesA$card, known_cards, pinta_suit, playFirst = FALSE, played_card = playB)
        playA <- filter(eval_handA, penalty == max(penalty))$card[1]
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
      #playA <- sample(playablesA$card,1)
      eval_handA <- smart_pick2(playablesA$card, known_cards, pinta_suit, playFirst = FALSE, played_card = playB)
      playA <- filter(eval_handA, penalty == max(penalty))$card[1]
      valueA <- filter(cards_df, card == playA)$value
      pointsA <- pointsA + valueA + valueB
      winA2 <- TRUE
    } else { # can't follow suit and don't have pinta card
      #playA <- sample(handA,1)
      eval_handA <- smart_pick2(handA, known_cards, pinta_suit, playFirst = FALSE, played_card = playB)
      playA <- filter(eval_handA, penalty == max(penalty))$card[1]
      valueA <- filter(cards_df, card == playA)$value
      pointsB <- pointsB + valueA + valueB
      winA2 <- FALSE
    }
    handA <- handA[-which(handA == playA)]
  }
  print(paste0("playA: ",playA," playB: ",playB))
  print(paste0("player A: ", pointsA, " - player B: ", pointsB, ". Winner A: ",winA2))
}
#
# Final points: 10 de monte
if (winA2) pointsA <- pointsA + 10 else pointsB <- pointsB + 10
print(paste0("Final score> Player A: ",pointsA, " Player B: ",pointsB))


