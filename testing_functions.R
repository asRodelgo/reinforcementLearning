## test functions
# checking probabilities and risks
this_state <- state2cards(this_game$State[2])
hand <- this_state$handA
c <- hand[3]
known_cards <- this_state$known_cards
pinta_suit <- this_state$pinta
unknown <- filter(cards_df, !(card %in% c(hand,known_cards)))
# Expected point loss
for (c in hand) {
  print(paste0(c,": ",round(computeCanteRisk(play_card = c, unknown = unknown, pinta_suit = pinta_suit),3)))
  print(paste0(c,": ",round(expectedValue(hand = hand, play_card = c, unknown = unknown, pinta_suit = pinta_suit),3)))
  #print(paste0(c,": ",round(expectedValueAdded(play_card = c, unknown = unknown, pinta_suit = pinta_suit),3)))
}
# expected value from player A action
round(expectedValue(hand = hand, play_card = hand[1], unknown = unknown, pinta_suit = pinta_suit),3)
# expected value added from player B
round(expectedValueAdded(play_card = hand[1], unknown = unknown, pinta_suit = pinta_suit),3)
#
########################
