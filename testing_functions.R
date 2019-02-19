## test functions
# checking probabilities and risks
this_state <- state2cards(this_game$State[1])
hand <- this_state$handA
c <- hand[3]
known_cards <- this_state$known_cards
pinta_suit <- this_state$pinta
unknown <- filter(cards_df, !(card %in% c(hand,known_cards)))
# Expected point loss
for (c in hand) {
  this_risk <- round(computeCanteRisk(play_card = c, unknown = unknown, pinta_suit = pinta_suit),3)
  this_expValue <- round(expectedValue(hand = hand, play_card = c, unknown = unknown, pinta_suit = pinta_suit),3)
  #print(paste0(c,": ",this_risk))
  #print(paste0(c,": ",this_expValue))
  print(paste0(c,": ",this_expValue, " / ",this_risk, " / ", this_expValue-this_risk))
}
# expected value from player A action
round(expectedValue(hand = hand, play_card = hand[1], unknown = unknown, pinta_suit = pinta_suit),3)
# expected value added from player B
round(expectedValueAdded(play_card = hand[1], unknown = unknown, pinta_suit = pinta_suit),3)
#
########################
