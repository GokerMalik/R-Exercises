#In House Roluette chance of losing money for 500 bets

set.seed(21, sample.kind = "Rounding")

price <- 6
penalty <- -1

p_win = 5/38
p_loose <- 1-p_win

expected_one_bet <- price*p_win + penalty*p_loose
se_one_bet <- abs(price-penalty)*sqrt(p_win*p_loose)

a500_bets_avg <- expected_one_bet
se_500_bets_avg <- se_one_bet/sqrt(500)

expected_500_bets <- expected_one_bet * 500
se_500_bets <- se_one_bet*sqrt(500)

pnorm(0, expected_500_bets, se_500_bets)