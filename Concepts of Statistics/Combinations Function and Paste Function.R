#Check if the required packages were installed and attached
packsAsked <- c("gtools")

insDemandIndex <- !(packsAsked %in% rownames(installed.packages()))
sapply(packsAsked[insDemandIndex], install.packages)

attDemandIndex <- !(packsAsked %in% .packages())
packsToAtt <- packsAsked[attDemandIndex]

for (i in packsToAtt){
  library(i, character.only = TRUE)
}

#start coding here

#configure the RNG to ensure the interoperability with the course material
set.seed(1, sample.kind = "Rounding")

# Course 3 
#1.2 Combinations and Permutations
#1- Combinations and Permutations

###WHAT IT THE PROBABILITY OF HAVING NATURAL 21 IN BLACKJACK####


#generate a vector of deck which includes all the cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spaces")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)


#make a vector that includes all the aces
aces <- paste ("Ace", suits)

#make a vector that includes all the face cards
faceCards <- c("King","Queen","Jack","Ten")
faceCards <- expand.grid(number = faceCards, suit = suits)
faceCards <- paste(faceCards$number, faceCards$suit)

#list all the hands (this time the order doesn't matter so 2,1 and 1,2 are idenctical)
hands <- combinations(52, 2, v = deck)

#calculate the probability that having an Ace and a faceCard. Note that
#all the Ace Cards are listed before any face cards in Hands. That means
#and hand satisfies the condition is gonna have an ace card as the firt card
#and a face card as the second
r1 <- mean(hands[,1] %in% aces & hands[,2] %in% faceCards)

#if we didn't know that in combinations, the first column is not always the ace, we could rather use
r2 <- mean((hands[,1] %in% aces & hands[,2] %in% faceCards)|(hands[,2] %in% aces & hands[,1] %in% faceCards))

r1 == r2

#Run a Monte Carlo simulation to see if the outcome is similar
n <- 10000
results <- replicate(n, {
  hand <- sample(deck,2)
  (hand[1] %in% aces & hand[2] %in% faceCards) |
    (hand[1] %in% faceCards & hand[2] %in% aces)
})

r3 <- mean(results)
r3 - r1