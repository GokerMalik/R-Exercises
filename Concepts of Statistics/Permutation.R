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
set.seed(1986, sample.kind = "Rounding")

# Course 3 
#1.2 Combinations and Permutations
#1- Combinations and Permutations

#print all the way that you can pick two elements from the list of numbers from 1 to 5
couples <- permutations(5,2)

#create a list of all the possible phone numbers using each charachter only once
#below we define a sequence from 0 to 9 rather than 1 to 10
numbersWithoutRepeat <- permutations(10, 7, v = 0:9)

n <- nrow(numbersWithoutRepeat)
index <- sample(n, 5)
numbersWithoutRepeat[index,]

#generate a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spaces")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)

deck <- paste(deck$number, deck$suit)

#calculate the probability of picking a king
kings <- paste("King", suits)
mean(deck %in% kings)

#find all the possible hands
hands <- permutations (52, 2, v= deck)

#create a hand
first_card <- hands[,1]
second_card <- hands[,2]

#check how many cases the first card is a king
Cases1 <- sum(first_card %in% kings)

#check what fraction of these have king also as the second card
res1 <- sum(first_card %in% kings & second_card %in% kings) /
  sum(first_card %in% kings)

#re-wrtie the code by using mean
res2 <- mean(first_card %in% kings & second_card %in% kings) /
  mean(first_card %in% kings)

#this is because mean checks the condition and sums all the TRUES and divides
#by the length. Without dividing by the length. We can simply divide the both sides of the
#division by the lenght (or number of all the hands)

res1 == res2 #this should turn TRUE