#Check if the required packages were installed and attached
packsAsked <- c()

insDemandIndex <- !(packsAsked %in% rownames(installed.packages()))
sapply(packsAsked[insDemandIndex], install.packages)

attDemandIndex <- !(packsAsked %in% .packages())
packsToAtt <- packsAsked[attDemandIndex]

for (i in packsToAtt){
  library(i, character.only = TRUE)
}

#start coding here

# Course 3 
#1.1 Introduction to Discrete Probability
#2- Monte Carlo Simulations

#create an urn with 2 red beads and 3 blue beads
urn <- rep(c("red", "blue"), times = c(2,3))    # create an urn with 2 red, 3 blue

#define an event of chosing one bead
sample(urn, 1)    # sample 1 bead at random

#repet the event of chosing one bead
n <- 10000
events1 <- replicate(n, sample(urn, 1))

#save each selection in a data-table
records1 <- table(events1)    # make a table of outcome counts

#calculate the probability of events of chosing red and blue beads
prop.table(records1)    # view table of outcome proportions

#repeat the same exercise without using replicate function
events2 <- sample(urn, n, replace = TRUE)
records2 <- table(events2)
prop.table(records2)