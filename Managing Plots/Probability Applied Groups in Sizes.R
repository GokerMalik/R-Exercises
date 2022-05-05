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
#2- sapply

#write a function that calculates the probability of at least two people
#having the same birthday for a given number of group
compute_prob <- function (n, B = 10000){
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
 mean(same_day)
}

#define groups from with one member to 60 members
n <- seq(1,60)

#apply the function over each. Line temporarily commented to avoid time lost
#prob <- sapply(n, compute_prob)

#plot the results vs number of group members
#plot(n, prob)

#define the same function without the monte-carlo experiment
compute_probWMC <- function (n){
  prob_unique <- seq(365, 365-n+1)/365
  1-prod(prob_unique)
}

prob <- sapply(n, compute_prob)
lines(n, prob, col = "red")