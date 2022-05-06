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

#configure the RNG to ensure the interoperability with the course material
set.seed(1986, sample.kind = "Rounding")

# Course 3 
#1.2 Combinations and Permutations
#3- sapply

#define groups from with one member to 60 members
n <- seq(1,60)

#write a function that runs Monte-Carlo to estimate the probability of at least two people
#having the same birthday for a given number of group
MonteCarlo <- function (n, count = 10000){
  same_day <- replicate(count, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
 mean(same_day)
}

#apply the function over each. Line temporarily commented to avoid time lost
probMC <- sapply(n, MonteCarlo)

#define the same function without the monte-carlo experiment
compute_prob <- function (Arg){
  #probability of NOT having a birthday for a new member of n membered group
  #goes (365/365)*(364/365)*(363/365)*(362/365)....
  prob_unique <- seq(365, 365-Arg)/365
  1-prod(prob_unique)
}

prob <- sapply(n, compute_prob)

#plot MonteCarlo over calculations
plot(n, probMC)
lines(n, prob, col = "red")