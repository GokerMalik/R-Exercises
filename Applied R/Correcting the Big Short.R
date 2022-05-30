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
#4.1 The Big Short
#2- The Big Short

#### THE WORKING THEORY####

## Amount of lending
loan <- 180000

## interest rate
ir <- 0.05 # 5%

## profit when the loan is successfull
a <- ir * loan

## lose in case of foreclosure
b <- -200000

#Run a monte-carlo simulation! This time assume that a global event can affect
#every client and change the probability of successful loans.
B<-10000

profit <- replicate(B, {
  
  #wrtie a code to change probability randomly from 0.03 to 0.05
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample(c(a, b), n, prob = c(1-new_p, new_p), replace = TRUE)
  
  sum(draws)
  
})

#the expected profit is still high
profit

#however, the chance of losing money is very high too
mean(profit<0)

#this is the correct theory to simulate the events of the Big Short