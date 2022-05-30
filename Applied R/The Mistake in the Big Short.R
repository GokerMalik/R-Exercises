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

#### THE UNWORKING THEORY####

## Amount of lending
loan <- 180000

## interest rate
ir <- 0.05 # 5%

## profit when the loan is successfull
a <- ir * loan

## lose in case of foreclosure
b <- -200000

# percentage_of_success
p <- 0.04

# find the value that 1% of the values are equals or less in z-score
z <- qnorm(0.01)

# calculate the expected value for a single loan
loanE <- b*p + a*(1-p)

# calculate the standard error for a single loan
loanSE <- (a-b)*sqrt(p*(1-p))

# calculate the amount of loans to be given to keep the risk of money 1%.
n <- ceiling((z^2*loanSE^2)/ #The formula applied here works only for independent draws which is not the case with
               loanE^2) #the events in the Big Short.

#calculate the expected earnings
SumE <- loanE*n

#Run a monte-carlo to prove findings
B<- 10000

profit <- replicate (B, {
  
  draws <- sample(c(a, b), n, prob=c(1-p, p), replace = TRUE)
  sum(draws)
  
})

mean(profit<0)