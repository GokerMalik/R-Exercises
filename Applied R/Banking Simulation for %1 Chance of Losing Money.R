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
#1- The Big Short: Interest Rates Explained

# CALCULATE how to reduce to probability of losing money to 1%

## suppose the bank gives out 1000 loans
n <- 1000

## lose in case of foreclosure
b <- -200000

# percentage_of_success
p <- 0.02

# find the value S(sum)that corresponds Pr(S<0) = 0.01
# qnorm function can be useful. However, we don't know the standard error of sums towards years yet.
# to over-come this, we can modify Pr(S<0)=0.01 to represent same value with Z-scale.
# for What value, probabilities of z-scores are less or equals to 0.001? We can calculate
# this value (say z) by the defaults parameters of qnorm which average is zero and standard deviation is 1.

z <- qnorm(0.01) 

# modify Pr(S<0)=0.01 as follows
# PR(Z < -(a*(1-p)+b*p)*n/abs(a-b)*sqrt(n*p*(1-p)))=0.01
# this transformation explained in notion notes.

# to satisfy the un-equation above, right side has to equal z (which is qnorm(0.01))
# z = -(a*(1-p)+b*p)*n/abs(a-b)*sqrt(n*p*(1-p))
# Pull a from the equation.

a <- -b*(n*p - z*sqrt(n*p*(1-p))) / (n*(1-p) + z*sqrt(n*p*(1-p)))

a/180000*100

# Now Run a Monte-Carlo Simulation to see the chance of losing money is %1

B <- 10000
profit <- replicate(B,{
  
  draws <- sample(c(a, b), n, prob=c(1-p,p), replace = TRUE)
  sum(draws)
  
})

mean(profit<0)