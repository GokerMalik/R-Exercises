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

# Banking Simulation

set.seed(1, sample.kind = "Rounding")

## suppose the bank gives out 1000 loans
n <- 1000

## lose in case of foreclosure
b <- -200000

# percentage_of_success
p <- 0.02

# SIMULATE the loss per year
defaults <- sample(c(0,1), n, prob=c(1-p,p), replace = TRUE)
sum(defaults*b)

# Build a monte_carlo simulation to get the distribution of loss throughout years
B<-10000

losses <- replicate(B, {
  
  defaults_per_year <- sample(c(0,1), n, prob=c(1-p, p), replace = TRUE)
  sum(defaults_per_year*b)  
  
  })


# printout a graph visualises the distribution
p1 <- data.frame(losses_in_millions = losses/10^6) %>% ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col="black")
p1

# now CALCULATE the expected loss per year
year_avg <- n*(p*b + (1-p)*0)

# CALCULATE the standard deviation in a year
year_sd <- sqrt(n)*abs(b-0)*sqrt(p*(1-p))

# CALCULATE the interest that sums the outcomes of a year to zero
#profit*(1-p) + loss(p) = 0
a <- -b*p/(1-p)



