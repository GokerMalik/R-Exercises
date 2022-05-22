#Check if the required packages were installed and attached
packsAsked <- c("dplyr", "ggplot2")

insDemandIndex <- !(packsAsked %in% rownames(installed.packages()))
sapply(packsAsked[insDemandIndex], install.packages)

attDemandIndex <- !(packsAsked %in% .packages())
packsToAtt <- packsAsked[attDemandIndex]

for (i in packsToAtt){
  library(i, character.only = TRUE)
}

####start coding here####

#Banking Simulation

set.seed(1, sample.kind = "Rounding")

##suppose the bank gives out 1000 loans
n <- 1000

##lose in case of foreclosure
loss_per_foreclosure <- -200000

#percentage_of_success
p <- 0.02

#SIMULATE the loss per year
defaults <- sample(c(0,1), n, prob=c(1-p,p), replace = TRUE)
sum(defaults*loss_per_foreclosure)

#Build a monte_carlo simulation to get the distribution of loss throughout years
B<-10000

losses <- replicate(B, {
  
  defaults_per_year <- sample(c(0,1), n, prob=c(1-p, p), replace = TRUE)
  sum(defaults_per_year*loss_per_foreclosure)  
  
  })


#printout a graph visualises the distribution
p1 <- data.frame(losses_in_millions = losses/10^6) %>% ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col="black")
p1

#now CALCULATE the expected loss per year
year_avg <- n*(p*loss_per_foreclosure + (1-p)*0)

#CALCULATE the standard deviation in a year
year_sd <- sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))

#CALCULATE the interest that sums the outcomes of a year to zero
#(profit*p) + loss(1-p) = 0
profit <- -loss_per_foreclosure*p/(1-p)

#The profit calculates above makes the average 0. However, chance of losing money is still 50%
#Calculate how to drop the chance of losing money to 1% (S=sum and Pr(S<0) = 0.01)

#S = (profit*p -loss_per_closure*p)*n
#se = abs(profit - (-loss_per_closure))*sqrt(n*p(1-p))
