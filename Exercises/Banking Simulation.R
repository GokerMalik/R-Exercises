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
b <- -200000

#percentage_of_success
p <- 0.02

#SIMULATE the loss per year
defaults <- sample(c(0,1), n, prob=c(1-p,p), replace = TRUE)
sum(defaults*b)

#Build a monte_carlo simulation to get the distribution of loss throughout years
B<-10000

losses <- replicate(B, {
  
  defaults_per_year <- sample(c(0,1), n, prob=c(1-p, p), replace = TRUE)
  sum(defaults_per_year*b)  
  
  })


#printout a graph visualises the distribution
p1 <- data.frame(losses_in_millions = losses/10^6) %>% ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col="black")
p1

#now CALCULATE the expected loss per year
year_avg <- n*(p*b + (1-p)*0)

#CALCULATE the standard deviation in a year
year_sd <- sqrt(n)*abs(b-0)*sqrt(p*(1-p))

#CALCULATE the interest that sums the outcomes of a year to zero
#profit*(1-p) + loss(p) = 0
a <- -b*p/(1-p)

#The profit calculated above makes the average 0. However, chance of losing money is still 50%.
#CALCULATE how to reduce to probability of losing money to 1%

#find the value S(sum)that corresponds Pr(S<0) = 0.01
#qnorm function can be useful. However, we don't know the standard error of sums towards years yet.
#to over-come this, we can modify Pr(S<0)=0.01 to represent same value with Z-scale. We can calculate
#z by the defaults parameters of qnorm which average is zero and standard deviation is 1.

z <- qnorm(0.01) 

#modify Pr(S<0)=0.01 as follows
#PR(Z < -(a*(1-p)+b*p)*n/abs(a-b)*sqrt(n*p*(1-p)))=0.01

#this transformation explained in notion notes.

#Pull a from the equation.


#
a <- -b*(n*p - z*sqrt(n*p*(1-p))) / (n*(1-p) + z*sqrt(n*p*(1-p)))
a

