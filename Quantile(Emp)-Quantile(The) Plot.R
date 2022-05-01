#Check if the required packages were installed and attached
packsAsked <- c("dslabs")

insDemandIndex <- !(packsAsked %in% rownames(installed.packages()))
sapply(packsAsked[insDemandIndex], install.packages)

attDemandIndex <- !(packsAsked %in% .packages())
packsToAtt <- packsAsked[attDemandIndex]

for (i in packsToAtt){
  library(i, character.only = TRUE)
}

#start coding here

# Course 2 
#1.3 Introduction to Distributions
#3- Quantile-Quantile Plots

ind = heights$sex == "Male"
mh <- heights$height[ind]

##VALUES

#define a series of percentiles
pers <- seq(0.01, 0.99, 0.01)

#fin the obsered quantiles
obs_quant <- quantile(mh, pers)

#find theoretical quantiles
the_quant <- qnorm(pers, mean(mh), sd(mh))

#define a graph compares the theoretical quantiles with observed quantiles
#with a reference line
p1 <- plot(the_quant, obs_quant) + abline(0,1)

#STANDARD UNITS

#this time find how many standard units is the value (that is assigned to the percentage)
#away from the average. The function works in the same way, but points out the standard units
obs_quantSU <- quantile(scale(mh), pers)

#find theoretical quantiles. We can use the qnorm with default variables because
#the average of the standard normal dist is 0, and sd is 1.
the_quantSU <- qnorm(pers)

#define a graph compares the theoretical quantiles with observed quantiles
#with a reference line
p2 <- plot(the_quantSU, obs_quantSU) + abline(0,1)

#either plot p1 or p2