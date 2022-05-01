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

#Course 2 - 1.1 Introduction to Data Visualization
#2 Introduction to Data Distributions

#function to calculate the percentage of a single pin
cumulative_distribution <- function(pin) {
  mean(heights$height <= pin)
}

# define range of 100 values from lowest value to heighest value
hist_pins <- seq(min(heights$height), max(heights$height), length = 100)

# applt the function over all the pins
pin_values <- sapply(hist_pins, cumulative_distribution)

plot(hist_pins, pin_values)