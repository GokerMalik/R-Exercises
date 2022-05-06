#Check if the required packages were installed and attached
packsAsked <- c("dslabs", "dplyr", "tidyverse", "gridExtra")

insDemandIndex <- !(packsAsked %in% rownames(installed.packages()))
sapply(packsAsked[insDemandIndex], install.packages)

attDemandIndex <- !(packsAsked %in% .packages())
packsToAtt <- packsAsked[attDemandIndex]

for (i in packsToAtt){
  library(i, character.only = TRUE)
}

#start coding here

# Course 2 
#2.2 Introduction to Distributions
#5- Other Examples

#define histograms with male heights and female heights
p1 <- heights %>% filter(sex == "Male") %>%
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 1, fill = "blue", col = "black")

p2 <- heights %>% filter(sex == "Female") %>%
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 1, fill = "blue", col = "black")

#plot the histograms side by side with 2 columns
grid.arrange(p1,p2, ncol = 2)