#Check if the required packages were installed and attached
packsAsked <- c("dslabs", "dplyr")

insDemandIndex <- !(packsAsked %in% rownames(installed.packages()))
sapply(packsAsked[insDemandIndex], install.packages)

attDemandIndex <- !(packsAsked %in% .packages())
packsToAtt <- packsAsked[attDemandIndex]

for (i in packsToAtt){
  library(i, character.only = TRUE)
}

#start coding here

# Course 2 
#4.2 Data Visualization Principles, Part 2
#1- Show the Data

p1 <- heights %>% ggplot(aes(sex, height)) + geom_jitter(width = 0.1, alpha = 0.2)

p1