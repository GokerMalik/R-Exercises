#Check if the required packages were installed and attached
packsAsked <- c("dslabs", "dplyr", "tidyverse")

insDemandIndex <- !(packsAsked %in% rownames(installed.packages()))
sapply(packsAsked[insDemandIndex], install.packages)

attDemandIndex <- !(packsAsked %in% .packages())
packsToAtt <- packsAsked[attDemandIndex]

for (i in packsToAtt){
  library(i, character.only = TRUE)
}

#start coding here

# Course 2 
#3.2 Using the Gapminder Dataset
#2- Time Series Plots

countries <- c("South Korea", "Germany")

#create plots on the same grid

#opt1
p1 <- gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year,fertility, col = country)) +
  geom_point(size = 0.85, na.rm = TRUE) +
  geom_line(na.rm = TRUE)

#opt2
p2 <- gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year,fertility, group = country)) +
  geom_point(size = 0.85, na.rm = TRUE) +
  geom_line(na.rm = TRUE)

p2