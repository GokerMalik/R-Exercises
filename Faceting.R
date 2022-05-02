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
#1- Faceting

ds_theme_set() #removes the greyed background

#define series of plots with facet_grid
p1 <- filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point() +
  facet_grid(continent ~ year)

#p1

#define series of plots with facet_wrap
p2 <- filter(gapminder, year %in% c(1962, 1980, 1990, 2000, 2012), continent %in% c("Asia", "Europe")) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point() +
  facet_wrap(~ year)

p2

#NOTE: When facet is used, the axis range is mutual in all the plots