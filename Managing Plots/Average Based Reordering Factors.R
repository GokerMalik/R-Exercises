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
#4- Stratify and Boxplot

#add the dollars_per_day value
gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)

#define years
years <- c(pastYear = 1970, thisYear = 2010)

######################################################
######################################################
#understanding the ReOrder
#define arbitrary categories (factors)

factors <- factor(c("Red","Red","Blue","Blue","Blue"))
levels(factors)

#put objects with values in categories
values <- c(20, 15, 21, 10, 25)

#re-order the factors regarding the average of values in
#and print the levels with level function
factorsOrd <- reorder(factors, values, FUN = mean)
levels(factorsOrd)

####################################################
####################################################

#plot the gdp per day but order the regions by their average
p <- gapminder %>%
  filter(year == years["pastYear"] & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab(label = "Regions") +
  ylab(label = "Dollars per day (log2)") +
  ggtitle("Dollars Earned Per Day by Regions (1970)") +
  scale_y_continuous(trans = "log2") +
  geom_boxplot(show.legend = TRUE) + 
  geom_point(size = 1.2, show.legend = FALSE, aes(col = continent)) +
  scale_fill_discrete(name = "Continent") +
  scale_color_discrete(name = "Continent")

#although it isn't ideal to keep the continent color for points, it has been kept
#just as an example

p