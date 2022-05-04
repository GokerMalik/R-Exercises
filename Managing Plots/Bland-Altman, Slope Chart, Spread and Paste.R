#Check if the required packages were installed and attached
packsAsked <- c("dslabs","dplyr", "ggrepel")

insDemandIndex <- !(packsAsked %in% rownames(installed.packages()))
sapply(packsAsked[insDemandIndex], install.packages)

attDemandIndex <- !(packsAsked %in% .packages())
packsToAtt <- packsAsked[attDemandIndex]

for (i in packsToAtt){
  library(i, character.only = TRUE)
}

#start coding here

# Course 2 
#4.3 Data Visualization Principles, Part 2
#1- Slope Charts

#define some western countries
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

#define the data-set
dataSet <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

p1 <- dataSet %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         #move UK and Portugal a little to the right
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         #make the labels left or right aligned
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy")

p2 <- dataSet %>%
  mutate(year = paste0("expectancy_in_", year)) %>%
  select(country, year, life_expectancy) %>%
  #make columns representing years and spread other valuse across the years
  spread(year, life_expectancy) %>%
  mutate(average = (expectancy_in_2015 + expectancy_in_2010)/2,
         difference = expectancy_in_2015 - expectancy_in_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")

p2