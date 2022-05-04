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
#7- Ecological Fallacy

#define years
years <- c(pastYear = 1970, thisYear = 2010)

#define western regions
west <- c("Western Europe","Northern Europe","Southern Europe","Northern America","Australia and New Zealand")

#define other regions
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

data1 <- gapminder %>%
  filter(year == years["thisYear"] &
           !is.na(gdp) &
           !is.na(infant_mortality) &
           !is.na(group)) %>%
  group_by(group)

r1 <- data1 %>%
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1-sum(infant_mortality*population/1000)/sum(population)) %>%
  arrange(income)

r2 <- data1 %>% filter(group == "Sub-Saharan Africa") %>% summarize("total infant lost" = sum(infant_mortality))
r3 <- data1 %>% filter(group == "Sub-Saharan Africa") %>% summarize("total population" = sum(population))

r1
r2[2]
r3[2]