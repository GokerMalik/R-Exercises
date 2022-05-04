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
p1 <- gapminder %>%
  ##below, we define new groups
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands")) %>%
  filter(year == years["thisYear"] &
           !is.na(gdp),
         !is.na(infant_mortality),
         !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365,
            #normally infant_mortality is estimated by (unsurvived_newborns)/live_births*1000
            #apparently, it was supposed below (unsurvived_newborns)/population*1000
           infant_survival_rate = 1-sum(infant_mortality/1000*population)/sum(population)) %>%
  arrange(income) %>%
  ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  #see the limit argument below and check how it is projected.
  #also, see the limit argument
  #also, see the description of the logit transformation at the end of the code
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, 0.9981),
                     breaks = c(0.85, 0.90, 0.95, 0.99, 0.995, 0.998)) +
  geom_label(size =3, show.legend = FALSE)

p1


#########LOGIT TRANSFORMATION#######
#Useful for the numbers that are near to 0 or 1.
#f(p) = log(p/(p-1)) is how to apply the logistic transformation.
#log function makes the outcome symmetric. If the rates are the same, the
#outcome is 0