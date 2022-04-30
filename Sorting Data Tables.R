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

murders <- murders %>% mutate(rate = total/population*10^5)
murders %>% arrange(region, desc(rate))