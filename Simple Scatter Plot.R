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

p1 <- murders %>% ggplot(aes(population/10^6, total, col = region)) + geom_point(size = 3) +
  geom_text(aes(label = abb), size = 3, nudge_x = 0.060, nudge_y = 0.080) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  xlab("Population in millions (log10)") +
  ylab("Total number of murders (log10)") +
  ggtitle("Gun murders in US in 2010")

p1