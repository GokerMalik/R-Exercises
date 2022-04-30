packsAsked <- c("dslabs", "tidyverse")
demandIndex <- !(packsAsked %in% rownames(installed.packages()))
sapply((packsAsked[demandIndex]), install.packages)

