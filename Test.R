packTest <- function (PackList){

    if (PackList %in% rownames(installed.packages()) == FALSE){
    install.packages(PackList)
    }
}

packsAsked <- c("dslabs", "tidyverse")
sapply(packsAsked, packTest)


