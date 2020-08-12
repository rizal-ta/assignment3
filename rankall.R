rankall <- function(outcome, num = "best"){
    source("rankhospital.R")
    tab <- read.csv("outcome-of-care-measures.csv")
    j <- if(outcome == "heart attack") 11
    else if(outcome == "heart failure") 17
    else if(outcome == "pneumonia") 23
    else stop("invalid outcome")
    s <- sort(unique(tab[,7]))
    dat <- data.frame(state = character(), hospital.name = character())
    for(i in seq_along(s)){
        h.name <- rankhospital(s[i], outcome, num)
        dat[i,] <- c(s[i], h.name)
    }
    dat
}