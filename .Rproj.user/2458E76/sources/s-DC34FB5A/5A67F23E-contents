best <- function(state, outcome){
    setwd("~/Course/Assignment3")
    tab <- read.csv("outcome-of-care-measures.csv")
    s <- unique(tab[,7])
    if(!(state %in% s)){
        stop('invalid state')
    }
    j <- if(outcome == "heart attack") 11
         else if(outcome == "heart failure") 17
         else if(outcome == "pneumonia") 23
         else stop("invalid outcome")
    dat <- data.frame(Hospital.Name = tab[tab[,7] == state, 2], 
                      Death.Rate = suppressWarnings(as.numeric(tab[tab[,7] == state, j])))
    m <- min(dat[,2], na.rm = TRUE)
    name <- sort(dat[dat[,2] == m, 1])
    name[1]
}