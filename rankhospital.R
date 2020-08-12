rankhospital <- function(state, outcome, num = "best"){
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
    o.dat <- dat[order(dat[,2], dat[,1], na.last = NA), ] 
    if(num == "best") o.dat[1,1]
    else if(num == "worst") o.dat[length(o.dat[,1]), 1]
    else if(num > length(o.dat[,1])) NA
    else o.dat[num, 1]
}