

                                        # split the data frame
dfm <- hrgwas

names(dfm)[3] <- "bp"

dfm.split <- split(dfm, dfm$CHR)

relpos <- function(x,minbp,maxbp,nrows,startingpoint){
    actual.distance <- x - minbp
    relative.distance <- (actual.distance*nrows)/maxbp
    return(relative.distance + startingpoint)
}


startingpoint = 1
dfm.list <- lapply(dfm.split, function(x){
    minbp <- as.numeric(min(x$bp))
    maxbp <- as.numeric(max(x$bp))
    nrows <- as.numeric(nrow(x))
    startingpoint <<- startingpoint + 1
    x$newpos <- relpos(x$bp,minbp,maxbp,nrows)
    return(x)
})

