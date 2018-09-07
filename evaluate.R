
evaluate <- function(x= data.frame, rank){
    # Remove NA's
    f <- complete.cases(x)
    x <- x[f,]
    if(is.numeric(rank)){
        rank <- as.numeric(rank)
        res <- x[rank, 1]
    }
    else {
        rank <- as.character(rank)
        if(rank=="worst"){
            res <- x[nrow(x),1]
        }
        if(rank=="best"){
            res <- x[1,1]
        }
    }
    res
}