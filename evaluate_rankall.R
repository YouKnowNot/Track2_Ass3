
evaluate_rankall <- function(x= data.frame, num){
    # Remove NA's
    f <- complete.cases(x)
    x <- x[f,]
    
    #split by state
    spl <- split(x, x$State)
    
    #create a data frame to hold th results
    l <- data.frame("Hospital name"=character() , "State"=character(), "OC"=numeric())

    #retrieve the rank for rach state
    for(i in 1:length(spl)){
        
            if(is.numeric(num)){
                num <- as.numeric(num) }
        
            if(num == "best"){
                num <- 1 }
        
            if(num == "worst"){
                num <- nrow(spl[[i]]) }
            aux <-spl[i]
            aux <- data.frame(aux)
            colnames(aux) <- c("Hospital name", "State", "OC")
            
            if(is.na(aux[num,3])){
                Hospital_name <- NA
                State <- names(spl[i])
                OC <- NA
                au <- data.frame(Hospital_name, State, OC)
                colnames(au) <- c("Hospital name", "State", "OC")
                l <- rbind(l,au)
                }
            
            else{
                l <- rbind(l,aux[num,]) 
                }
            }
    l
}
