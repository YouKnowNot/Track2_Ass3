### Finding the hspital by a specified rank in the state
# Takes three arguments. 1: outcome type (e.g. hear attack) COL #11, 17, 23
#                        2: specified rank (char or num)  
# Returns a dataframe with 2 columns: 1: name of the hospital with the specified rank
#                                     2: rate of the outcome     

rankall <- function(outcome, num ) {
    #uniform the input format to lower case (allow case flexibility)
    outcome <- tolower(outcome)

    #Read the data
    dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## check if the number and outcome are valid
    st <- dat[,7]
    st <- unique(st)
    ou <- c("heart attack", "heart failure", "pneumonia")
    
    if(!(outcome %in% ou)){
        stop("invalid outcome")
        
    }
    
    ## Read outcome data with relevant columns
    mort <- dat[,c(2,7,11,17,23)]
    
    #transform relevant columns to numeric
    cols <- c(3,4,5)
    mort[,cols]<- lapply(mort[,cols], as.numeric)
    
    # set col names to easy to read names
    colnames(mort) <- c("Hospital Name","State", "Heart Attack Rate", "Heart Failure Rate", "Pneumonia")
    
    source("evaluate_rankall.R")
    
    # Switch to check which outcome to generate 
    if(outcome=="heart attack"){
        ha <- mort[,c(1,2,3)]
        ha<- ha[order(ha$`Heart Attack Rate`, -xtfrm(ha[,1])),]
    }
    if(outcome=="heart failure"){
        ha <- mort[,c(1,2,4)]
        ha<- ha[order(ha$'Heart Failure Rate', -xtfrm(ha[,1])),]
        
    }
    if(outcome=="pneumonia"){
        ha <- mort[,c(1,2,5)]
        ha<- ha[order(ha$Pneumonia, -xtfrm(ha[,1])),]
    }
    res<-evaluate_rankall(ha, num)
    res[,c(1,2)]
}