
### Finding the hspital by a specified rank in the state
# Takes three arguments. 1: 2 char abbr name of state. COL #7  2: outcome type (e.g. hear attack) COL #11, 17, 23
# 3: specified rank of the hospital
# Returns name of the institute COL #2

rankhospital <- function(state=character, outcome=character, rank){
    
    #uniform the input format to lower case (allow case flexibility)
    outcome <- tolower(outcome)
    s <- toupper(state)
   
    #Read the data
    dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## check if the state and outcome are valid
    st <- dat[,7]
    st <- unique(st)
    ou <- c("heart attack", "heart failure", "pneumonia")
    
    if(!(state %in% st)){
        stop("invalid state")
        
    }
    
    if(!(outcome %in% ou)){
        stop("invalid outcome")
    }
    
    ## Read outcome data with relevant columns
    mort <- dat[,c(2,7,11,17,23)]
    
    #transform relevant columns to numeric
    cols <- c(3,4,5)
    mort[,cols]<- lapply(mort[,cols],as.numeric)
    
    #filter dataframe by specified state
    flag <- mort[,2]==s
    sp <- mort[flag,]
    
    # set col names to easy to read names
    colnames(sp) <- c("Hospital Name","State", "Heart Attack Rate", "Heart Failure Rate", "Pneumonia")
    
    #source function evaluate to check the rank input (char vs num) and retrun the final result
    source("evaluate.r")
    print(sp)
    # Switch to check which outcome to generate 
    if(outcome=="heart attack"){
        ha <- sp[,c(1,2,3)]
        ha<- ha[order(ha$`Heart Attack Rate`, xtfrm(ha[,1])),]
        print(ha)
    }
    if(outcome=="heart failure"){
        ha <- sp[,c(1,2,4)]
        ha<- ha[order(ha$'Heart Failure Rate', xtfrm(ha[,1])),]
        print(ha)
        

    }
    if(outcome=="pneumonia"){
        ha <- sp[,c(1,2,5)]
        ha<- ha[order(ha$Pneumonia, xtfrm(ha[,1])),]
        print(ha)
        
    }
    
    #function produces the results based on rank (char vs num)
    res <- evaluate_rankall(ha, rank)
    
    #supress warnings
    options(warn=-1)
    
    #return the result
    print(res)
}

    