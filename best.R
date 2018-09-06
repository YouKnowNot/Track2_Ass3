
## Finding the best hospital in a state
# Takes two arguments. 1: 2 char abbr name of state. COL #7  2: outcome type (e.g. hear attack) COL #11, 17, 23
# Returns name of the institute COL #2

best <- function(state=character, outcome=character){
    
    #uniform the char format 
    outcome <- tolower(outcome)
    s <- toupper(state)
    
    #Read the data
    dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    dat[,11] <- as.numeric(dat[,11])
    
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
    cols <- c(3,4,5)
    mort[,cols]<- lapply(mort[,cols],as.numeric)
        
    #filter dataframe by relevant state
    flag <- mort[,2]==s
    sp <- mort[flag,]

    # set col names to easy to read names
    colnames(sp) <- c("Hospital Name","State", "Heart Attack Rate", "Heart Failure Rate", "Pneumonia")
    
    if(outcome=="heart attack"){
        ha <- sp[,c(1,2,3)]
        ha<- ha[order(ha$`Heart Attack Rate`, -xtfrm(ha[,1])),]
        res<-ha[1,1]
    }
    if(outcome=="heart failure"){
        ha <- sp[,c(1,2,4)]
        ha<- ha[order(ha$'Heart Failure Rate', -xtfrm(ha[,1])),]
        res<-ha[1,1]
    }
    if(outcome=="pneumonia"){
        ha <- sp[,c(1,2,5)]
        ha<- ha[order(ha$Pneumonia, -xtfrm(ha[,1])),]
        res<-ha[1,1]
    }
    options(warn=-1)

    print(res)
}