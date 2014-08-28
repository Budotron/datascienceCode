## The following function returns a two column dataframe containing the hospital in each
## state that has the ranking specified in num

# Set the working directory
setwd("/Volumes/Media and Stuff/code/rprog-data-ProgAssignment3-data")

rankall <- function(outcome, num = "best") {
        
        ## Read outcome data
        
        hospitaldata<-read.csv("outcome-of-care-measures.csv", colClasses="character")
        
        ## Check that the input outcome is valid
        
        conditionslist<-c("heart attack", "heart failure", "pneumonia")
        if (!is.element(outcome, conditionslist)){
                stop("Invalid Outcome")
        }
        
        stateslist<-unique(hospitaldata$State)
        ## For each state, find the hospital of the given rank
        
        # First, we create the required factor
        
        condition<-.simpleCap(outcome)
        reqdcondition<-paste("Hospital.30.Day.Death..Mortality..Rates.from", 
                             condition, sep = ".") #required factor
        
        # we subset the hospital data to obtain data only for the required factor
        
        hospitaldata<-hospitaldata[, c("Hospital.Name", "State", reqdcondition)]
        
        # the hospital data is split by different states. For each state, we arrange 
        # third column, corresponding to the required outcome, in increasing order. Thus
        # databystate$TX[rankedbystate$TX[1,]] is the higest ranked hospital in Texas
        # for the outcome of interest
        databystate<-split(hospitaldata, hospitaldata$State)
        rankbystate<-lapply(databystate, 
               function(x) order(as.numeric(x[, 3])))
#         print(databystate$TX[rankedbystate$TX[1:6], ])
        #Now loop through every state in the stateslist vector. 
        for (i in (1:length(stateslist))){
                # construct a one-dimensional matrix from every element in the list 
                # rankbystate
                instateranks<-do.call(cbind, rankbystate[stateslist[i]])
                if (num=="best"){
                        num<-1
                }
                if(num=="worst"){
                        num<-length(instateranks)
                }
                if (num > length(instateranks)){
                        print(NA)
                }
                temp<-do.call(cbind, databystate[stateslist[i]])
                print(temp[instateranks[num], c(1,2, 3)])
        }   
}

#this function converts the leading letters of a string into capital letters
.simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1, 1)), substring(s, 2),
              sep = "", collapse = ".")
}