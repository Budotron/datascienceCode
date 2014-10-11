rankall <- function(outcome, num = "best") {
        
        ## Read outcome data
        setwd("/Volumes/Media and Stuff/code/rprog-data-ProgAssignment3-data")
        alldata<-read.csv("outcome-of-care-measures.csv", colClasses="character")

        ## Check that state and outcome are valid
        isvalid<-checkvalidity(outcome)
        if (isvalid == F){stop("Invalid Outcome")}
        mortalityrate <- reqdcondition(outcome)

        ## For each state, find the hospital of the given rank
        # crop the dataset to only include hospital name, state, and the outcome 
        # of interest
        alloutcomedata <- alldata[, c("Hospital.Name", "State", mortalityrate)]
        # split the resulting dataframe by state, and order them by increasing 
        # mortality rate. 
        databystate<-split(alloutcomedata, alloutcomedata$State)
        rankedbystate<-lapply(databystate, function(x) order(x[,3]))
        stateslist<-unique(alloutcomedata$State)
        
        # rankedbystate is a list, the ith element of which holds the ranked indices of 
        # hospitals in stateslist[i]. Loop through each state on stateslist
        
        # Create an empty dataframe
        hospital<-NA; state<-NA; outcome<-NA
        allranked<-data.frame(hospital, state, outcome)[numeric(0),]
        for (i in (1:length(stateslist))){
                # make a dataframe of unranked hospitals by state
                temp1<-do.call(cbind, databystate[i])
                # make a dataframe of ranked indices of hospitals by state
                temp2<-do.call(cbind, rankedbystate[i])
                # arrange the rows of temp1 by the indices in temp2. The resulting data 
                # frame has all hospitals in state[i] ranked by increasing mortality, 
                # but has not accounted for alphabetizing ties
                nearrank<-temp1[temp2, ]; 
                # get the required rank
                rank<-getrank(num, nearrank)
                # check if the rankth rank is duplicated in nearrank
                isduped<-checkdupes(nearrank)
                if (rank == 0){
                        # the rankth number DNE
                        allranked[i,] <-c(NA, stateslist[i], NA)
                }else if (isduped[rank] == F){
                        # the rankth number is not repeated
                        allranked[i, ]<-nearrank[rank, c(1,2,3)]
                }else{
                        # extract all rows with the same mortality rate, and alphabetize
                        # the hospitals
#                         allranked[i, ]<-c("problems", stateslist[i], "problems")
                        names<-(nearrank[1])
                        inds<-which (nearrank[,3] %in% nearrank[rank, 3])
                        names<-sort((names[inds, ]))
                        if (num == "best"){
                                allranked[i, ]<-c(names[1], 
                                                  stateslist[i], nearrank[rank, 3])
                        }
                        if (num == "worst"){
                                allranked[i, ]<-c(tail(names, 1), 
                                                  stateslist[i], nearrank[rank, 3])
                        }
                } 
        }
        print(allranked)
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
#         display<-allranked[, c(1,2)]
#         print(display)
}

# checks that the input arguments of the function are valid
checkvalidity <- function(x){
        validconditions<-c("heart attack", "heart failure", "pneumonia")
        if (!is.element(x, validconditions)){F}else{T}
}

# capitalizes the first letter of input words
.simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1, 1)), substring(s, 2),
              sep = "", collapse = ".")
}

# Obtains the factor of interest
getfactorname <- function(x){
        
        paste("Hospital.30.Day.Death..Mortality..Rates.from", 
              x, sep = ".") #required factor
}

# Combine simpleCap and get factor name
reqdcondition <- function(x){
        y<-.simpleCap(x)
        getfactorname(y)
}

# Gets a number representing the required rank

getrank<-function(x, y){
        if (x == "best"){
                1
        }else if (x == "worst"){
                        nrow(y)
                }else if ((1 <= x) && (x <= nrow(y))){
                        x
                }else if (x>nrow(y)){
                        0
                }
}

checkdupes <- function(x){
        lookforward<-duplicated(x[,3]) #find all dupes from the front
        lookbackwards<-duplicated(x[, 3], fromLast = T)#find all dupes from the back
        duped<-vector()
        for (i in (1:length(x[,3]))){
                if ((lookforward[i] == F) && (lookbackwards[i] == F)){
                        #in this case, the value is not duplicated
                        duped[i]<-F
                }else{duped[i]<-T}
        }
        duped
}

