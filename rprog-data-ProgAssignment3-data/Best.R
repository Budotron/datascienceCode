## The following function obtains the lowest 30 day mortality for the specified outcome 
## any US state
setwd("/Volumes/Media and Stuff/code/rprog-data-ProgAssignment3-data")
best <- function(state, outcome){
        #Read outcome data
        hospital.data<-read.csv("outcome-of-care-measures.csv", colClasses="character")
        # Check that state and outcome are valid
        stateslist<-unique(hospital.data$State)
        if (!is.element(state, stateslist)){
                stop("Invalid State")
        }
        conditionslist<-c("heart attack", "heart failure", "pneumonia")
        if (!is.element(outcome, conditionslist)){
                stop("Invalid Outcome")
        }
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        findstate<-(hospital.data$State==state)
        hospital.data<-hospital.data[findstate, ]
        .simpleCap <- function(x) {
                s <- strsplit(x, " ")[[1]]
                paste(toupper(substring(s, 1, 1)), substring(s, 2),
                      sep = "", collapse = ".")
        }
        condition<-.simpleCap(outcome)
        reqdcondition<-paste("Hospital.30.Day.Death..Mortality..Rates.from", 
                             condition, sep = ".")
#         print(reqdcondition)
        hospital.data<-hospital.data[,c("Hospital.Name", "State", reqdcondition)]
#         print(hospital.data[, 3])
        min<-which(hospital.data[,3] == min(as.numeric(hospital.data[, 3]), na.rm = T))
#         print(min(hospital.data[,3]))
#         print(hospital.data[min, ])
        desired<-hospital.data[min, ]
        list<-sort(desired$Hospital.Name, decreasing = F)
#         print(list)
        list[1]
}