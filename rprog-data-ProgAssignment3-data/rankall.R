rankall <- function(outcome, num = "best") {
        require(reshape)
        ## Read outcome data
        setwd("/Volumes/Media and Stuff/code/rprog-data-ProgAssignment3-data")
        hospital.data<-read.csv("outcome-of-care-measures.csv", colClasses="character")
        
        ## Check that state and outcome are valid
        
        stateslist<-unique(hospital.data$State)
        conditionslist<-c("heart attack", "heart failure", "pneumonia")
        if (!is.element(outcome, conditionslist)){
                stop("Invalid Outcome")
        }
                
        #the following line splits the dataframe hospital.data by states
        databystates<-split(hospital.data, hospital.data$State)
        hospital<-NA; state<-NA
        allranked<-data.frame(hospital, state)[numeric(0),]
        list<-list()
        for (i in (1:54)){
                statename<-stateslist[i]
                #the following line splits the dataframe hospital.data by states
                databystates<-split(hospital.data, hospital.data$State)
                #the following line obtains the data associated only with the input state
                statedatalist<-databystates[names(databystates)==stateslist[i]]
                #the output of the previous line is a list. The following line converts it into 
                #dataframe. 
                statedataframe<-do.call(rbind.data.frame, statedatalist) 
                #converts the outcome into a string with capital leading letters
                condition<-.simpleCap(outcome)
                #concatenates the result of the above line to form the required factor name
                
                reqdcondition<-paste("Hospital.30.Day.Death..Mortality..Rates.from", 
                                     condition, sep = ".")
                #restrict the newhospitaldata dataframe to only the outcome and hospital names
                
                newhospitaldata<-statedataframe[,c("Hospital.Name", reqdcondition)]
                #finds the indices of the outcome in increasing order
                
                arrange<-order(as.numeric(newhospitaldata[, 2]))
                #arange the hospitals in decreasing order, ignoring ties
                newhospitaldata<-newhospitaldata[arrange, ]
                allhospitals<-nrow(newhospitaldata)
                # the data contains NA values, which are identified by converting the second
                # column to numeric values
                
                deaths<-as.numeric(newhospitaldata[, 2])
                deaths<-deaths[!is.na(deaths)]
                
                # truncate the newhospitaldata to remove hospitals for which we have 
                # no death info
                
                newhospitaldata<-newhospitaldata[1:length(deaths), ]
                
                #simplify typing by 
                
                names<-newhospitaldata$Hospital.Name
                
                # we address ties. When tied, the system must choose alphabetically 
                
                #obtain duplicated values
                bad1<-duplicated(deaths) # T means a that a value is repeated later in the vec
                bad2<-duplicated(deaths, fromLast = T) # T means a value is repeated before
                repeatedval<-vector() #a vector that will note the positions of all repeated vals
                for (i in (1:length(bad1))){
                        if (bad1[i]==F && bad2[i]==F){
                                # entry into this loop means that the values are not repeated
                                repeatedval[i]<-F
                        }else {
                                repeatedval[i]<-T
                        }
                }
                hospitaldata2<-cbind(newhospitaldata, repeatedval) 
                rankedlistofnames<-vector()
                for (i in (1:nrow(hospitaldata2))){
                        if (repeatedval[i]=="FALSE"){
                                rankedlistofnames<-c(rankedlistofnames,names[i])
                        } else{
                                #obtain the all rows that contain a repeated number of deaths
                                inds<-which(hospitaldata2[, 2] %in% hospitaldata2[i,2])
                                #extract and alphabetically order the names associated with 
                                #those rows
                                temp<-names[inds]
                                rankedlistofnames<-c(rankedlistofnames, sort(temp))
                                hospitaldata2[i:(i+length(temp)-1),3]<-"FAlSE"
                        }
                }
                rankedlistofnames<-unique(rankedlistofnames)
                if (num == "best"){
                        hospitalname<-rankedlistofnames[1]
                }else if (num == "worst"){
                        hospitalname<-tail(rankedlistofnames, 1)
                }else if (num > length(rankedlistofnames)){
                        hospitalname<-NA
                }else {
                        hospitalname<-rankedlistofnames[num]
                }
                list<-(cbind(list, c(hospitalname, statename)))

        }
        asvector<-unlist(list)
        asmatrix<-matrix(asvector, nrow = 0.5*length(asvector), ncol = 2, 
                         byrow = T )
        colnames(asmatrix)<-c("hospital", "state")
        df<-as.data.frame(asmatrix)
        print(df)
}

#this function converts the leading letters of a string into capital letters
.simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1, 1)), substring(s, 2),
              sep = "", collapse = ".")
}
getrank<-function(x, y){
        
}