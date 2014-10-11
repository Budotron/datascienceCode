complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        setwd(paste("/Volumes/Media and Stuff/code", directory, sep="/"))
        
        # The following code uses the monitor id numbers to construct a character vector 
        # of length 1 with the extension ".csv", and appends one or two zeros to the 
        # front of the input number to match the filenames, if necessary. This is then 
        # used to read the csv data appropriate to that monitor. 
        
        filenames<-vector(length = length(id))
        for (i in 1:length(id)){
                numdigits<-nchar(id[i])
                if (numdigits==1){
                        tag<-paste("00",  id[i], sep="")
                }
                else if (numdigits==2){
                        tag<-paste("0",  id[i], sep="")
                }
                else {
                        tag<-as.character(id[i]);
                }
                filenames[i]<-paste(tag, ".csv", sep="")
        }
        
        # create an empty vector of the same length as "id" to store the total count of 
        # complete cases for each monitor id
        
        nobs<-vector(length = length(id))
        
        # create a data frame containing the monitor id numbers and the (initially empty)
        # nobs vector
        cc<-data.frame(id, nobs) #cc for complete cases
        
        # populate the nobs column of the dataframe  
        for (i in 1:length(filenames)){
                idset<-read.csv(filenames[i])
                idset<-na.omit(idset) # remove all rows containing an "NA"
                cc$nobs[i]<-nrow(idset)
        }
        print(cc)
}