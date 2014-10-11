pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        
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
        
        # The following merges all files in "filenames" into a single data frame. It is 
        # based on the mailing list topic "Import Multiple csv files and merge into one
        # Master file" (https://stat.ethz.ch/pipermail/r-help/2010-October/255593.html)
        
        mergedfiles<-do.call("rbind", lapply(filenames, read.csv, header = TRUE))
        
        # The mean of the pollutant of interest is obtained by the following
        
        if (pollutant=="nitrate"){
                reqdmean<-mean(mergedfiles$nitrate, na.rm = T)
        } else {
                reqdmean<-mean(mergedfiles$sulfate, na.rm = T)
        }
        reqdmean
}