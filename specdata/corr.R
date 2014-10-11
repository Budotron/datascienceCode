corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
        setwd(paste("/Volumes/Media and Stuff/code", directory, sep="/"))
        
        # Obtain the numbers of complete cases for every monitor id
        
        allcc<-complete(directory = directory, id = 1:332)
        
        # Retain only those data for which the number of complete cases exceeds the 
        # specificed threshold
        
        discardmonitor<-(allcc$nobs<=threshold)
        allcc<-allcc[!discardmonitor, ]
        
        # Initialize the vector that will hold the correlations
        
        corvec<-vector(length = nrow(allcc))
        
        # For each monitor id in the allcc dataframe, the following reads the .csv file, 
        # calculates the correlation between the sulfate and nitrate values for which 
        # there are complete cases, and assigns the jth corrlation calculated to the jth 
        # position of the corvec vector
        
        filenames<-vector(length = nrow(allcc))
        j<-1
        for (i in (allcc$id)){
                numdigits<-nchar(allcc$id[j])
                if (numdigits==1){
                        tag<-paste("00",  allcc$id[j], sep="")
                }
                else if (numdigits==2){
                        tag<-paste("0",  allcc$id[j], sep="")
                }
                else {
                        tag<-as.character(allcc$id[j]);
                }
                filenames[i]<-paste(tag, ".csv", sep="")
                idset<-read.csv(filenames[i])
                idset<-na.omit(idset)
                corvec[j]<-cor(idset$nitrate, idset$sulfate)
                j<-j+1
        }
        corvec
}