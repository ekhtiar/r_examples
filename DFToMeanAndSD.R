DFToMeanAndSD <- function(table){
#This function takes input of a data frame and returns
#the SD and mean of the columns of the datafarme
        
        #Select only numeric from data frame
        numeric <- sapply(table, is.numeric)
        #store only numeric data
        df <- table[,numeric]
        
        #use apply function for SD and mean
        #ps: margin = 2 indicates columns
        sd <- apply(X = df, 2, FUN = sd, na.rm = TRUE)
        mean <- apply(X = df, 2, FUN = mean, na.rm = TRUE)
        
        #transpose data
        #need reshape library for this
        library(reshape)
        sd <- melt(sd)
        mean <- melt(mean)
        #name the column accordingly
        names(sd) <- "SD"
        names(mean) <- "Mean"
        
        #merge the data and return result
        result <- cbind(sd,mean)
        result
        

}        