run_analYsis <- function(){
    ##Set working directory
    wDir <- "H:/Coursera/GettingAndCleaningData/project/Coursera project/UCI HAR Dataset"
    setwd(dir =  wDir)
    
    ##Read file that contains the column names
    dataColNames <- read.table("features.txt", sep = "")
    
    ##Get all the column names and positon related to the MEAN and STD
    dataColNamesSubset <- rbind(dataColNames[grep("std", dataColNames$V2),])
    dataColNamesSubset <- rbind(dataColNamesSubset,dataColNames[grep("mean", dataColNames$V2),])
    
    ##Load Test and Train data 
    dataTrainX <- read.table("train//X_train.txt", sep = "")
    dataTestX <- read.table("test//X_test.txt", sep = "")
    
    ##Load Test and Train descriptive activity names 
    dataTrainY <- read.table("train//y_train.txt", sep = "")
    dataTestY <- read.table("test//y_test.txt", sep = "")
    dataDescriptiveNames <- read.table("activity_labels.txt", sep = "")
    
    ##Get subset data for Test and Train observations
    dataTrainXSubset <- dataTrainX[,dataColNamesSubset[,1]]
    dataTestXSubset <- dataTestX[,dataColNamesSubset[,1]]
    
    ##Adding column observationType and rowNumber to identify the data source
    dataTrainXSubset$observationType <- "Train"
    dataTestXSubset$observationType  <- "Test"
    
    ##Adding column Activity
    dataTrainXSubset$Activity <- ""
    dataTestXSubset$Activity <- ""
    
    ##Adding column Subject
    dataTrainXSubset$Subject <- ""
    dataTestXSubset$Subject <- ""
    
    ##Assigning subject to each observation
    subjectTrain <- read.table("train/subject_train.txt", sep = "")
    subjectTest <- read.table("test//subject_test.txt", sep = "")
    
    dataTrainXSubset[,82] <- subjectTrain
    dataTestXSubset[,82] <- subjectTest
    
    ##Adding descriptive Activity for Test and Train data sets
    for(i in 1:nrow(dataDescriptiveNames)) {
        row <- dataDescriptiveNames[i,]
        name <- as.character(row[[2]])
        # do stuff with rowsour
        dataTrainXSubset [which(dataTrainY$V1 ==i),81] <-name 
        dataTestXSubset [which(dataTestY$V1 ==i),81] <-name
    }
    
    ##Consolidate both observations into 1
    dataToAnalize <- rbind(dataTrainXSubset)
    dataToAnalize <- rbind(dataToAnalize,dataTestXSubset)
    
    ##Name the columns of data frame with the names in dataColnamesSubset  
    v<-as.vector(t(dataColNamesSubset[,2]))  ##Make sure we get explicit names before assigning them to the data frame
    colnames(dataToAnalize) <- c(v,"observationType","Activity","Subject")
    
    tidyDataSet <-NULL
    
    ##LOOP through the distinct descriptive Names and subjects to get the MEAN
    for(i in 1:nrow(dataDescriptiveNames)) {
        row <- dataDescriptiveNames[i,]
        name <- as.character(row[[2]])
        
        ##Reset SubsetMeans variable
        subsetMeans <- NULL
        
        # do stuff with rowsour
        for(j in 1:30) {
            calcSubset <- subset(dataToAnalize,dataToAnalize$Subject ==j & dataToAnalize$Activity == name)
            
            subsetMeans <- data.frame(t(colMeans(calcSubset[,1:79])))
            subsetMeans <- cbind(subsetMeans, name) ##include Activity
            subsetMeans <- cbind(subsetMeans, j) ##include subject
            colnames(subsetMeans)[80] <- "Activity"
            colnames(subsetMeans)[81] <- "Subject"
            
            tidyDataSet <- rbind(tidyDataSet,subsetMeans)
        }
      
      
    }
    
    ##Write the output to a text file
    write.table(tidyDataSet, file = "run_analysis_output.txt", row.names = FALSE)

}