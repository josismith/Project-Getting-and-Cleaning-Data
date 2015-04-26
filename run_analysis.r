run_analysis <- function (url = "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" ) {
  library(plyr)
  library(utils)
  ## run_analysis function extracts the mean and standard deviation of measurements in the 
  ## http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones  dataset
  ## and presents averages of the features grouped by subject and activity?
  
  
  ##decloration of variables
  
  test_input <-vector()
  train_input <- vector()
  train_subject <- vector()
  test_subject <- vector()
  feature_labels <- vector()
  activity_labels <- vector ()
  extract_labels <- vector ()
  extract <- vector ()
  input <- vector()
  column_name <- character
  index <-integer
  
  ## import files
  download.file(url, dest="dataset.zip", mode="wb") 
  unzip ("dataset.zip", exdir ="./Summary of UCI HAT Dataset" )
  
## read in files  

  train_set<- read.table(file = "./Summary of UCI HAT Dataset/UCI HAR Dataset/train/X_train.txt",colClasses = "character")
  train_labels <- read.table(file = "./Summary of UCI HAT Dataset/UCI HAR Dataset/train/Y_train.txt",colClasses = "character")
  train_subject <- read.table(file = "./Summary of UCI HAT Dataset/UCI HAR Dataset/train/subject_train.txt",colClasses = "character")
  test_set <-read.table(file = "./Summary of UCI HAT Dataset/UCI HAR Dataset/test/X_test.txt",colClasses = "character")
  test_labels <- read.table(file = "./Summary of UCI HAT Dataset/UCI HAR Dataset/test/Y_test.txt",colClasses = "character")
  test_subject <- read.table(file = "./Summary of UCI HAT Dataset/UCI HAR Dataset/test/subject_test.txt",colClasses = "character")
  activity_labels <- read.table(file = "./Summary of UCI HAT Dataset/UCI HAR Dataset/activity_labels.txt",colClasses = "character")
  feature_labels <- read.table(file = "./Summary of UCI HAT Dataset/UCI HAR Dataset/features.txt",colClasses = "character")


## putting the activities and set together
  test_input <-c(test_subject, test_labels, test_set)
  train_input <-c(train_subject, train_labels, train_set)
  
## Merge the training and the test sets to create one data set.

  input <- merge(train_input,test_input,all=TRUE)

## adds subject and activities row
extract <- cbind(extract,input[,1]) 
extract <- cbind(extract,input[,2])
extract_labels <-cbind(extract_labels, "SubjectID")
extract_labels <-cbind(extract_labels, "ActivityID")

##Extracts only the measurements on the mean and standard deviation for each measurement. mean() and std()
## extract only columns identified in the list


 I <- 0
 for(I in feature_labels[,1]){
   
   ## find the feature labels that end with mean() or std() 

   
   
     if(length(grep("mean()",feature_labels[I,2]))>0) {
       
        if(length(grep("Freq()",feature_labels[I,2]))==0){

          if (feature_labels[I,1] == 1){
            index = 1.2
          }else{index = feature_labels[I,1]}
          
         ## add the corresponding column label to a list
         extract_labels <- cbind(extract_labels, feature_labels[I,2])
     
          ## extract the column
            column_name <- paste("V", as.character(index), sep = "")
            extract <- cbind(extract, input[,column_name]) 
            
        } }
     
     if (length(grep("std()",feature_labels[I,2]))>0) {
       extract_labels <- cbind(extract_labels, feature_labels[I,2])
       column_name <- paste("V", as.character(index), sep = "")
       extract <- cbind(extract, input[,column_name]) }}

##Appropriately labels the data set with descriptive variable names. 

  colnames(extract) <- extract_labels 
  colnames(activity_labels) <- c("ActivityID","Activity")

##Uses descriptive activity names to name the activities in the data set
  
  
  extract <- merge(extract, activity_labels, by.x="ActivityID", by.y = "ActivityID")


## builds summary tidy data set with the average of each variable for each activity and each subject.

x<-ddply(extract[,!(names(extract) %in% c("ActivityID"))], .(SubjectID, Activity), colwise(mean))
write.table(x,"./Summary of UCI HAT Dataset/Summary.txt", row.names = FALSE)

}
