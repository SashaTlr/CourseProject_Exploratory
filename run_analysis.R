Course_Proj <- function() {

    ## read in test files
    subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
    X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
    Y_test <- read.table("UCI HAR Dataset/test/Y_test.txt")

    ## read in train files
    subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
    X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
    Y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
    
    #combine files
    test_data <- cbind(subject_test, X_test, Y_test)
    train_data <- cbind(subject_train, X_train, Y_train)
    
    ## bind test and train data together
    total_data <- rbind(test_data, train_data)

    ## read in features table
    features <- read.table("UCI HAR Dataset/features.txt", as.is = TRUE)
    activity <- read.table("UCI HAR Dataset/activity_labels.txt", as.is = TRUE)
    
    
    ## name columns of total_data

    colnames(total_data)[1] <- "Subject" 
    colnames(total_data)[563] <- "Activity"
    colnames(total_data)[2:562] <- features[,2]
    
    ## pull out columns that have a mean or a standard deviation calculation
    temp_mean <- copy_data[, grep('mean', names(copy_data))]
    temp_std <- copy_data[, grep('std', names(copy_data))]
    
    ## merge data that contains either a mean or standard deviation with the corresponding Subject and Activity
    new_data <- cbind(as.numeric(total_data[,1]), total_data[,563], temp_mean, temp_std)
    colnames(new_data)[1] <- "Subject" 
    colnames(new_data)[2] <- "Activity"
    
    tidyData <- aggregate(.~ Subject + Activity,new_data,mean,na.rm=T)
    
    #write out tidy data file
    write.table(tidyData, file="tidyData.txt")

    
}


