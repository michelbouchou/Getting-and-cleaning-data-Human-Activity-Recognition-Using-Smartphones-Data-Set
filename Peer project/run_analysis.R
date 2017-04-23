run_analysis <- function() {
    
    library("dplyr")
    
    # Get data
    
    train <- read.table("UCI HAR Dataset/train/X_train.txt")
    test <- read.table("UCI HAR Dataset/test/X_test.txt")
    labels <- read.table("UCI HAR Dataset/activity_labels.txt")
    train_y <- read.table("UCI HAR Dataset/train/y_train.txt")
    test_y <- read.table("UCI HAR Dataset/test/y_test.txt")
    train_sub <- read.table("UCI HAR Dataset/train/subject_train.txt")
    test_sub <- read.table("UCI HAR Dataset/test/subject_test.txt")
    
    # Merge to one big dataset
    
    train <- cbind(train_y, train_sub, train)
    test <- cbind(test_y, test_sub, test)
    merged <- rbind(train, test)
    
    # Name the colum
    
    features <- read.table("UCI HAR Dataset/features.txt")
    names <- c("Labels", "Subject")
    names <- c(names, as.character(features$V2))
    colnames(merged) <- names
    
    # Get the "mean" and "std" values
    
    smaller <- merged[grepl("Labels", names(merged)) | grepl("Subject", names(merged)) | grepl("mean", names(merged)) | grepl("std", names(merged))]
    
    # Labels the activities
    
    labelled <- merge(smaller, labels, by.x = "Labels", by.y = "V1")
    labelled$labels <- labelled$V2
    labelled <- labelled[-82]
    arrange(labelled, subject, labels)
    
    # Get the second tidy dataset with means value
    
    mean <- labelled[-c(1, 2)]
    mean <- as.data.frame(sapply(mean, mean))
    
    # Write the datasets
    
    write.table(labelled, "tidy.txt")
    write.table(mean, "mean.txt")
    
}