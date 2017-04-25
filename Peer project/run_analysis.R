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
    
    train <- cbind(train_sub, train_y, train)
    test <- cbind(test_sub, test_y, test)
    merged <- rbind(train, test)
    
    # Name the colum
    
    features <- read.table("UCI HAR Dataset/features.txt")
    names <- c("subject", "labels")
    names <- c(names, as.character(features$V2))
    colnames(merged) <- names
    
    # Get the "mean" and "std" values
    
    smaller <- merged[grepl("subject", names(merged)) | grepl("labels", names(merged)) | grepl("mean", names(merged)) | grepl("std", names(merged))]
    
    # Labels the activities
    
    labelled <- merge(smaller, labels, by.x = "labels", by.y = "V1")
    labelled$labels <- labelled$V2
    labelled <- labelled[-82]
    
    # Get the second tidy dataset with means value
    
    tidy <- labelled %>% group_by(subject, labels) %>% summarise_each(funs(mean))
    
    # Write the dataset
    
    write.table(tidy, "tidy.txt", row.names = FALSE)
    
}