#load libraries
library(dplyr)

#declare initial variables
urlDataSource <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destDataSource <- "./data/dataSource.zip"

#Check if directory exists and creates it it's missing
if(!file.exists("./data")){
  dir.create("./data")
}

#download data source file
download.file(urlDataSource,
              destfile = destDataSource,
              method="curl",
             mode = "wb" )

#extract zip file
unzip(zipfile = destDataSource, 
      list = FALSE,
      overwrite = TRUE,
      exdir = "./data")

###########################
#load data into R
###########################

#training data
trainSubjects <- read.table("data/UCI HAR Dataset/train/subject_train.txt")
trainValues <- read.table("data/UCI HAR Dataset/train/X_train.txt")
trainActivity <- read.table("data/UCI HAR Dataset/train/y_train.txt")

#test data
testSubjects <- read.table("data/UCI HAR Dataset/test/subject_test.txt")
testValues <- read.table("data/UCI HAR Dataset/test/X_test.txt")
testActivity <- read.table("data/UCI HAR Dataset/test/y_test.txt")

#activity labels
labels <- read.table("data/UCI HAR Dataset/activity_labels.txt")

#features
features <- read.table("data/UCI HAR Dataset/features.txt", as.is = TRUE)

#######################################
# 01 merge data sets and set labels
#######################################

mergedDS <- rbind(
  cbind(trainSubjects, trainValues, trainActivity),
  cbind(testSubjects, testValues, testActivity)
)

#define column names
colnames <- c("SubjectId", features$V2, "ActivityType")


# if number of columns in mergedDS equals mumber of elements in colnames
if (ncol(mergedDS) == length(colnames)){
   colnames(mergedDS) <- colnames
}

#######################################
# 02 Extract mean and standad deviation for each measurement
#######################################

#extract column names to keep
#avoids dplyr use since column names are not unique
columnsToKeep <- grepl("SubjectId|ActivityType|mean[(][)]|std", colnames(mergedDS))
cleanDS <- mergedDS[, columnsToKeep]

#######################################
# 03 Use descriptive activity values with named factor levels
#######################################

#factors used to represent categorical data in this case, we want to replace the
#mergedDS$ActivityType which is a number by its categorical data value from the
#labels Dataset
cleanDS$ActivityType <- factor(cleanDS$ActivityType,levels = labels[, 1], labels = labels[, 2])

#######################################
# 04 Label data with descriptive variable names
#######################################

colnames <- colnames(cleanDS)
colnames <- gsub("^f", "frequency_", colnames)
colnames <- gsub("^t", "time_", colnames)
colnames <- gsub("Body", "body_", colnames)
colnames <- gsub("body_body_", "body_", colnames)
colnames <- gsub("Gravity", "gravity_", colnames)
colnames <- gsub("Acc", "accelerometer_", colnames)
colnames <- gsub("Gyro", "gyroscope_", colnames)
colnames <- gsub("Mag", "magnitude_", colnames)
colnames <- gsub("Jerk", "jerk_", colnames)
colnames <- gsub("-mean[(][)]", "mean", colnames)
colnames <- gsub("-std[(][)]", "standard_deviation", colnames)

#replace original column names
colnames(cleanDS) <- colnames 

#######################################
# 05 Create second independent tidy data 
# set with the average of each variable for each activity and sucject
#######################################

#groupby and summarise by subject and activity
cleanDS_mean <- cleanDS %>%
  group_by(SubjectId, ActivityType) %>%
  summarise_all(funs(mean))

#spit txt with results
write.table(cleanDS_mean, "data/spit_data.txt",
            row.names = TRUE)

#spit csv with results
write.table(cleanDS_mean, "data/spit_data.csv",
            row.names = TRUE)





