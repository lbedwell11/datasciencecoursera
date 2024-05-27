### GETTING AND CLEANING DATA COURSE - WEEK 4 PROJECT
# ASSIGNMENT OBJECTIVES -- You should create one R script called run_analysis.R that does the following. 
# 1.  Merges the training and the test sets to create one data set.
# 2.  Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.  Uses descriptive activity names to name the activities in the data set
# 4.  Appropriately labels the data set with descriptive variable names. 
# 5.  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(tidyverse)

#unzip the files
zipped_files <- list.files("data/ProgAssignment4", pattern = ".zip$", full.names = TRUE)
walk(zipped_files, unzip, exdir= "data/PA4unzipped")
file.names <- list.files("data/PA4unzipped/UCI HAR Dataset", full.names = TRUE)
dataFolder <- "data/PA4unzipped/UCI HAR Dataset"
### READ IN THE DATA
# read training data
trainSubject <- read.table(file.path(paste0(dataFolder,"/train/subject_train.txt")))
trainValues <- read.table(file.path(paste0(dataFolder,"/train/X_train.txt")))
trainActivity <- read.table(file.path(paste0(dataFolder,"/train/y_train.txt")))

# read test data
testSubject <- read.table(file.path(paste0(dataFolder,"/test/subject_test.txt")))
testValues <- read.table(file.path(paste0(dataFolder,"/test/X_test.txt")))
testActivity <- read.table(file.path(paste0(dataFolder,"/test/y_test.txt")))

# features
features <- read.table(file.path(dataFolder,"/features.txt"))
names(features)
head(features)
# activity labels
activity <- read.table(file.path(dataFolder,"/activity_labels.txt"))
head(activity)
colnames(activity) <- c("activityID","activityLabel")

### OBJECTIVE 1: Merges the training and the test sets to create one data set.
head(testActivity)
head(testSubject)
head(testValues)
dim(trainValues)
#combine datasets
dcomb <- rbind(cbind(trainSubject,trainValues,trainActivity),
               cbind(testSubject,testValues,testActivity)
               )
#assign column names using reference columns from features and activity
colnames(dcomb) <- c("subject",features[,2],"activity")

### OBJECTIVE 2: Extracts only the measurements on the mean and standard deviation for each measurement.
colsNeeded <- grep("subject|activity|mean|std",x = colnames(dcomb))
dcomb2 <- dcomb %>% select(colsNeeded)


### OBJECTIVE 3: Uses descriptive activity names to name the activities in the data set
dcomb2$activity %>% unique()
activity
dcomb2$activity <- factor(dcomb2$activity,
                          levels = activity[,1],
                          labels = activity[,2])


### OBJECTIVE 4: Appropriately labels the data set with descriptive variable names. 
colnames(dcomb2)
colnames(dcomb2) <- gsub(pattern = "^t","time",colnames(dcomb2)) 
colnames(dcomb2) <- gsub(pattern = "^f","freq",colnames(dcomb2)) 
colnames(dcomb2) <- gsub(pattern = "Acc","Accelerometer",colnames(dcomb2)) 
colnames(dcomb2) <- gsub(pattern = "Gyro","Gyroscope",colnames(dcomb2))
colnames(dcomb2) <- gsub(pattern = "std","Std Dev", colnames(dcomb2))
colnames(dcomb2) <- gsub(pattern = "BodyBody","Body", colnames(dcomb2))
colnames(dcomb2)

### OBJECTIVE 5: From the data set in step 4, creates a second, independent tidy data set 
###   with the average of each variable for each activity and each subject.
final_df <- dcomb2 %>% 
  group_by(subject, activity) %>% 
  summarise_all(.funs = mean) 
final_df


### ASSIGNMENT REQUIREMENTS: You will be required to submit: 
###   1) a tidy data set as described below, 
###   2) a link to a Github repository with your script for performing the analysis, and 
###   3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. 
###   You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.
write.table(x = final_df, 
            file = "ProgrammingAssignment4/tidy_data_output.txt", 
            quote = FALSE, 
            row.names = FALSE)
