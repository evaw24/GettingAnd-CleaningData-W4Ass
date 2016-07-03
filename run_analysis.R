# 1. Merges the training and the test sets to create one data set.

# Read train and test files

setwd("/Users/donttouch/Coursera/GettingAndCleaningData/FinalAss/UCI HAR Dataset")

# Read train and test Subject files

File_TrainSubject = "./train/subject_train.txt"
File_TestSubject = "./test/subject_test.txt"

TrainSubject <- read.table(File_TrainSubject)
TestSubject <- read.table(File_TestSubject)

# Read train and test Labels files

# Activity Labels
# 1 WALKING
# 2 WALKING_UPSTAIRS
# 3 WALKING_DOWNSTAIRS
# 4 SITTING
# 5 STANDING
# 6 LAYING

File_TrainLabels = "./train/y_train.txt"
File_TestLabels = "./test/y_test.txt"

TrainLabels <- read.table(File_TrainLabels)
TestLabels <- read.table(File_TestLabels)

# Read train and test Sets files

File_TrainSets = "./train/X_train.txt"
File_TestSets = "./test/X_test.txt"

TrainSets<- read.table(File_TrainSets)
TestSets <- read.table(File_TestSets)

# Merge Subject,Labels and Sets files

TotalSubject = rbind(TrainSubject,TestSubject)
TotalLabels = rbind(TrainLabels,TestLabels)
TotalSets = rbind(TrainSets,TestSets)

# Set columns names for Subject and Labels data tables

names(TotalSubject) <- c("subject")
names(TotalLabels) <- c("activitynumber")

# Read and set column names  for activity_labels file

File_ActivityLabels = "activity_labels.txt"
ActivityLabelsDataFile <- read.table(File_ActivityLabels)
names(ActivityLabelsDataFile)<-c("activitynumber","activityname")

# Read features file and sets columns names

File_Features = "features.txt"
FeaturesDataFile <- read.table(File_Features)
names(FeaturesDataFile)<-c("featurenumber","featurename")

# Set TotalSets column names using FeaturesDataFile

ColumnNames<-FeaturesDataFile[,"featurename"]	
names(TotalSets)<-ColumnNames

# Merge data columns
SubjectActivityData <- cbind(TotalSubject,TotalLabels)
SubjectData <- cbind(SubjectActivityData, TotalSets)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

SubjectDataSubset1 <-SubjectData[1:2]
SubjectDataSubset2 <-SubjectData[grepl("mean\\(\\)|std\\(\\)",names(SubjectData))]

SubjectData <- cbind(SubjectDataSubset1,SubjectDataSubset2)

# 3. Use descriptive activity names to name the activities in the data set.

SubjectData <-merge(ActivityLabelsDataFile,SubjectData, by="activitynumber",all.x=TRUE)

#Order and sort by columns "subject", "activityname", ..
library(dplyr)
SubjectData <- SubjectData[c(3,2,1,4:ncol(SubjectData))]
SubjectData <- arrange(SubjectData,subject,activityname)

# 4. Appriopriately labels the data set with descriptive variable names.

# Variables names before 
str(SubjectData)
#'data.frame':	10299 obs. of  69 variables:
# $ subject                    : int  1 1 1 1 1 1 1 1 1 1 ...


# Gyro - gyroscopic measurements
# Acc - accelerometer measurements
# f and t - indicate frequency and time domain signal
# mean() and std() - for each subject for each activity measurements
#     Standard gravity units 'g'/s for accelerometer, radians/second for gyroscope 
#      and jerk ?rad/sec?
# Jerk - the body linear acceleration and angular velocity
# Gravity - gravity acceleration signals
# Body - body motion components
# Mag - magnitude

names(SubjectData)<-gsub("^t", "time", names(SubjectData))
names(SubjectData)<-gsub("^f", "frequency", names(SubjectData))
names(SubjectData)<-gsub("Acc", "Accelerometer", names(SubjectData))
names(SubjectData)<-gsub("Gyro", "Gyroscope", names(SubjectData))
names(SubjectData)<-gsub("Mag", "Magnitude", names(SubjectData))
names(SubjectData)<-gsub("BodyBody", "Body", names(SubjectData))
names(SubjectData)<-gsub("mean()", "MEAN", names(SubjectData))
names(SubjectData)<-gsub("std()", "SD", names(SubjectData))

str(SubjectData)
#'data.frame':	10299 obs. of  69 variables:

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for
# each activity and each subject.

SubjectDataAggr <- aggregate(. ~ subject - activityname, data=SubjectData , mean)

str(SubjectDataAggr)

#'data.frame':	180 obs. of  69 variables:
...
# Sort by subject ans activity
SubjectData <- arrange(SubjectDataAggr,subject,activityname)
str(SubjectDataAggr)

#'data.frame':	180 obs. of  69 variables:
...
## Write to a file

write.table(SubjectData,"tidydata.txt", row.names=FALSE)
