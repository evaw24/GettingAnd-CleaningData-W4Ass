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
#..
# $ tBodyAcc-std()-Z           : num  -0.992 -0.994 -0.995 -0.994 -0.993 ...
# $ tGravityAcc-mean()-X       : num  -0.209 -0.207 -0.207 -0.206 -0.206 ...
# $ tGravityAcc-mean()-Y       : num  0.771 0.769 0.77 0.769 0.769 ...
# $ tGravityAcc-mean()-Z       : num  0.589 0.593 0.592 0.592 0.591 ...
# $ tGravityAcc-std()-X        : num  -1 -0.999 -0.999 -0.998 -0.998 ...
# $ tGravityAcc-std()-Y        : num  -0.999 -0.998 -0.998 -0.997 -0.997 ...
# $ tGravityAcc-std()-Z        : num  -0.997 -0.995 -0.996 -0.997 -0.993 ...
# $ tBodyAccJerk-mean()-X      : num  0.0756 0.0726 0.0745 0.0792 0.078 ...
# $ tBodyAccJerk-mean()-Y      : num  0.01234 0.01028 0.01542 0.01387 0.00854 ...
# $ tBodyAccJerk-mean()-Z      : num  0.0014 -0.00264 0.00492 0.0021 -0.0053 ...
# $ tBodyAccJerk-std()-X       : num  -0.992 -0.991 -0.993 -0.993 -0.992 ...
# $ tBodyAccJerk-std()-Y       : num  -0.993 -0.994 -0.994 -0.993 -0.986 ..
# ...

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
# ...
# $ timeGravityAccelerometer-MEAN()-X             : num  -0.209 -0.207 -0.207 -0.206 -0.206 ...
# $ timeGravityAccelerometer-MEAN()-Y             : num  0.771 0.769 0.77 0.769 0.769 ...
# $ timeGravityAccelerometer-MEAN()-Z             : num  0.589 0.593 0.592 0.592 0.591 ...
# $ timeGravityAccelerometer-SD()-X               : num  -1 -0.999 -0.999 -0.998 -0.998 ...
# $ timeGravityAccelerometer-SD()-Y               : num  -0.999 -0.998 -0.998 -0.997 -0.997 ...
# $ timeGravityAccelerometer-SD()-Z               : num  -0.997 -0.995 -0.996 -0.997 -0.993 ...
# $ timeBodyAccelerometerJerk-MEAN()-X            : num  0.0756 0.0726 0.0745 0.0792 0.078 ...
# $ timeBodyAccelerometerJerk-MEAN()-Y            : num  0.01234 0.01028 0.01542 0.01387 0.00854 ...
# $ timeBodyAccelerometerJerk-MEAN()-Z            : num  0.0014 -0.00264 0.00492 0.0021 -0.0053 ...
# $ timeBodyAccelerometerJerk-SD()-X              : num  -0.992 -0.991 -0.993 -0.993 -0.992 ...
# $ timeBodyAccelerometerJerk-SD()-Y              : num  -0.993 -0.994 -0.994 -0.993 -0.986 ...
# $ timeBodyAccelerometerJerk-SD()-Z              : num  -0.992 -0.993 -0.993 -0.993 -0.993 ...
# $ timeBodyGyroscope-MEAN()-X                    : num  -0.0304 -0.0279 -0.0282 -0.0276 -0.0274 ...
# $ timeBodyGyroscope-MEAN()-Y                    : num  -0.0735 -0.0757 -0.0759 -0.0747 -0.0762 ...
# $ timeBodyGyroscope-MEAN()-Z                    : num  0.0556 0.0862 0.0852 0.0876 0.0882 ...
# $ timeBodyGyroscope-SD()-X                      : num  -0.999 -0.999 -0.998 -0.9
#...

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
