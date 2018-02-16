# Loading the required packages
#install.packages("dplyr") # install if required
#install.packages("plyr") # install if required


filesDir <- "C:/Coursera/R/myData/"
list.files(filesDir)  

# Files available in directory
# features.txt
# activity_labels.txt 
# X_train.txt
# y_train.txt
# subject_train.txt 
# subject_test.txt 
# X_test.txt 
# y_test.txt 


library(dplyr)
library(plyr)

# Read Features data
featuresData <- read.table(file.path(filesDir, "features.txt")) # will be used for both X_train & X_test data
dim(featuresData)

# Read the Activity labels data
activityData <-  read.table(file.path(filesDir, "activity_labels.txt"))

# read train data
xTrainData <- read.table(file.path(filesDir, "X_train.txt"))
yTrainData <- read.table(file.path(filesDir, "y_train.txt"))
subjectTrainData <- read.table(file.path(filesDir, "subject_train.txt"))

# Read Test Data
xTestData <- read.table(file.path(filesDir, "X_test.txt"))
yTestData <- read.table(file.path(filesDir, "y_test.txt"))
subjectTestData <- read.table(file.path(filesDir, "subject_test.txt"))


# column combine all the train data
trainData <- cbind(subjectTrainData,yTrainData,xTrainData )


# column Combine for all Test data
testData <- cbind(subjectTestData,yTestData,xTestData )

# ---- STEP 1.Merges the training and the test sets to create one data set --------------
oneDataSet <- rbind(trainData,testData)

# Now Set all Variable Names for the merged data set
varNames <- c("subject","activity_num",as.character(featuresData$V2))

# Now assign the Variable names to the merged data set
names(oneDataSet) <- varNames


# Assign the Variable Names to the Activity Data Set
names(activityData) <- c("activity_num", "activity_type")

# .. Joining two data set (oneDataSet and activityData) with reference activity_num
oneDataSet <- join(oneDataSet, activityData, by="activity_num")


#----------- STEP 2: EXCTRACT ONLY MEASUREMENTS ON THE MEAN AND STD DEVIATION.

# Can't perform the extraction because of duplicated column names: 
# So we need to remove the duplicate culumns
noOfDuplicates <- sum(duplicated(featuresData$V2)) # 84 duplicates


# Remove the duplicates from the Merged table(oneDataSet)
duplCol <-  as.character(featuresData$V2[duplicated(featuresData$V2)])

finalDataSet <- oneDataSet[, !(colnames(oneDataSet) %in% duplCol)]


#Now subset with only measurements on the mean and standard deviation variables
subDataSet <- select(finalDataSet, contains("subject"), contains("activity_type"), contains("mean"), contains("std"))
dim(subDataSet) # dataset has 88 variables

# 3.Uses descriptive activity names to name the activities in the data set
  # ** Already done in step 1 ****


# 4.Appropriately labels the data set with descriptive variable names
  # ** Already done in step 1 


#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## S3 method for class 'data.frame'
# syntax: aggregate(x, by, FUN, ..., simplify = TRUE, drop = TRUE)

finalDf <- aggregate(subDataSet,by=list(subDataSet$subject, subDataSet$activity_type), FUN=mean )

dim(finalDf) # 180 rows which makes sense. But 2 columns have been added.

#Fix columns for the finalDf created with the aggregate function:
finalDf$subject<-NUL          # remove subject column
finalDf$activity_type<-NULL   # remove activity_type column

# Now change the 1st and second column
colnames(finalDf)[1:2]<-c("subject","activity_type")
str(finalDf) #is a data frame, subject is integer, activity_type is factor and the rest of columns are all numeric class.

#Write final dataset in a .txt file:
write.table(finalDf, "tidy_dataset_with_avg.txt", row.name=FALSE)



