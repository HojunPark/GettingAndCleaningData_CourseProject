#TRAINING DATA
#set the working directory to be the train folder
setwd("~/Desktop/Online/OWinter Term 2014/Getting and Cleaning Data/Course Project/UCI HAR Dataset/train")

#read in the training data into R
X_train <- read.table("X_train.txt")
y_train <- read.table("y_train.txt")
subject_train <- read.table("subject_train.txt")

#========================================================================#

#TEST DATA
#set the working directory to be the train folder
setwd("~/Desktop/Online/OWinter Term 2014/Getting and Cleaning Data/Course Project/UCI HAR Dataset/test")

#read in the test data into R
X_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")
subject_test <- read.table("subject_test.txt")

#========================================================================#
#========================================================================#
#========================================================================#

#set the working directory to be the Course Project folder
setwd("~/Desktop/Online/OWinter Term 2014/Getting and Cleaning Data/Course Project")

#1. Merges the training and the test sets to create one data set.
#merge the training and test X, y, subject data together
X <- rbind(X_train, X_test)
y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)

#merge the X, y, and subject data into a data frame
data <- data.frame(subject, y, X)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
#mean measurement
#4. Appropriately labels the data set with descriptive variable names.

#set the working directory to be the UCI HAR Dataset folder that contains
#the features.txt file
setwd("~/Desktop/Online/OWinter Term 2014/Getting and Cleaning Data/Course Project/UCI HAR Dataset")

#read in the features.txt into R that contains the 561 feature names
features <- read.table("features.txt")
class(features[,1]) #integer
class(features[,2]) #factor
features[,2] <- as.character(features[,2])
class(features[,2]) #character

#replace the '-', ',', '(', ')', characters in the features character
#vector with '_', "", "", and "" respectively
features[,2] <- gsub("-", "_", features[,2])
features[,2] <- gsub(",", "", features[,2])
features[,2] <- gsub(")", "", features[,2])
features[,2] <- gsub("\\(", "", features[,2])

#set the working directory to be the Course Project folder
setwd("~/Desktop/Online/OWinter Term 2014/Getting and Cleaning Data/Course Project")

featureNames <- vector(mode="character")
featureColumns <- vector(mode="numeric")

i <- 0
for (name in features[,2]) {
        i <- i + 1
        print(i)
        meanMatch = grepl("mean", name)
        sdMatch = grepl("std", name)
        if (meanMatch || sdMatch) {
                featureNames <- append(featureNames, name)
                featureColumns <- append(featureColumns, i)
        }
}

dataset <- cbind(subject, y, X[, featureColumns])

names(dataset) <- c("subjectID", "activity", featureNames)

#3. Uses descriptive activity names to name the activities in the data set
#set the working directory to be the UCI HAR Dataset folder that contains
#the activity_labels.txt
setwd("~/Desktop/Online/OWinter Term 2014/Getting and Cleaning Data/Course Project/UCI HAR Dataset")

#read in the activity_labels.txt into R that contains the
#numeric labels for the 6 activities
activityFile <- read.table("activity_labels.txt")
activity <- activityFile
class(activity[,1]) #integer
class(activity[,2]) #factor
activity[,2] <- as.character(activity[,2])
class(activity[,2]) #character

#replace the integers 1, 2, 3, 4, 5, 6 in the y column of data with
#the corresponding activity label names
for (i in seq(activity[,1])) {
        y[,1] <- gsub(activity[i,1], activity[i,2], y[,1])   
}

#modify the data data frame with the modified y column
dataset[,2] <- y

#set the working directory to be the Course Project folder
setwd("~/Desktop/Online/OWinter Term 2014/Getting and Cleaning Data/Course Project")

# 5. Creates a second, independent tidy data set with the average of each
#variable for each activity and each subject. 
#initialize the columns of the tidy data frame
subjectID <- rep(1:30, 1, each=6)
activityVector <- rep(activity[,2], 30)
measurement <- matrix(rep(0, (ncol(dataset)-2)*180), nrow=180)
tidy <- data.frame(subjectID, activityVector, measurement)

#name the columns of the tidy data frame
names(tidy) <- c("subjectID", "activity", featureNames)

#populate the tidy data set with the average of each variable for each
#activity and each subject. 
for (row in seq(subjectID)) {
        subject <- subjectID[row]
        #print(paste(row, subject, sep=": "))
        for (act in activity[,2]) {
                measurementColumns <- dataset[dataset$subjectID==subject & dataset$activity==act, 3:ncol(dataset)]
                currentMeasurementMean <- apply(measurementColumns, 2, mean)
                tidy[row,3:ncol(tidy)] <- currentMeasurementMean
        }
}

#Save the tidy data frame containing the aggregate data in a .txt file
write.table(tidy, file='tidyData.txt')

#Save the featureNames vetor containing the features names in our tidy
#data into a .txt file. Then, we can conveniently copy these names from
#the .txt file into our code book
write.table(featureNames, file='featureNames.txt')
