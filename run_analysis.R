##Setting Working Directory
install.packages("dplyr")
library(dplyr)
setwd("/Users/maulikchaudhary/Desktop/UCI HAR Dataset/")

## Importing test data sets such as subject_test, X_test, Y_test and feature table
features <- as.list(read.table("features.txt"))
subject_test <- read.table("test/subject_test.txt", col.names = "subjectid")
X_test <- read.table("test/X_test.txt", col.names = features$V2)
names(X_test) <- gsub("\\.","" , names(X_test))
Y_test <- read.table("test/y_test.txt", col.names = "activity")
activitylabels <- read.table("activity_labels.txt", col.names = c("id", "activity"))

##Binding test datasets with cbind function.
testset <- cbind(subject_test, Y_test,X_test)

#Applying for loop to change descriptive values of activity.
for(i in 1:6)
{
        testset[testset$activity == i, "activity"] <- as.character(activitylabels[[2]][i])
        
}

## Importing test data sets such as subject_train, X_train, Y_train
subject_train <- read.table("train/subject_train.txt", col.names = "subjectid")
X_train <- read.table("train/X_train.txt", col.names = features$V2)
names(X_train) <- gsub("\\.", "", names(X_train))
Y_train <- read.table("train/y_train.txt", col.names = "activity")

##Binding train datasets
trainingset <- cbind(subject_train, Y_train, X_train)

## Applying for loop to change numerical values of activity labels. 
## Following loop will convert activity variable to descriptive values.
for(i in 1:6)
{
        trainingset[trainingset$activity == i, "activity"] <- as.character(activitylabels[[2]][i])
}


#Binding Testset and traininig set. 
##First 2947 value will be from test set and rest 7352 values will be from trainingset.

activitytrackingcombined <- rbind(testset, trainingset)

#Filtering columns names with mean or std. 1 belongs to subjectid and 2 belongs to activity.
##Output has 88 columns. Out of which 86 has mean or std in their name and rest 2 belongs to subjectid and activity.
activityTrackingMeanStd <- activitytrackingcombined[, c(1, 2, grep("mean" , 
                                                        names(activitytrackingcombined)), 
                                                        grep("std", names(activitytrackingcombined)))]

##  Appropriately labels the data set with descriptive variable names
names(activityTrackingMeanStd)<-gsub("^t", "time", names(activityTrackingMeanStd))
names(activityTrackingMeanStd)<-gsub("^f", "frequency", names(activityTrackingMeanStd))
names(activityTrackingMeanStd)<-gsub("Acc", "Accelerometer", names(activityTrackingMeanStd))
names(activityTrackingMeanStd)<-gsub("Gyro", "Gyroscope", names(activityTrackingMeanStd))
names(activityTrackingMeanStd)<-gsub("Mag", "Magnitude", names(activityTrackingMeanStd))
names(activityTrackingMeanStd)<-gsub("BodyBody", "Body", names(activityTrackingMeanStd))

## Making a tidy dataset
newData<-aggregate(. ~subjectid + activity, activityTrackingMeanStd, mean)
newData<-newData[order(newData$subject,newData$activity),]
rownames(newData) <- NULL
write.table(newData, file = "tidydata.txt",row.name=FALSE,quote = FALSE, sep = '\t')
