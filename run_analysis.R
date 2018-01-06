
#1. Merge the training and test sets to create one data set
#---------------------------------------------------------------
#Download the data
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="Dataset.zip",method="curl")

#Unzip the file.
unzip(zipfile="Dataset.zip")

#Read the Data
#Training Data
setwd("UCI HAR Dataset")

x_train<-read.table(".//train//X_train.txt")
y_train<-read.table(".//train//Y_train.txt")
subject_train<-read.table(".//train//subject_train.txt")

#Test Data
x_test<-read.table(".//test//X_test.txt")
y_test<-read.table(".//test//Y_test.txt")
subject_test<-read.table(".//test//subject_test.txt")

#Feature Vector
features<-read.table("features.txt")

#Activity Lables
activityLabels<-read.table("activity_labels.txt")

#Assign column names to the data
colnames(x_train)<-features[,2]
colnames(x_test)<-features[,2]

colnames(y_train)<-"ActivityID"
colnames(y_test)<-"ActivityID"

colnames(subject_train)<-"SubjectID"
colnames(subject_test)<-"SubjectID"

colnames(activityLabels)<-c("ActivityID","Activity")

#Merge the Data
train_merge<-cbind(subject_train,y_train,x_train)
test_merge<-cbind(subject_test,y_test,x_test)
data_all<-rbind(train_merge,test_merge)

# 2. Extract only the measurements on the mean and standard deviation for each measurement.

#Identify which columns are mean or std values
means<-grep("-mean()",colnames(data_all),fixed=TRUE)
sdevs<-grep("-std()",colnames(data_all),fixed=TRUE)
colsKeep<-c(means,sdevs)

#Have to keep the first 2 columns because these are the SubjectID and ActivityID
data_all_mean_std<-data_all[,c(1,2,colsKeep)]

# 3. Uses descriptive activity names to name the activities in the data set
data_all_named<-merge(activityLabels,data_all_mean_std,by="ActivityID",all.x=TRUE)
data_all_named<-data_all_named[,-1]

# 4. Appropriately labels the data set with descriptive variable names.
colnames(data_all_named)<-gsub("^t", "Time", colnames(data_all_named))
colnames(data_all_named)<-gsub("^f", "Frequency", colnames(data_all_named))
colnames(data_all_named)<-gsub("Acc", "Accelerometer", colnames(data_all_named))
colnames(data_all_named)<-gsub("Gyro", "Gyroscope", colnames(data_all_named))
colnames(data_all_named)<-gsub("Mag", "Magnitude", colnames(data_all_named))
colnames(data_all_named)<-gsub("BodyBody", "Body", colnames(data_all_named))
colnames(data_all_named)<-gsub("-", "", colnames(data_all_named))
colnames(data_all_named)<-gsub("mean", "Mean", colnames(data_all_named))
colnames(data_all_named)<-gsub("std", "Std", colnames(data_all_named))
colnames(data_all_named)<-gsub("(", "", colnames(data_all_named),fixed=TRUE)
colnames(data_all_named)<-gsub(")", "", colnames(data_all_named),fixed=TRUE)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Want to group the rows with the same Subject ID and ActivityID, and average the other rows
library(plyr)
data_final<-aggregate(. ~SubjectID + Activity, data_all_named, mean)
data_final<-data_final[order(data_final$Subject,data_final$Activity),]

write.table(data_final,"tidydata.txt",row.names=FALSE,col.names=TRUE)

