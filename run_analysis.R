#Start with a clean enviornment 
rm(list = ls())

library(dplyr)
library(tidyr)

# BE SURE TO CHANGE YOUR WORKING DIRECTORY
setwd("C:/Users/shiva/Desktop/coding/R/Getting and Cleaning Data/Final Project")

filename <- "Final_Proj_Data.zip"
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#we want the "curl" method passed through download.file so we see how long R takes 
if(!file.exists(filename)){
  download.file(fileURL, filename,method ="curl")} 

if(!file.exists("UCI HAR Dataset")){unzip(filename)}

# Read data into R: 

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code") 

# merge by Row (I find this easier than merging by columns at this stage)
#Step 1: Merge Training and Test Sets together 
X_rows <- rbind(x_train, x_test)
Y_rows <- rbind(y_train, y_test)
Subject_Rows <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject_Rows, Y_rows, X_rows)

#Step 2: Extracts measurements on the mean and SD for each measurement 

Clean_Data <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))

#Step 3 / 4: Uses descriptive activity names 

Clean_Data$code <- activities[Clean_Data$code, 2]

# *Note, I use "gsub" here over "grep" or "grepl" so the matches are replaced programatically 
names(Clean_Data)[2] = "activity"
names(Clean_Data)<-gsub("Acc", "Accelerometer", names(Clean_Data))
names(Clean_Data)<-gsub("Gyro", "Gyroscope", names(Clean_Data))
names(Clean_Data)<-gsub("BodyBody", "Body", names(Clean_Data))
names(Clean_Data)<-gsub("Mag", "Magnitude", names(Clean_Data))
names(Clean_Data)<-gsub("^t", "Time", names(Clean_Data))
names(Clean_Data)<-gsub("^f", "Frequency", names(Clean_Data))
names(Clean_Data)<-gsub("tBody", "TimeBody", names(Clean_Data))
names(Clean_Data)<-gsub("-mean()", "Mean", names(Clean_Data), ignore.case = TRUE)
names(Clean_Data)<-gsub("-std()", "STD", names(Clean_Data), ignore.case = TRUE)
names(Clean_Data)<-gsub("-freq()", "Frequency", names(Clean_Data), ignore.case = TRUE)
names(Clean_Data)<-gsub("angle", "Angle", names(Clean_Data))
names(Clean_Data)<-gsub("gravity", "Gravity", names(Clean_Data))

#Step 5: 

FinalData <- Clean_Data %>%
  group_by(subject, activity) %>%
  summarise_all(list(mean = mean))

write.table(FinalData,"FinalData.txt", row.name = FALSE)

#summary(FinalData) 