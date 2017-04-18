
#Downloading the zip file and getting the data into R.

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, "data.zip")
unzip("data.zip")

activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")
x_training <- read.table("UCI HAR Dataset/train/X_train.txt")
y_training <- read.table("UCI HAR Dataset/train/y_train.txt")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_training <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

unlink("data.zip")

#Column names are specified in training and test data.

colnames(x_training) <- t(features[2])
colnames(x_test) <- t(features[2])

#Activity and participant columns are added to training and test data.

x_training <- cbind(x_training, activity = y_training[, 1], 
                    participant = subject_training[, 1])
x_test <- cbind(x_test, activity = y_test[, 1], participant = subject_test[, 1])

#Training and test data are merged.

data <- rbind(x_training, x_test)
data <- data[, !duplicated(colnames(data))]

#Functions of dplyr package are applied in the rest of the assignment.

library(dplyr)

#Only the measurements on the mean and standard deviation for each measurement are extracted. 

data_mn_std <- data %>% select(participant, activity, contains("mean()"), contains("std()"))

#Activities in the data set are renamed with descriptive activity names. 

data_mn_std$activity <- recode(as.character(data_mn_std$activity), "1" = "WALKING", "2" = "WALKING_UPSTAIRS", 
                        "3" = "WALKING_DOWNSTAIRS", "4" = "SITTING",
                        "5" = "STANDING", "6" = "LAYING" )

#Variable names are changed with more appropriate ones.

names(data_mn_std) <- gsub("Acc", "Accelerator", names(data_mn_std))
names(data_mn_std) <- gsub("Gyro", "Grroscope", names(data_mn_std))
names(data_mn_std) <- gsub("Mag", "Magnitude", names(data_mn_std))
names(data_mn_std) <- gsub("^t", "Time", names(data_mn_std))
names(data_mn_std) <- gsub("^f", "Frequency", names(data_mn_std))

#A new data set is created from the previous one
#by taking averages of each variable for each activity and each subject.

ave_data_mn_std <- data_mn_std %>% group_by(participant, activity) %>% summarise_each(funs(mean))

#The tidy data set created is saved as a txt file.

write.table(ave_data_mn_std, "ave_data_mn_std.txt", row.names = FALSE)
