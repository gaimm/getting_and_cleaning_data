setwd('C:\\Users\\gaimm\\OneDrive\\Documents\\GitHub\\getting_and_cleaning_data')
library(dplyr)
# READ EACH FILE
features           <- read.table("UCI HAR Dataset\\features.txt", col.names = c("n","functions"))
head(features); str(features);
x_test        <- read.table("UCI HAR Dataset\\test\\X_test.txt", col.names = features$functions)
head(x_test); str(x_test);
x_train       <- read.table("UCI HAR Dataset\\train\\X_train.txt", col.names = features$functions)
str(x_train)
y_test        <- read.table("UCI HAR Dataset\\test\\y_test.txt", col.names = "code")
str(y_test)
y_train       <- read.table("UCI HAR Dataset\\train\\y_train.txt", col.names = "code")
str(y_train)
subject_test  <- read.table("UCI HAR Dataset\\test\\subject_test.txt", col.names = "subject")
str(subject_test)
subject_train <- read.table("UCI HAR Dataset\\train\\subject_train.txt", col.names = "subject")
str(subject_train)
activities    <- read.table("UCI HAR Dataset\\activity_labels.txt", col.names = c("code", "activity"))
head(activities)                            
 
#1 Merges the training and the test sets to create one data set.
#Combine Training and Test Dataframes
subject_df <- rbind(subject_train, subject_test)
y_df <- rbind(y_train, y_test)
x_df <- rbind(x_train, x_test)
#Merge the above dataframes
merge_df<- cbind(subject_df, y_df, x_df)


#2 Extracts only the measurements on the mean and standard deviation for each measurement.

subset_df <- merge_df %>% select(subject, code, contains("mean"), contains("std"))

#3 Uses descriptive activity names to name the activities in the data set
subset_df$code <- activities[subset_df$code, 2]

#4 Appropriately labels the data set with descriptive variable names.
names(subset_df)[2] = "activity"
names(subset_df)<-gsub("-freq()", "Frequency", names(subset_df), ignore.case = TRUE)
names(subset_df)<-gsub("-mean()", "Mean", names(subset_df), ignore.case = TRUE)
names(subset_df)<-gsub("-std()", "STD", names(subset_df), ignore.case = TRUE)
names(subset_df)<-gsub("Acc", "Accelerometer", names(subset_df), ignore.case = TRUE)
names(subset_df)<-gsub("BodyBody", "Body", names(subset_df), ignore.case = TRUE)
names(subset_df)<-gsub("Gyro", "Gyroscope", names(subset_df), ignore.case = TRUE)
names(subset_df)<-gsub("Mag", "Magnitude", names(subset_df), ignore.case = TRUE)
names(subset_df)<-gsub("^f", "Frequency", names(subset_df), ignore.case = TRUE)
names(subset_df)<-gsub("^t", "Time", names(subset_df), ignore.case = TRUE)
names(subset_df)<-gsub("angle", "Angle", names(subset_df), ignore.case = TRUE)
names(subset_df)<-gsub("gravity", "Gravity", names(subset_df), ignore.case = TRUE)
names(subset_df)<-gsub("tBody", "TimeBody", names(subset_df), ignore.case = TRUE)


dim(subset_df)

#5 From the data set in step 4, creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject.
tidy_df<- subset_df %>%
 group_by(subject, activity) %>%
 summarise_all(funs(mean))

dim(tidy_df)

write.table(tidy_df, "tidy_df.txt", row.name=FALSE)
                            
                            