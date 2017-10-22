#run_analysis.R
#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement.
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names.
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#the first step is to get the list of the features that have been analyzed and keep a
#list of the features that should be kept in the second step of the exercise

#read the file that factors in the different features analyzed
features <- read.table('Data Cleaning/Chapter 4/UCI HAR Dataset/features.txt')

#function used to determine if the features will be used in the 2nd step or not
f <- function(x) {
  y = c()
  if (grepl("mean()", x)) {
    y = "keep"
  } else if (grepl("std()", x)) {
    y = "keep"
  } else 
    y = "delete"
}

#apply the function to the list of the features
features$keep <- sapply(features$V2, f)

#keep only the features that will be used in the next steps
features_kept <- filter(features, keep == "keep")

#create another data frame to add "subject" and "activity" that will be used in the next exercise
name_df <- data.frame( "V1" = character(), "V2" = character(), "keep" = character(),stringsAsFactors=FALSE)
name_df[nrow(name_df) + 1, ] <- c( "", "subject", "")
name_df[nrow(name_df) + 1, ] <- c( "", "activity", "")

#add this dataframe to the features_kept exercise
features_kept <- rbind(features_kept,name_df)

#read the detail of the testing performed
test <- read.table('Data Cleaning/Chapter 4/UCI HAR Dataset/test/X_test.txt')

#change the names of the testing in order for it to be more easily readable
names(test) <- features$V2

#read the other tables that will be needed to get the full data frame
subjecttest <- read.table('Data Cleaning/Chapter 4/UCI HAR Dataset/test/subject_test.txt')
activitytest <- read.table('Data Cleaning/Chapter 4/UCI HAR Dataset/test/y_test.txt')

#consolidated the testing dataframe
test_conso <- cbind(subjecttest, activitytest, test)

#change the names of the training in order for it to be more easily readable
train <- read.table('Data Cleaning/Chapter 4/UCI HAR Dataset/train/X_train.txt')

#change the names of the testing in order for it to be more easily readable
names(train) <- features$V2

#read the other tables that will be needed to get the full data frame
subjecttrain <- read.table('Data Cleaning/Chapter 4/UCI HAR Dataset/train/subject_train.txt')
activitytrain <- read.table('Data Cleaning/Chapter 4/UCI HAR Dataset/train/y_train.txt')
train_conso <- cbind(subjecttrain, activitytrain, train)

#consolidated the training dataframe
conso <- rbind(test_conso, train_conso)

#change the names of the 2 first columns of the consolidated data frame
colnames(conso)[1] <- "subject"
colnames(conso)[2] <- "activity"

#select the columns to be kept based on the step 2 of the exercise
listtokeep <- as.character(features_kept$V2)
conso_selected <- conso[listtokeep]

#read the activities table and merge it with the consolidated table
activities <- read.table('Data Cleaning/Chapter 4/UCI HAR Dataset/activity_labels.txt')
conso_selected <- merge(conso_selected, activities, by.x = "activity", by.y = "V1", all = TRUE)

#rename the file and change its structure to make it tydier
conso_selected <- rename(conso_selected, activity_label = V2)
conso_selected$subject <- as.factor(conso_selected$subject)
conso_selected <- select(conso_selected, 'activity_label', 'subject', everything(), -activity)

#create a new and independent tidy data set with the average of each variable for each activity and each subject
summarized_conso <- group_by(conso_selected, activity_label, subject)
summarized_conso <- summarize_all(summarized_conso, mean)
write.csv(summarized_conso, file = "step5_data_cleaning_course.csv")
write.table(summarized_conso, file = "step5_data_cleaning_course.txt", row.name=FALSE)
