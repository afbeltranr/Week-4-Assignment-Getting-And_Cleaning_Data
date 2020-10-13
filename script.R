#Description


# This is a script developed in order to generate a tidy data set from the data provided at 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip' 
#about an experiment performed with the accelerometers from  Samsung Galaxy S, used by 30 volunteers performing different activities. 
# The obtained data sets has been randomly partitioned into two data sets, 70% are in the training data and 30% in the test data.

# Starting point: read data

library(data.table)
library(dplyr)

# First, we download the zip file containing the data sets, if someone who's using this R script hasn't downloaded it yet.

zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

# We only want to download this file if it doesn't exist in the local folder, so we check as follows:

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

# Thus, it only will be downloaded if a file called "UCI HAR Dataset.zip" doesn't exist in the working directory. note the use of the negation '!' operator.

# Then we can unzip the desired file ONLY if there isn't yet a directory called "UCI HAR Dataset.zip"

dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}

# As before, we use the negation '!' operator, so we only unzip a folder called "UCI HAR Dataset" if it doesn't exist.

# First step: read the data


# # read training data
# trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt")) # information about the subjects
# trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))# experimental values obtained
# trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt")) # Activity values

# # read test data
# testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt")) # information about the subjects
# testValues <- read.table(file.path(dataPath, "test", "X_test.txt")) # experimental values obtained
# testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))
# # Activity values


# # we read the features of this experiment

# features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

# # read activity labels

# activities <- read.table(file.path(dataPath, "activity_labels.txt")) #We read the data
# colnames(activities) <- c("activityId", "activityLabel")# Then we assign names to the columns

#----------------------------------------
#-----------STEP 1 -- Merge the test and training data sets to create one data set ----------------
#---------------------------------------


# First, we join the columns of each group, using cbind() as they have the same amount of obsevations (70% for training data and 30% for test data), then we put one table below the other using rbind:

# Activity <- rbind(
#   cbind(trainingSubjects, trainingValues, trainingActivity),
#   cbind(testSubjects, testValues, testActivity)
# )

 # As these are very large data sets, and R saves its objects in the RAM, we can save some memory removing the previous data tables because we now have the table 'Activity' which contains all the information:

# rm(trainingSubjects, trainingValues, trainingActivity, 
#    testSubjects, testValues, testActivity)

# Now, we assign the corresponding names for each column, as the first one is the subject id, then we assign a feature name for each column, and at the end we name the dependent variable 'activity'.
colnames(Activity) <- c("subject", features[, 2], "activity")



#----------------------------------------
#-----------STEP 2 -- Extracts only the measurements on the mean and standard deviation for each measurement ----------------
#---------------------------------------

# we can create a regular expression, which we can use with the grepl() function in order to find the names of the columns that we need.

columnsToKeep <- grepl("subject|activity|mean|std", colnames(Activity))
# Notice that we're using the 'or' '|' operator, so if a value within columnames(Activity) has one of the words "subject|activity|mean|std" it is selected and saved in the object ColumnsToKeep

# Then we rearrange our matrix using only the columns of subject, ativity, mean and std (standard deviation)

Activity <- Activity[, columnsToKeep]

#--------------------------------------------------------------
# Step 3 - Use descriptive activity names to name the activities in the data set-----------------------------------------------------#

# replace activity values with named factor levels

Activity$activity <- factor(Activity$activity, 
  levels = activities[, 1], labels = activities[, 2])

##############################################################################
# Step 4 - Appropriately label the data set with descriptive variable names
##############################################################################

# get column names
ActivityCols <- colnames(Activity)

# remove special characters
ActivityCols <- gsub("[\\(\\)-]", "", ActivityCols)

# expand abbreviations and clean up names
ActivityCols <- gsub("^f", "frequencyDomain", ActivityCols)
ActivityCols <- gsub("^t", "timeDomain", ActivityCols)
ActivityCols <- gsub("Acc", "Accelerometer", ActivityCols)
ActivityCols <- gsub("Gyro", "Gyroscope", ActivityCols)
ActivityCols <- gsub("Mag", "Magnitude", ActivityCols)
ActivityCols <- gsub("Freq", "Frequency", ActivityCols)
ActivityCols <- gsub("mean", "Mean", ActivityCols)
ActivityCols <- gsub("std", "StandardDeviation", ActivityCols)

# correct typo
ActivityCols <- gsub("BodyBody", "Body", ActivityCols)

# use new labels as column names
colnames(Activity) <- ActivityCols

#-----------------------------------------------------------------------------------------------------------------Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
#-------------------------------------------------------------------------------------------------

# group by subject and activity and summarise using mean
ActivityMeans <- Activity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(ActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)

