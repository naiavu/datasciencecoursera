#########################################################################################
# Step 1 - Load all data and merge the training and the test sets to create one data set.
#########################################################################################

# Read Feature files
featureTrain <- read.table("UCI HAR Dataset/train/x_train.txt", header = FALSE)
featureTest  <- read.table("UCI HAR Dataset/test/X_test.txt" , header = FALSE)

# Read Activity files
activityTrain <- read.table("UCI HAR Dataset/train/Y_train.txt", col.names=c("activityID"))
activityTest  <- read.table("UCI HAR Dataset/test/Y_test.txt", col.names=c("activityID"))

# Read Subject files
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names=c("subject"))
subjectTest  <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names=c("subject"))

# Combine rows of each Train and Test file sets
featureSet <- rbind(featureTrain, featureTest)
activitySet <- rbind(activityTrain, activityTest)
subjectSet <- rbind(subjectTrain, subjectTest)

# Combine columns to create one raw dataset 
rawData <- cbind(featureSet, subjectSet, activitySet)

################################################################################################
# Step 2 - Extract only the measurements on the mean and standard deviation for each measurement
################################################################################################

# Read the feature list file containing variable names for all measurements
featureList <- read.table("UCI HAR Dataset/features.txt", col.names = c("id", "name"))

# Create a vector to hold the names for all measurements, as well as
# "subject" and "activityID" variables to be used later in final merging and calculation
varNames <- c(as.character(featureList[, "name"]), "subject", "activityID")

# Assign names to columns in rawData
names(rawData) <- varNames

# Select only measurements on the mean or standard deviation
featureSubset <- featureList$name[grep("mean\\(\\)|std\\(\\)", featureList$name)]

# Create a vector to hold a filtered subset of the variable names
selectedNames <- c(as.character(featureSubset), "subject", "activityID" )

# Select subset of the rawData with specified columns/variables
rawDataSubset <- rawData[, selectedNames]

#################################################################################
# Step 3 - Uses descriptive activity names to name the activities in the data set
#################################################################################

# Read activities label file
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names=c("id", "activity"))

# Merge activity labels with the filtered raw data
rawDataSet <- merge(rawDataSubset, activityLabels, by.x = "activityID", by.y = "id")

# Drop the numeric label ID since we now have the actual activity description
rawDataSet <- rawDataSet[, !(names(rawDataSet) %in% c("activityID"))]

#############################################################################
# step 4 - Label the data set with descriptive variable names.
#############################################################################

# Make variable names descriptive and adjust them according R variable naming
names(rawDataSet) <- gsub("^t", "time", names(rawDataSet))
names(rawDataSet) <- gsub("^f", "frequency", names(rawDataSet))
names(rawDataSet) <- gsub("Acc", "Accelerometer", names(rawDataSet))
names(rawDataSet) <- gsub("Gyro", "Gyroscope", names(rawDataSet))
names(rawDataSet) <- gsub("Mag", "Magnitude", names(rawDataSet))
names(rawDataSet) <- gsub("BodyBody", "Body", names(rawDataSet))
names(rawDataSet) <- gsub("-mean", "Mean", names(rawDataSet))
names(rawDataSet) <- gsub("-std", "STD", names(rawDataSet))
names(rawDataSet) <- gsub("[()-]", "", names(rawDataSet))

###################################################################################################################
# step 5 - Create an independent tidy data set with the average of each variable for each activity and each subject.
###################################################################################################################

# To calculate the average for each variable the "aggregate" function is used with convinient dot notation, 
# which means to execute "MEAN" function the for all other variables not present in the formula 
# (e.g. all except subject + activity), and grouped by subject + activity

tidyData <- aggregate(. ~subject + activity, rawDataSet, FUN = "mean")

# Order the dataset by subject, then by activity
tidyData <- tidyData[order(tidyData$subject,tidyData$activity),]

# Save to TXT file
write.table(tidyData, file = "tidydata.txt",row.name=FALSE)

