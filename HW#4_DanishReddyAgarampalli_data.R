library(data.table, reshape2)
library(dplyr)
path <- getwd()
# Reading Subjects
TrainingSubjects <- read.table("train/subject_train.txt") 
TestSubjects <- read.table("test/subject_test.txt") 
#reading Activities (Y_train/_test)

TrainingActivities <- read.table("train/y_train.txt") 
TestActivities <- read.table("test/y_test.txt")

#reading X_test and X-train

TrainingMeasure <- read.table("train/X_train.txt") 
TestMeasure <- read.table("test/X_test.txt")

activity_labels <- read.table("activity_labels.txt")
features <- read.table("features.txt")  

dataSet <- rbind(TrainingMeasure,TestMeasure)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# Create a vector of only mean and std, use the vector to subset.
MeanStdOnly <- grep("(mean|std)\\(\\)", features[, 2]) 
dataSet <- dataSet[,MeanStdOnly]


# 3. Uses descriptive activity names to name the activities in the data set
# group the activity column of dataSet, re-name lable of levels with activity_levels, and apply it to dataSet.
act_group <- factor(dataSet$activity)
levels(act_group) <- activity_labels[,2]
dataSet$activity <- act_group


# 4. Appropriately labels the data set with descriptive activity names.
# Create vector of "Clean" feature names by getting rid of "()" apply to the dataSet to rename labels.
CleanFeatureNames <- sapply(features[, 2], function(x) {gsub("[()]", "",x)})
names(dataSet) <- CleanFeatureNames[MeanStdOnly]

# combine test and train of subject data and activity data, give descriptive lables

subject <- rbind(TrainingSubjects, TestSubjects)
names(subject) <- 'subject'
activity <- rbind(TrainingActivities, TestActivities)
names(activity) <- 'activity'

# combine subject, activity, and mean and std only data set to create final data set.
dataSet <- cbind(subject,activity, dataSet)


transformData <- melt(dataSet,(id.vars=c("subject","activity")))
newDataSet <- dcast(transformData, subject + activity ~ variable, mean)

names(newDataSet)[-c(1:2)] <- paste("[mean of]" , names(newDataSet)[-c(1:2)] )
write.table(newDataSet, "HW#4_DanishReddyAgarampalli_data.csv", sep = ",")
