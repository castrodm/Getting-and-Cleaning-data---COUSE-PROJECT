#Read files into R
trainingset <- read.table("./UCI Har dataset/train/X_train.txt")
traininglabels <- read.table("./UCI Har dataset/train/y_train.txt")
trainingsubjects <- read.table("./UCI Har dataset/train/subject_train.txt")

testset <- read.table("./UCI Har dataset/test/X_test.txt")
testlabels <- read.table("./UCI Har dataset/test/y_test.txt")
testsubjects <- read.table("./UCI Har dataset/test/subject_test.txt")

features <- read.table("./UCI Har dataset/features.txt")
activitylabels <- read.table("./UCI Har dataset/activity_labels.txt")

#Activity labels: all lower case and no subtitles...
activitylabels$V2 <- sub("_", " ", activitylabels$V2)
activitylabels$V2 <- tolower(activitylabels$V2)

#Name the numbers in the train and test labels as activity number
names(traininglabels) <- "activity number"
names(testlabels) <- "activity number"

#Name subject columns as subject
names(testsubjects) <- "subject"
names(trainingsubjects) <- "subject"

#Name training and test set columns - features (using provided features document)
names(trainingset) <- features$V2
names(testset) <- features$V2

#Helper function to return the activity in the reference activity_labels document
tolabel <- function(x) {
      row <- which(x == activitylabels)  
      activitylabels[row,2]
}

#Append a column "activity": corresponding names to the traning and test labels
traininglabels$activity <- apply(traininglabels, 1, tolabel)
testlabels$activity <- apply(testlabels, 1, tolabel)

#Append the activity labels and subjects to the respective datasets

trainingset <- cbind(trainingsubjects, traininglabels, trainingset)
testset <- cbind(testsubjects, testlabels, testset)


#COURSE PROJECT(1) Merge training and test sets
#Independent measurements (rbind - add rows - both sets and labels)

training_test <- rbind(trainingset, testset)


#COURSE PROJECT(2) Extract mean and standard deviation for each measurement

#Extract column numbers containing std and mean features

stdcols <- grep("std()", names(training_test), fixed = TRUE)
meancols <- grep("mean()", names(training_test), fixed = TRUE)


#COURSE PROJECT(3): column numbers std, mean + 1, 2, 3
#1(subject), 2(activity number), 3 => descriptive activity names!

columnsIwant <- append(c(1,2,3), sort(append(stdcols, meancols)))


#COURSE PROJECT(2, 4): Select only std and mean columns
#appropriately labeled activities...

training_test <- training_test[, columnsIwant]


#COURSE PROJECT(5): Creates a second, independent tidy data set with the 
#average of each variable for each activity and each subject.  

samsungmelt <- melt(training_test, id=c("subject", "activity"), measure.vars=names(training_test)[4:69])
tidydata <- dcast(samsungmelt, subject + activity ~ variable, mean)
write.table(tidydata, file = "./UCI HAR Dataset/IndependentTidyDataset.txt")
