rm(list=ls())

# Part 1

# Training Data
features <- read.table("features.txt", header = FALSE)
activitylabels <- read.table("activity_labels.txt", header = FALSE)
subjecttrain <- read.table("./train/subject_train.txt", header = FALSE)
xtrain <- read.table("./train/X_train.txt", header = FALSE)
ytrain <- read.table("./train/y_train.txt", header = FALSE)

# Header Names
colnames(activitylabels) <- c("Activity_ID","Activity_Name")
colnames(subjecttrain) <- "Subject_ID"
colnames(xtrain) <- features[,2]
colnames(ytrain) <- "Activity_ID"

# Merge
Train <- cbind(ytrain, subjecttrain, xtrain)

# Test Data
subjecttest <- read.table("./test/subject_test.txt", header = FALSE)
xtest <- read.table("./test/X_test.txt", header = FALSE)
ytest <- read.table("./test/y_test.txt", header = FALSE)

# Header Names
colnames(subjecttest) <- "Subject_ID"
colnames(xtest) <- features[,2]
colnames(ytest) <- "Activity_ID"

# Merge
Test <- cbind(ytest, subjecttest, xtest)

# Merge train and test data
combineddata <- rbind(Train, Test)

# Part 2
selectFeat <- features
templog <- grepl("mean\\(\\)|std\\(\\)", selectFeat[,2])
selectFeat <- data.frame(selectFeat[,2][templog])

tempnum <- features[,1][templog]
tempnum <- c(1, 2,tempnum + 2)

meanstdevdata <- combineddata[,tempnum]

# Part 3

combineddata <- merge(combineddata, activitylabels, by='Activity_ID', all.x=TRUE)
names <- colnames(combineddata)

# Part 4
for (i in 1:length(names)) 
{
  names[i] <- gsub("\\()", "", names[i])
  names[i] <- gsub("-std$", "StDev", names[i])
  names[i] <- gsub("-mean", "Mean", names[i])
  names[i] <- gsub("^(t)", "Time", names[i])
  names[i] <- gsub("^(f)", "Frequency", names[i])
  names[i] <- gsub("([Gg]ravity)", "Gravity", names[i])
  names[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)", "Body", names[i])
  names[i] <- gsub("[Gg]yro", "Gyroscope", names[i])
  names[i] <- gsub("AccMag", "AccelerometerMagnitude", names[i])
  names[i] <- gsub("([Bb]odyaccjerkmag)", "BodyAccelerometerJerkMagnitude", names[i])
  names[i] <- gsub("JerkMag", "JerkMagnitude", names[i])
  names[i] <- gsub("GyroMag", "GyroscopeMagnitude", names[i])
  names[i] <- gsub("Acc", "Accelerometer", names[i])
};

colnames(combineddata) <- names

# Part 5

finalset <- aggregate(combineddata[, names(combineddata) != c("Activity_ID", "Subject_ID")], by = list(combineddata$Activity_ID,combineddata$Subject_ID),mean)

write.table(finalset, "./tidyData.txt", row.names=FALSE, sep="\t")


