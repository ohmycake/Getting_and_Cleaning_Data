##Step 0. Prepares for packages loading and data reading
##------------------------------------------------------
#load  packages
library(plyr)

#read source data
n_features <- read.table(file = "features.txt", header = FALSE, stringsAsFactors = FALSE)
x_train <- read.table(file = "./train/X_train.txt", header = FALSE, stringsAsFactors = FALSE)
x_test <- read.table(file = "./test/X_test.txt", header = FALSE, stringsAsFactors = FALSE)
y_train <- read.table(file = "./train/Y_train.txt", header = FALSE, stringsAsFactors = FALSE)
y_test <- read.table(file = "./test/Y_test.txt", header = FALSE, stringsAsFactors = FALSE)
sub_train <- read.table(file = "./train/subject_train.txt", header = FALSE, stringsAsFactors = FALSE)
sub_test <- read.table(file = "./test/subject_test.txt", header = FALSE, stringsAsFactors = FALSE)


#take a look at the structures of loaded data
str(n_features)
str(x_train)
str(x_test)
str(y_train)
str(y_test)
str(sub_train)
str(sub_test)



##Step 1. Merges the training and the test sets to create one data set
##--------------------------------------------------------------------
#row-combine train and test data
x_data <- rbind(x_train, x_test)
y_data <- rbind(y_train, y_test)
subject <- rbind(sub_train, sub_test)

#add names of combined data
names(x_data) <- n_features[,2]
names(y_data) <- "activity"
names(subject) <- "subject"

#column-combine x_data, y_data, and subject
mydf <- cbind(x_data, y_data, subject)



##Step 2. Extracts only the measurements on the mean and standard deviation for each measurement
##----------------------------------------------------------------------------------------------
ifmeanstd <- grepl("mean\\(\\)|std\\(\\)", n_features[,2])
colneeded <- c(ifmeanstd, TRUE, TRUE)
mydf2 <- subset(mydf, select = colneeded)



##Step 3. Uses descriptive activity names to name the activities in the data set
##------------------------------------------------------------------------------
#read activity_labels
activity_labels <- read.table(file = "activity_labels.txt", header = FALSE, stringsAsFactors = FALSE)
names(activity_labels) <- c("activity", "activity_name")
mydf3 <- join(mydf2, activity_labels, by = "activity")
mydf3 <- mydf3[,-which(names(mydf3)=="activity")]


##Step 4. Uses descriptive activity names to name the activities in the data set
##-------------------------------------------------------------------------------
names(mydf3)
names(mydf3) <- gsub("^t", "Time", names(mydf3))
names(mydf3) <- gsub("^f", "Frequency", names(mydf3))
names(mydf3) <- gsub("Acc", "Accelerometer", names(mydf3))
names(mydf3) <- gsub("Gyro", "Gyroscope", names(mydf3))
names(mydf3) <- gsub("Mag", "Magnitude", names(mydf3))


##Step 5. Creates a second, independent tidy data set with the average of each variable 
##        for each activity and each subject
##-------------------------------------------------------------------------------------
#get means by subject(30) and activity(6) - a total of 180 rows
mydf3mean <- aggregate(.~subject+activity_name, mydf3, mean)

#write the tidy data set into a txt file
write.table(mydf3mean, file = "tidy_data_set.txt", row.names = FALSE)