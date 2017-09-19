# One of the most exciting areas in all of data science right now is wearable computing - 
# see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing 
# to develop the most advanced algorithms to attract new users. The data linked to from the 
# course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. 
# A full description is available at the site where the data was obtained:
#   
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# 
# Here are the data for the project:
#   
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 
# You should create one R script called run_analysis.R that does the following.
# 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each 
# variable for each activity and each subject.


# 1. Merges the training and the test sets to create one data set.
# read and rbind the subject x and subject y 
subject_train <- read.table("C:/Sanjay.Rini/Rini/Big_Data/RWorkingFolder/UCI HAR Dataset/train/subject_train.txt",header= FALSE, stringsAsFactors=FALSE)
#dim(subject_train) #7352    1
subject_test <- read.table("C:/Sanjay.Rini/Rini/Big_Data/RWorkingFolder/UCI HAR Dataset/test/subject_test.txt",header= FALSE, stringsAsFactors=FALSE)
#dim(subject_test) #2947    1

subject <- rbind(subject_train, subject_test)
#dim(subject) #10299  
#head(subject)
colnames(subject)[1] <- "subject"

# rbind the y train and y test 
y_train <- read.table("C:/Sanjay.Rini/Rini/Big_Data/RWorkingFolder/UCI HAR Dataset/train/y_train.txt",header= FALSE, stringsAsFactors=FALSE)
#dim(y_train) # 7352  11

y_test <- read.table("C:/Sanjay.Rini/Rini/Big_Data/RWorkingFolder/UCI HAR Dataset/test/y_test.txt",header= FALSE, stringsAsFactors=FALSE)
#dim(y_test) #2947    1

y_train_test<- rbind(y_train, y_test)
#dim(y_train_test) #10299  1
#head(y_train_test)
colnames(y_train_test) <- "activityNum"

# read xtrain and xtest files and rbind them

x_train <- read.table("C:/Sanjay.Rini/Rini/Big_Data/RWorkingFolder/UCI HAR Dataset/train/X_train.txt",header= FALSE, stringsAsFactors=FALSE)
dim(x_train) # 7352  561

x_test <- read.table("C:/Sanjay.Rini/Rini/Big_Data/RWorkingFolder/UCI HAR Dataset/test/X_test.txt",header= FALSE, stringsAsFactors=FALSE)
dim(x_test) # 2947  561

x_train_test <- rbind(x_train, x_test)
dim(x_train_test) #10299   561

# cbind the three tables- subject, x_train_test and y_train_test 
mergeddata <- cbind(subject,y_train_test,x_train_test)
View(mergeddata_test)


#reading data in features.txt
features <- read.table("C:/Sanjay.Rini/Rini/Big_Data/RWorkingFolder/UCI HAR Dataset/features.txt",header= FALSE, stringsAsFactors=FALSE, fill = T)
#dim(features) # 561 2
#View(features)


colnames(mergeddata)[1:2] <- c("Subject", "Activity")
## Set names for all other columns, those coming from X, according to features data frame
colnames(mergeddata)[3:563] <- features[, 2]
dim(mergeddata) # 10299   563

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
mergeddata1 <- mergeddata[,grepl("mean()|std()|Subject|Activity",colnames(mergeddata))]
View(mergeddata1) # 10299    81
summary(mergeddata$Activity)
# 3. Uses descriptive activity names to name the activities in the data set
#reading data in features.txt
activities <- read.table("C:/Sanjay.Rini/Rini/Big_Data/RWorkingFolder/UCI HAR Dataset/activity_labels.txt", header= FALSE, stringsAsFactors=FALSE, fill = T)
#View(activities)

# 4. Appropriately labels the data set with descriptive variable names.
#merging activity table to main table-merged data

mergeddata$Activity <- factor(mergeddata$Activity, levels = activities$V1, labels = activities$V2)
View(mergeddata) # 10299   81
dim(mergeddata)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each 
# variable for each activity and each subject.
tidydata <- sapply(mergeddata[,3:563], FUN= mean)
summary(tidydata)
write.csv(tidydata, "C:/Sanjay.Rini/Rini/Big_Data/RWorkingFolder/UCI HAR Dataset//tidydata.txt", row.names = FALSE)

install.packages("knitr")
knit("makeCodebook.Rmd", output="codebook.md", encoding="ISO8859-1", quiet=TRUE)
markdownToHTML("codebook.md", "codebook.html")

install.packages("plyr")
library(ddply)
tidyData <- ddply(mergedData,
                  .(Subject, Activity),
                  .fun=function(x) { colMeans(x[ ,-c(1:2)]) })
