setwd("~/My R Files/Coursera/3 - Getting and Cleaning Data")

# Step 1: Merge training and test data sets

data.train <- read.table(".//getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
dim(data.train) 
# data.train is 7352 by 561
data.test <- read.table(".//getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
dim(data.test) 
# data.test is 2947 by 561
data.merged <- rbind(data.train, data.test)
dim(data.merged) 
# data.merged is 10299 by 561

label.train <- read.table(".//getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
table(label.train)
label.test <- read.table(".//getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt") 
table(label.test)
label.merged <- rbind(label.train, label.test)

subject.train <- read.table(".//getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
subject.test <- read.table(".//getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
subject.merged <- rbind(subject.train, subject.test)
names(subject.merged) <- "subject"

# Step 2: Extract the mean and SD 

data.features <- read.table(".//getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt")
dim(data.features)  
# data.features is 561 by 2

data.meansd <- grep("mean\\(\\)|std\\(\\)", data.features[, 2])

data.final <- data.merged[, data.meansd]
dim(data.final) 
# data.final is 10299 by 66

names(data.final) <- gsub("\\(\\)", "", data.features[data.meansd, 2])
names(data.final) <- gsub("-", "", names(data.final))
names(data.final) <- tolower(names(data.final))

# Step 3: Give the activities descriptive names

label.activity <- read.table(".//getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")
label.activity[, 2] <- gsub("_", " ", label.activity[, 2])
label.activity[, 2] <- tolower(label.activity[, 2])
label.activity.merged <- label.activity[label.merged[, 1], 2]
label.merged[, 1] <- label.activity.merged
names(label.merged) <- "activity"

# Step 4: Label the data set with the descriptive activity names

data.clean <- cbind(subject.merged, label.merged, data.final)
dim(data.clean)
# data.clean is 10299 by 68

write.table(data.clean, "mergedtraintest.txt")

# Step 5. Create tidy data with the avg for each variable 
# for each activity and each subject
length.subject <- length(table(subject.merged))
length.activity <- dim(label.activity)[1]
length.column <- dim(data.clean)[2]
data.tidy <- matrix(NA, nrow=length.subject*length.activity, ncol=length.column) 
data.tidy <- as.data.frame(data.tidy)
colnames(data.tidy) <- colnames(data.clean)
populate <- 1
        for(i in 1:length.subject) {
                for(j in 1:length.activity) {
                        data.tidy[populate, 1] <- sort(unique(subject.merged)[, 1])[i]
                        data.tidy[populate, 2] <- label.activity[j, 2]
                        a <- i == data.clean$subject
                        b <- label.activity[j, 2] == data.clean$activity
                        data.tidy[populate, 3:length.column] <- colMeans(data.clean[a&b, 3:length.column])
                        populate <- populate + 1
                }
        }

write.table(data.tidy, "avgbyactivitybysubject.txt",row.name=FALSE)