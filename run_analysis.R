#read the files
tf <- tempfile()
td <- tempdir()
zip.file.location <- 
  "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(zip.file.location,
              tf,
              method = "curl")

file.name <- unzip(tf, exdir = td)
act_lab <- read.table(file.name[1])

#1.merege test and traning data
#1.1 merge subject_train, X_train and Y_train into one file, called train
##and use descriptive activity names
train_labels <- read.table(file.name[28])
names(train_labels)[names(train_labels) == "V1"] <- "activity_labels"
train_labels$activity <- act_lab$V2[match(train_labels$activity_labels, act_lab$V1)]
train_sub <- read.csv(file.name[26])
names(train_sub)[names(train_sub) == "X1"] <- "subject_id"
add_sub <- data.frame(NA)
names(add_sub) <- "subject_id"
train_sub <- rbind(train_sub, add_sub)
train_set <- read.table(file.name[27])
names(train_set) <- sub("V", "time_frequency_domain", names(train_set), )
train <- cbind(train_sub, train_labels, train_set)

#1.2 merge subject_test, X_test and Y_test into one file, called test
##and use descriptive activity names
test_sub <- read.table(file.name[14])
test_set <- read.table(file.name[15])
test_labels <- read.table(file.name[16])
names(test_sub)[names(test_sub) == "V1"] <- "subject_id"
names(test_labels)[names(test_labels) == "V1"] <- "activity_labels"
names(test_set) <- sub("V","time_frequency_domain", names(test_set), )
test_labels$activity <- act_lab$V2[match(test_labels$activity_labels, act_lab$V1)]
test <- cbind(test_sub, test_labels, test_set)

#1.3 merge train and test into train_test with 10299 obsevations and 564 variables
train_test <- rbind(train, test)

#2.1 create a new column for rowmeans
str(train_test)
rowMeans(train_test[, -(1:3)])
str(rowMeans(train_test[, -(1:3)]))
train_test$row_mean <- rowMeans(train_test[, -(1:3)])
#due to column1,2,3 are not measurements, I remove them from the calculation.

#2.2 create a new column for row sd
transform(train_test, SD=apply(train_test,1,sd, na.rm = TRUE ))
train_test <- transform(train_test, SD=apply(train_test,1,sd, na.rm = TRUE ))
# therefore, train_test has two new variables, row_mean and SD.
#there are total 566 variables in train_test.

##3. I have added activity column in my data set at step 1 so it is easy to read which activity the subject enrol.
##4. according to README_text, each record means a 561-feature vector with time and frequency domain variables. 
## therefore, I named the variables as time_frequency_domain1 to time_frequency_domain561.

##5 create a tidy data with the average of each variable for each activity and each subject
##5.1 generate a data fram with averageTFD (time_frequency_domain) based on each acitivity and per subject id
meansofsomeRows_5 <- function(input.df, id = 1:30){
  input.test.df <- subset(input.df,subject_id == id)
  activity_labels <- c(1,2,3,4,5,6)
  n <- length(activity_labels)
  activity_id <- numeric(n)
  averageTFD <- numeric(n)
  running.index <- 0
  for (j in 1:6) {
    y <- activity_labels[j]
    right_act <- input.test.df[which(input.test.df[, 2] == j), ]
    right_number <- as.numeric(unlist(right_act[, -(1:3)]))
    x <-mean(right_number)
    running.index <- running.index+1
    activity_id[running.index] <- y
    averageTFD[running.index] <- x
    subject_id <- input.test.df$subject_id[1:6]
  }
  result <- data.frame(subject_id, activity_id, averageTFD)
  return(result)
}
#train_test has 566 variables with row means and sd, which I would need to delete these two columns
#because they are not needed for step5 average calculation.
n_train_test <- train_test[,-(565:566)]
meansofsomeRows_5(n_train_test,id = 1)
meansofsomeRows_5(n_train_test,id = 2)
##5.2 rbind each subject_id data frame
whole_df <- data.frame()
for (k in 1:30) {
  raw_df <- meansofsomeRows_5(n_train_test, k)
  whole_df <- rbind(whole_df, raw_df)
}
##5.3 add activity and change column order as subject_id, activity_id, activity and average TFD
whole_df$activity <- act_lab$V2[match(whole_df$activity_id, act_lab$V1)]
whole_df <- whole_df[, c(1,2,4,3)]
##5.4 save the tidy data
write.table(whole_df, file = "tidy_data")
read.table("tidy_data", header = TRUE)


