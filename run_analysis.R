## Set working directory.
setwd("getdatacourproj/ucihardataset")


## Read information from text files.
xtest <- read.table("test/X_test.txt", header = FALSE)
xtrain <- read.table("train/X_train.txt", header = FALSE)
ytest <- read.table("test/y_test.txt", header = FALSE)
ytrain <- read.table("train/y_train.txt", header = FALSE)
features <- read.table("features.txt", row.names = 1, header = FALSE)
subtest <- read.table("test/subject_test.txt", header = FALSE)
subtrain <- read.table("train/subject_train.txt", header = FALSE)
actlabels <- read.table("activity_labels.txt", header = FALSE, col.names = c("activity", "actname"))


## Combine training and test data
xtraintest <- rbind(xtrain, xtest)
ytraintest <- rbind(ytrain, ytest)
subtraintest <- rbind(subtrain, subtest)


## Create column labels
extracollabels <- data.frame(V2 = c("subject", "activity"))
featureslabels <- rbind(extracollabels, features)


## Keep only columns with mean and std
keepcolumns <- vector()
for(i in featureslabels[1:length(featureslabels)]) {
	keepcolumns <- c(keepcolumns, (grepl("mean", i) | grepl("std", i) | grepl("subject", i) | grepl("activity", i) == TRUE))
}
keepcolumns <- c(1:563) * keepcolumns
keepcolumns <- keepcolumns[keepcolumns != 0]


## Bind tables and create a table with only mean and std columns
table1 <- cbind(subtraintest, ytraintest, xtraintest)
table2 <- table1[keepcolumns]


## Make column labels legal
featureslabels1 <- featureslabels[keepcolumns, ]
featureslabels1 <- gsub("\\(", "", featureslabels1)
featureslabels1 <- gsub("\\)", "", featureslabels1)
featureslabels1 <- gsub("\\-", "", featureslabels1)


## Assign legal column names to table2
colnames(table2) <- featureslabels1


##Install plyr & dplyr package if not already installed (credit to Tim in https://class.coursera.org/getdata-008/forum/thread?thread_id=247)
if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")};library(plyr)
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")};library(dplyr)


## Assign descriptive activities names
library(plyr)
table3 <- join(table2, actlabels, by = "activity")
table3 <- table3[c(1, 2, 82, 3:81)]

## Write table with above data to working directory
write.table(table3, file = "../actandvarall.txt", row.names = FALSE)

## Use group_by() and summarise_each() to get averages for each activity and subject
table4 <- group_by(table3, subject, activity, actname)
table4 <- table4 %>% summarise_each(funs(mean))

## Write table with above (summarized) data to working directory
write.table(table4, file = "../actandvarmeans.txt", row.names = FALSE)