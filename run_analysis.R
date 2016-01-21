## Project

## You should create one R script called run_analysis.R that does the following.

## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set with the 
##    average of each variable for each activity and each subject.

## create initial data frames for X, y and subject id.

X_test<-read.table("X_test.txt", header = FALSE, sep = "")
subject_test<-read.table("subject_test.txt", header = FALSE, sep = "")
y_test<-read.table("y_test.txt", header = FALSE, sep = "")

## add descriptive labels for activity.

index<-1

for (i in 1:nrow(y_test)) {
  
  if(y_test[index,1]==1) {
    y_test[index,2]="WALKING"
  }
  else if((y_test[index,1])==2) {
    y_test[index,2]="WALKING_UPSTAIRS"
  } 
  else if((y_test[index,1])==3) {
    y_test[index,2]="WALKING_DOWNSTAIRS"
  } 
  else if((y_test[index,1])==4) {
    y_test[index,2]="SITTING"
  } 
  else if((y_test[index,1])==5) {
    y_test[index,2]="STANDING"
  } 
  else { (y_test[index,2]="LAYING")
  }  
  index<-(index+1)
}

## create initial data frames for X, y and subject id.

X_train<-read.table("X_train.txt", header = FALSE, sep = "")
subject_train<-read.table("subject_train.txt", header = FALSE, sep = "")
y_train<-read.table("y_train.txt", header = FALSE, sep ="")

## add descriptive labels for activity.

index<-1

for (i in 1:nrow(y_train)) {
  
  if(y_train[index,1]==1) {
    y_train[index,2]="WALKING"
  }
  else if((y_train[index,1])==2) {
    y_train[index,2]="WALKING_UPSTAIRS"
  } 
  else if((y_train[index,1])==3) {
    y_train[index,2]="WALKING_DOWNSTAIRS"
  } 
  else if((y_train[index,1])==4) {
    y_train[index,2]="SITTING"
  } 
  else if((y_train[index,1])==5) {
    y_train[index,2]="STANDING"
  } 
  else { (y_train[index,2]="LAYING")
  }  
  
  index<-(index+1)
}

## add feature names (column labels)

features<-read.table("features.txt", header = FALSE, sep = "")
feature_names<-features[ ,2]

feature_names_character<-as.character(feature_names)

colnames(X_test)<-(feature_names_character)
colnames(X_train)<-(feature_names_character)
colnames(y_test)<-c("Activity Number", "Activity Name")
colnames(y_train)<-c("Activity Number", "Activity Name")
colnames(subject_test)<-c("Subject ID Number")
colnames(subject_train)<-c("Subject ID Number")

## subset for mean and std

mean_grep <-grep("-mean()", feature_names)
std_grep <-grep("-std()", feature_names)

mean_X_test <- X_test[ , mean_grep]
mean_X_train <- X_train[ , mean_grep]
mean_subset_X <- rbind(mean_X_test, mean_X_train)

std_X_test <- X_test[ , std_grep]
std_X_train <- X_train[ , std_grep]
std_subset_X <- rbind(std_X_test, std_X_train)

data_y <-rbind(y_test, y_train)
data_subject <-rbind(subject_test, subject_train)

## create summary data frame of 10,299 observations across 82 variables

subset_data_Merge <- cbind(data_subject, data_y, mean_subset_X, std_subset_X)

## create Tidy Data Set

data_summary <- aggregate(x = subset_data_Merge, by = list(subset_data_Merge[ ,2], subset_data_Merge[ ,1]), FUN = "mean")
tidy_data_summary <- data_summary[ ,3:84]

## add back activity names

for (i in 1:180) {
  
  if(tidy_data_summary[i,2]==1) {
    tidy_data_summary[i,3]="WALKING"
  }
  else if((tidy_data_summary[i,2])==2) {
    tidy_data_summary[i,3]="WALKING_UPSTAIRS"
  } 
  else if((tidy_data_summary[i,2])==3) {
    tidy_data_summary[i,3]="WALKING_DOWNSTAIRS"
  } 
  else if((tidy_data_summary[i,2])==4) {
    tidy_data_summary[i,3]="SITTING"
  } 
  else if((tidy_data_summary[i,2])==5) {
    tidy_data_summary[i,3]="STANDING"
  } 
  else { (tidy_data_summary[i,3]="LAYING")
  }  
  
}

