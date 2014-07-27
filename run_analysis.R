# this program only invokes one function, run(), which in turn invokes other functions in order to complete a series of steps. Its end result is a tidy dataset called tidy_data.txt

# this function takes a set of file paths, using them to read a feature dataset (training or testing), gives the columns descriptive names by using the features dataset, adds in associated activities as a column from the activities dataset, adds another column with the associated activity labels from the activities label dataset, adds the associated subjects as a column from the activities dataset, and then return the related dataset
build_related_data_set <- function(data_file_path, features_file_path, activities_file_path, activity_labels_file_path, subjects_file_path) {
  # Read data
  data <- read.table(data_file_path)
  
  # Read features
  features <- read.table(features_file_path, col.names = c("feature_id", "feature"))
  
  # Name data columns with features
  colnames(data) <- features$feature
  
  # Read activities
  activities <- read.table(activities_file_path, col.names = c("activity"))
  
  # Add activity class labels column to data
  data$activity <- as.factor(activities$activity)
  
  # Read activity labels
  activity_labels <- read.table(activity_labels_file_path, col.names = c("activity", "activity_label"))
  
  # Add activity name labels column to data
  data_with_activity_labels <- join(x = data, y = activity_labels, by = "activity")
  
  # Read subjects
  subjects <- read.table(subjects_file_path, col.names = c("subject"))
  
  # Make subject a factor
  subjects$subject <- as.factor(subjects$subject)
  
  # Add subjects column to data
  cbind(subjects, data_with_activity_labels)

}

# this function takes the training and testing sets and returns a merged dataset
merge_data_sets <- function(training_data, testing_data) {
  # Bind the two data sets on row
  rbind(training_data, testing_data)
}

# This function takes a dataset, extracts all the variables related to mean and standard deviation, and returns the filtered dataset.
extract_mean_and_std_variables <- function(data, features_file_path) {
  # Read features
  features <- read.table(features_file_path, col.names = c("feature_id", "feature"))
  
  # Create a logical vector of variables that contain "mean" or "std" (as well as the columns added by this program)
  mean_and_std_variables <- grepl("subject|activity|activity_label|mean\\(\\)|std\\(\\)", colnames(data))

  # Subset data with logical vector
  data[,mean_and_std_variables]
}

# this function takes a dataset, groups all the subjects together, then groups all the activities together, calculates a mean across all variables, and return this dataset
produce_data_with_mean_per_feature_per_activity_per_subject <- function(trimmed_data) {
  library(plyr)
  ddply(trimmed_data, .(subject, activity, activity_label), numcolwise(mean))
}

# this function downloads the raw data (if it hasn't been downloaded already), builds the related testing and training datasets, merge them, extract the mean and standard deviation variables, calculate a mean across all features per subject per activity, and write the resulting data set to a file called tidy_data.txt
run <- function() {
    
  if (!file.exists("./UCI HAR Dataset")) {
  
    # download the data
    file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    file_name <- "UCI HAR Dataset.zip"
    download.file(file_url, file_name, method = "curl")
    
    # unzip file
    unzip(file_name)
    
  }
  
  training_data <-build_related_data_set("./UCI HAR Dataset/train/X_train.txt", "./UCI HAR Dataset/features.txt", "./UCI HAR Dataset/train/y_train.txt", "./UCI HAR Dataset/activity_labels.txt", "./UCI HAR Dataset/train/subject_train.txt")
  
  testing_data <- build_related_data_set("./UCI HAR Dataset/test/X_test.txt", "./UCI HAR Dataset/features.txt", "./UCI HAR Dataset/test/y_test.txt", "./UCI HAR Dataset/activity_labels.txt", "./UCI HAR Dataset/test/subject_test.txt")
  
  merged_data <- merge_data_sets(training_data, testing_data)
  
  trimmed_data <- extract_mean_and_std_variables(merged_data, "./UCI HAR Dataset/features.txt")
  
  data_with_mean_per_feature_per_activity_per_subject <- produce_data_with_mean_per_feature_per_activity_per_subject(trimmed_data)
  
  write.table(data_with_mean_per_feature_per_activity_per_subject, "./tidy_data.txt")
  
}

# invokes the main function
run();
