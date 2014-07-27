# this is the test suite for the main program. It runs automated tests to assert that the course goals are achieved by executing the program. If this suite runs without any errors, it can be assumed that the program achieves its goals. To get prettier output, source this file and run test_file("test_analysis.R")

library('testthat')
source("run_analysis.R")

# generate intermediate data frames for testing purposes

training_data <-build_related_data_set("./UCI HAR Dataset/train/X_train.txt", "./UCI HAR Dataset/features.txt", "./UCI HAR Dataset/train/y_train.txt", "./UCI HAR Dataset/activity_labels.txt", "./UCI HAR Dataset/train/subject_train.txt")

testing_data <- build_related_data_set("./UCI HAR Dataset/test/X_test.txt", "./UCI HAR Dataset/features.txt", "./UCI HAR Dataset/test/y_test.txt", "./UCI HAR Dataset/activity_labels.txt", "./UCI HAR Dataset/test/subject_test.txt")

merged_data <- merge_data_sets(training_data, testing_data)

trimmed_data <- extract_mean_and_std_variables(merged_data, "./UCI HAR Dataset/features.txt")

data_with_mean_per_feature_per_activity_per_subject <- produce_data_with_mean_per_feature_per_activity_per_subject(trimmed_data)

# start tests

test_that("the training and the test sets are merged to create one data set", {
  expect_that(nrow(merged_data), equals(nrow(training_data) + nrow(testing_data)))
})

test_that("that only the measurements on the mean and standard deviation for each measurement are extracted", {
  expect_that(all(grepl("subject|activity|activity_label|mean\\(\\)|std\\(\\)", colnames(trimmed_data))), is_true())
})

test_that("that descriptive activity names to name the activities in the data set are used", {
  expect_that(colnames(trimmed_data)[2], equals("tBodyAcc-mean()-X"))
  expect_that(all(grepl("subject|activity|activity_label|mean\\(\\)|std\\(\\)", colnames(trimmed_data))), is_true())
})

test_that("that the data set is appropriately labbeled with descriptive variable names", {
  expect_that(colnames(trimmed_data)[1], equals("subject"))
  expect_that(all(grepl("subject|activity|activity_label|mean\\(\\)|std\\(\\)", colnames(trimmed_data))), is_true())
})

test_that("that a second, independent tidy data set with the average of each variable for each activity and each subject is created", {
  run()
  tidy_data <- read.table("./tidy_data.txt")
  expect_that(nrow(tidy_data), equals(180))
})

test_that("that a code book exists", {
  expect_that(file.exists("./code_book.MD"), is_true())
})

test_that("that a README exists", {
  expect_that(file.exists("./README.MD"), is_true())
})