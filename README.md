# Coursera Getting and Cleaning Data course project

## Introduction
The goal of this project was to give students an opportunity to collect, work with and clean data to ultimately produce a tidy dataset. For the purposes of this project, the definition of tidy data is assumed to mean the following ([Wickham](http://vita.had.co.nz/papers/tidy-data.pdf)):

- each variable forms a column.
- each observation forms a row.
- each type of observational unit forms a table.

To this end, the student was expected to create an R program that does the following:

- Merges the training and the test sets to create one data set.
- Extracts only the measurements on the mean and standard deviation for each measurement. 
- Uses descriptive activity names to name the activities in the data set
- Appropriately label the data set with descriptive variable names. 
- Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

In addition, the student was also required to:

- Create a code book describing the variables and transformations that produced the variables in the tidy dataset.
- Create a README document describing how everything fits together (this document).

## About the raw data
The raw dataset consists of data collected in a study using the accelerometers of Samsung Galaxy S smartphones. A full description of the study is available [here](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones). The raw data is available for download [here](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip). The main components of the raw dataset were (see code_book.MD for more information):

- `X_train.txt` and `X_test.txt`: contain all the measurement data.
- `y_train.txt` and `y_test.txt`: contain an activity code for each observation of the measurement data.
- `subject_train.txt` and `subject_test`: contain a subject code for each observation of the measurement data.
- `features.txt`: contains information about each measurement variable contained in X_train.txt and X_test.txt.
- `activity_lables.txt`: contains a descriptive label for each activity code.

## Usage

The program consists of the following files:

- `run_analysis.R`: the main program.
- `test_analysis.R`: a test suite that tests whether the main program satisfies the course objectives (outlined above).

To run the program, open R and run `source("run_analysis.R")`. The program will produce a tidy dataset in a text file called `tidy_data.txt`.

To run the test suite, open R, import the `testthat` library with `source(testthat)` and then run the test suite with `test_file("test_analysis.R")` (alternatively you can directly source the test suite with `source("test_analysis.R")`; if the program runs without any errors all tests have passed).

## Program

`run_analysis.R` invokes one function, `run()`, which in turn invokes other functions in order to complete a series of steps. Its end result is a tidy dataset called `tidy_data.txt`.

`run()` executes the following series of steps:

1. downloads the raw data (if it hasn't been downloaded already).
2. invokes `build_related_data_set()`, which takes a set of file paths, using them to read a feature dataset (training or testing), gives the columns descriptive names by using the features dataset, adds in associated activities as a column from the activities dataset, adds another column with the associated activity labels from the activities label dataset, adds the associated subjects as a column from the activities dataset, and then return the related dataset. `build_related_data_set()` is invoked twice: once for the training data and once for the testing data.
3. invokes `merge_data_sets()` with the two results of `build_related_data_set()` as parameters, which returns a merged dataset.
4. invokes `extract_mean_and_std_variables()` with the result of `merge_data_sets()` as parameter, which extracts all the variables related to mean and standard deviation, and returns the filtered dataset.
5. invokes `produce_data_with_mean_per_feature_per_activity_per_subject()` with the result of `extract_mean_and_std_variables()` as parameter, which groups all the subjects together, then groups all the activities together, calculates a mean across all variables, and return this dataset.
6. writes the result of `produce_data_with_mean_per_feature_per_activity_per_subject()` to a text file called `tidy_data.txt`.

## Proof or originality
This [image](https://dl.dropboxusercontent.com/u/4900458/Coursera%20proof%20of%20originality/Getting%20and%20Cleaning%20Data%20course%20project.png) in my Dropbox account aims to serve as proof of the originality of my work.