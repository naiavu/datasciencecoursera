# "Getting and Cleaning Data" Course Project (Coursera)

## Initial Assignment

>The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that >can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to >submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book >that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should >also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected. 
>
>One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, >Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website >represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the >data was obtained:
>
>http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
>
>Here are the data for the project:
>
>https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
>
> You should create one R script called run_analysis.R that does the following. 
>
>    Merges the training and the test sets to create one data set.
>    Extracts only the measurements on the mean and standard deviation for each measurement. 
>    Uses descriptive activity names to name the activities in the data set
>    Appropriately labels the data set with descriptive variable names. 
>
>    From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each >subject.
>
>Good luck!


## Steps to reproduce the tidy data output

1. The initial ZIP file should be downloaded into the working directory for RStudio and un-zipped there manually. The ZIP file will produce "UCI HAR Dataset" folder within the working folder.
2. The script uses the following libraries: "data.table" and "dplyr". If they are not present, they need to be installed by running the following code in R:
   + install.packages("data.table")
   + library(data.table)
   + install.packages("dplyr")
   + library(dplyr)
3. The script run_analysis.R does the following:
   + Reads initial files to R global environment
   + Combines rows of each Train and Test file sets
   + Combines columns to create one raw dataset 
   + Reads the features file containing variable names for all measurements
   + Assigns names to columns in the combined raw dataset, which are taken from the features file
   + Select the subset of the raw dataset containing only measurements on the mean or standard deviation
   + Reads the activity labels file to merge it with the subset from the above step to produce activity description variable instead of ID 
   + Removes the numeric activity label ID since we now have the actual activity description
   + Makes variable names descriptive and adjusts them, removing reserved R symbols like "-" or "()"
   + Calculates the average for each variable using "aggregate" function grouping by subject and activity, and excluding subject and activity from calculation using dot notation
   + Saving ordered tidy dataset to tidydata.txt file
4. The script may be run from the working directory:
```{r}
source("run_analysis.R")
```
5. To view the result:
```{r}
View(tidydata)
```