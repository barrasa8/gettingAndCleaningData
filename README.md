* Title: Readme for run_analysis.R
* author: Ismael Oltra
* date: 16 June 2015
* output: run_analysis_output.txt

 
## Project Description
This is an assignment for Getting and Cleaning Data.  It involves analysing a series of collected smartphone observations.
 
##Study design and data processing
 
###Collection of the raw data
The data used was collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here is the data for the project: 
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
 
###Notes on the original (raw) data 
Refer to readme.txt in dataset for further information on original dta set
 
##Creating the tidy datafile
 
###Guide to create the tidy data file
1. Download data from above URL and unpack
2. Open run_analysis.R
3. Set path variable in run_analysis.R to appropriate location of unpacked data set
4. source run_analysis.R 
 
###Cleaning of the data
Train & test data were loaded and combined with the subject identifier.  The activity ID (integer) was converted to a textual activity description (e.g. walking) to make it more readable.

The original set of vectors were reduced to only those that are mean or standard deviations.  The observations were then summarised to show the mean of each vector by activity description and subject.

##Description of the variables
Refer to the codebook.md for variable definition information
