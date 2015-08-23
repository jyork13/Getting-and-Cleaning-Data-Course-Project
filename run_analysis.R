########################################################################################################
#
# 0) Load libraries, Retrieve dataset and unzip.
#
########################################################################################################

# Load libraries ()
# packages function checks to see if package is installed and if not, installs
#       (got this from http://stackoverflow.com/questions/9341635/check-for-installed-packages-before-running-install-packages)
packages<-function(x){
        x<-as.character(match.call()[[2]])
        if (!require(x,character.only=TRUE)){
                install.packages(pkgs=x,repos="http://cran.r-project.org")
                require(x,character.only=TRUE)
        }
}
packages(dplyr)
packages(tidyr)
#library(dplyr)
#library(tidyr)

# Initialize url and directory path and set working directory
fileurl <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
iwd <- "~/../RStudio/CleanData"
setwd(iwd)

# Get file, unzip and set working directory to directory with unzipped files
if (!file.exists("data")){dir.create("data")} #Create data directory if it doesn't exist
if (!file.exists("./data/UCI HAR Dataset")){
        download.file(fileurl, destfile="./data/Dataset.zip", mode='wb')
        unzip("./data/Dataset.zip", exdir = "./data")
        }
setwd("./data/UCI HAR Dataset")

########################################################################################################
#
# 1) Merges the training and the test sets to create one data set.
#
########################################################################################################

# Read feature (variable) names from features.txt file
col_names <- read.table("./features.txt", header = FALSE, stringsAsFactors = FALSE, col.names = c("FeatureID", "FeatureName"))

# Replace "()-" in column names with underscore (in preparation for tidy data step later)
col_names$FeatureName <- sub("()-", "_", col_names$FeatureName, fixed = T)

# Load training files
#       train_x contains the actual train data (7352 rows and 561 columns)
#       train_y contains the ActivityID (number from 1-6)
#       subject_train contains the subect (person volunteer) that took measurements (number 1-30)
train_x <- read.table("./train/X_train.txt", header = FALSE, col.names = col_names$FeatureName)
train_y <- read.table("./train/y_train.txt", header = FALSE, col.names = c("ActivityID"))
subject_train <- read.table("./train/subject_train.txt", header = FALSE, col.names = c("SubjectID"))

# Load test Files
#       test_x contains the actual test data
#       test_y contains the ActivityID (number from 1-6)
#       subject_test contains the subect (person volunteer) that took measurements (number 1-30)
test_x <- read.table("./test/X_test.txt", header = FALSE, col.names = col_names$FeatureName)
test_y <- read.table("./test/y_test.txt", header = FALSE, col.names = c("ActivityID"))
subject_test <- read.table("./test/subject_test.txt", header = FALSE, col.names = c("SubjectID"))

# Consolodate all data frames into one "untidy" data set 
test <- cbind(test_x, subject_test, test_y)
train <- cbind(train_x, subject_train, train_y)
untidy_data <- rbind(train, test)


########################################################################################################
#
# 2) Extracts only the measurements on the mean and standard deviation for each measurement. 
#
########################################################################################################

# Create a vector to index which columns contain "mean" or "std" (including SUbject and Activity)
# Assumed these were only variables with mean(): Mean value std(): Standard deviation which is
#       why in step 1 I replaced Mean()- with Mean_ and Std()- with Std_
col_index <- grep("mean_|std_|SubjectID|ActivityID", names(untidy_data))

# Extract mean and std columns (and keep all rows)
# Results in a 10299 x 50 data frame
untidy_data <- untidy_data[, col_index]

########################################################################################################
#
# 3) Uses descriptive activity names to name the activities in the data set.
#
########################################################################################################

# Load activity labels from activity_labels.txt
activity_labels <- read.table("./activity_labels.txt", header = FALSE, col.names = c("ActivityID", "ActivityName"))

# Join untidy_data and activity_labels by ActividyID, then drop AcivityID column
# note: can use join instead of merge to keep from reordering when using merge
untidy_data <- join(untidy_data, activity_labels)
untidy_data$ActivityID <- NULL

########################################################################################################
#
# 4) Appropriately labels the data set with descriptive variable names.
#
########################################################################################################

# Already added column names in step 1
# will replace with more descriptive names
# NOTE: Purposely left "." and "_" in name for use with separate command in tidy (Step 5)
funky_names <- names(untidy_data)
# Leading t with Time
descriptive_names <- sub("^t", "Time", funky_names)
# Leading f with Frequency
descriptive_names <- sub("^f", "Frequency", descriptive_names)
# Acc with Accelerometer
descriptive_names <- sub("Acc", "Accelerometer", descriptive_names)
# Gyro with Gyroscope
descriptive_names <- sub("Gyro", "Gyroscope", descriptive_names)
# Replace data frame names with descriptive_names vector
colnames(untidy_data) <- descriptive_names

########################################################################################################
#
# 5) From the data set in step 4, creates a second, independent tidy data set with
#    the average of each variable for each activity and each subject.
#
########################################################################################################

# Create the tidy data frame (see http://vita.had.co.nz/papers/tidy-data.pdf by Hadley Wickam) for background
# Chose to use gather/separate to split the variable name into Signal, Stat and Axis 
tidydata <- untidy_data %>%
        gather(signal_stat_axis, Value, -SubjectID, -ActivityName) %>%
        separate(signal_stat_axis , c("Signal", "Stat", "Axis")) %>%
        group_by(SubjectID, ActivityName, Signal, Stat, Axis) %>%
        summarise_each(funs(mean))

write.table(tidydata, file="tidy_data.txt", row.name=FALSE) 
?write.table
