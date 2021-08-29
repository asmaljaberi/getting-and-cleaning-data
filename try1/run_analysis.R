log <- function(...) {
  cat("[run_analysis.R] ", ..., "\n", sep="")
}

codebook <- function(...){
  cat(..., "\n",file=targetCodebookFilePath,append=TRUE, sep="")
}

# Load the needed packages
packages <- c("data.table", "reshape2", "dplyr")
sapply(packages, require, character.only=TRUE, quietly=TRUE)


#path <- getwd()

#data
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dataDir <- "required_data"
targetZipFile <- "Dataset.zip"
zipDir <- "UCI HAR Dataset"
#extractedZipPath <- file.path(getwd(), zipDir)

targetZipFilePath <- file.path(getwd(), targetZipFile)

#codebook
targetCodebookFilePath <- file.path(getwd(),"CodeBook.md")
file.remove(targetCodebookFilePath)
codebook("# Code Book")
codebook("generated ",as.character(Sys.time())," during sourcing of `run_analysis.R`")
codebook("")  

# unzip if not already exists
extractedZipPath <- file.path(getwd())


#codebook("* extracting zip file: `",targetZipFilePath,"` to `",zipDir,"`")

if(!file.exists(extractedZipPath)){
  log("unzip file: `",targetZipFilePath, "` to `",zipDir,"`")
  unzip(targetZipFilePath, exdir = extractedZipPath, overwrite=TRUE)
  file.rename(zipDir, dataDir)
}else{
  log("zip file already extracted to: `",dataDir,"`")
}


dirList <- list.files(extractedZipPath, recursive=TRUE)


# Give warning to set the working directory if not able to find data files.
projectDataPath <- file.path("required_data")
fileCount <- length(list.files(projectDataPath, recursive=TRUE))

# Read in the 'Subject' data
dtTrainingSubjects <- fread(file.path(projectDataPath, "train", "subject_train.txt"))
dtTestSubjects  <- fread(file.path(projectDataPath, "test" , "subject_test.txt" ))

# Read in the 'Activity' data
dtTrainingActivity <- fread(file.path(projectDataPath, "train", "Y_train.txt"))
dtTestActivity  <- fread(file.path(projectDataPath, "test" , "Y_test.txt" ))

# Read in the 'Measurements' data
dtTrainingMeasures <- data.table(read.table(file.path(projectDataPath, "train", "X_train.txt")))
dtTestMeasures  <- data.table(read.table(file.path(projectDataPath, "test" , "X_test.txt")))

# Row merge the Training and Test Subjects
dtSubjects <- rbind(dtTrainingSubjects, dtTestSubjects)
setnames(dtSubjects, "V1", "subject")

# Row merge the Training and Test Activities
dtActivities <- rbind(dtTrainingActivity, dtTestActivity)
setnames(dtActivities, "V1", "activityNumber")

# Merge the Training and Test 'Measurements' data
dtMeasures <- rbind(dtTrainingMeasures, dtTestMeasures)

# Column merge the subjects to activities
dtSubjectActivities <- cbind(dtSubjects, dtActivities)
dtSubjectAtvitiesWithMeasures <- cbind(dtSubjectActivities, dtMeasures)

# Order all of the combined data by, subject and activity
setkey(dtSubjectAtvitiesWithMeasures, subject, activityNumber)

## Read in the 'features.txt' 
## This file matches up to the columns in the data.table, dtSubjectActivitiesWithMeasures
## with the features/measures.
dtAllFeatures <- fread(file.path(projectDataPath, "features.txt"))
setnames(dtAllFeatures, c("V1", "V2"), c("measureNumber", "measureName"))

# Use grepl to just get features/measures related to mean and std
dtMeanStdMeasures <- dtAllFeatures[grepl("(mean|std)\\(\\)", measureName)]
# Create a column to 'index/cross reference' into the 'measure' headers
# in dtSubjectActivitiesWithMeasures
dtMeanStdMeasures$measureCode <- dtMeanStdMeasures[, paste0("V", measureNumber)]

# Build up the columns to select from the data.table,
# dtSubjectActivitiesWithMeasures
columnsToSelect <- c(key(dtSubjectAtvitiesWithMeasures), dtMeanStdMeasures$measureCode)
# Just take the rows with the columns of interest ( std() and mean() )
dtSubjectActivitesWithMeasuresMeanStd <- subset(dtSubjectAtvitiesWithMeasures, 
                                                select = columnsToSelect)

# Read in the activity names and give them more meaningful names
dtActivityNames <- fread(file.path(projectDataPath, "activity_labels.txt"))
setnames(dtActivityNames, c("V1", "V2"), c("activityNumber", "activityName"))

# Merge the 'meaningful activity names' with the 
# dtSubjectActiitiesWithMeasuresMeanStd
dtSubjectActivitesWithMeasuresMeanStd <- merge(dtSubjectActivitesWithMeasuresMeanStd, 
                                               dtActivityNames, by = "activityNumber", 
                                               all.x = TRUE)

# Sort the data.table, dtSubjectActivitesWithMeasuresMeanStd
setkey(dtSubjectActivitesWithMeasuresMeanStd, subject, activityNumber, activityName)

# Convert from a wide to narrow data.table using the keys created earlier
dtSubjectActivitesWithMeasuresMeanStd <- data.table(melt(dtSubjectActivitesWithMeasuresMeanStd, 
                                                         id=c("subject", "activityName"), 
                                                         measure.vars = c(3:68), 
                                                         variable.name = "measureCode", 
                                                         value.name="measureValue"))

# Merge measure codes
dtSubjectActivitesWithMeasuresMeanStd <- merge(dtSubjectActivitesWithMeasuresMeanStd, 
                                               dtMeanStdMeasures[, list(measureNumber, measureCode, measureName)], 
                                               by="measureCode", all.x=TRUE)

# Convert activityName and measureName to factors
dtSubjectActivitesWithMeasuresMeanStd$activityName <- 
  factor(dtSubjectActivitesWithMeasuresMeanStd$activityName)
dtSubjectActivitesWithMeasuresMeanStd$measureName <- 
  factor(dtSubjectActivitesWithMeasuresMeanStd$measureName)

# Reshape the data to get the averages 
measureAvgerages <- dcast(dtSubjectActivitesWithMeasuresMeanStd, 
                          subject + activityName ~ measureName, 
                          mean, 
                          value.var="measureValue")

# Write the tab delimited file
write.table(measureAvgerages, file="tidyData.txt", row.name=FALSE, sep = "\t")
