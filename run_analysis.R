library(dplyr)
library(plyr)
#the data ("UCI HAR Dataset" folder) should be in working directory, otherwise the program will download it 

downloadFile<-function(){download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","project_data")
                         unzip("project_data")}
file.info("UCI HAR Dataset")

if (file.exists("UCI HAR Dataset")){
    if (all(list.files("UCI HAR Dataset")==c("activity_labels.txt","features.txt",
                                        "features_info.txt","README.txt","test","train"))){}else{downloadFile() }
}else{downloadFile()  }
#reading column names first
x_names<-read.table("UCI HAR Dataset/features.txt") 

#Reading and merging test files

testSubT<-read.table("UCI HAR Dataset/test/subject_test.txt", col.names="subject")
testXT<-read.table("UCI HAR Dataset/test/X_test.txt",col.names=as.vector(x_names[,2]) ,check.names=T)
testYT<-read.table("UCI HAR Dataset/test/Y_test.txt", col.names="activity")

test<-cbind(testSubT,testYT,testXT)

#merging train files
trainSubT<-read.table("UCI HAR Dataset/train/subject_train.txt", col.names="subject")
trainXT<-read.table("UCI HAR Dataset/train/X_train.txt",col.names=as.vector(x_names[,2]) ,check.names=T)
trainYT<-read.table("UCI HAR Dataset/train/Y_train.txt", col.names="activity")

train<-cbind(trainSubT,trainYT,trainXT)

#binding test and train groups together
dataAll<-rbind(train,test)


#converting activity to factor and setting factor labels
dataAll$activity<-as.factor(dataAll$activity)
    
act_names <- read.table("UCI HAR Dataset/activity_labels.txt")
dataAll$activity<-plyr::mapvalues(dataAll$activity, from = 1:6, to = as.vector(act_names[,2]))



######Extracts only the measurements on the mean and standard deviation for each measurement. 

dataMeanStd<- select(dataAll, c(contains("subject"), contains("activity"),contains("mean"), contains("std")))

#####From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

dataMeans <- dataMeanStd %>% group_by(subject,activity) %>% summarise_each(funs(mean))
##Saving the output into new file
write.table(dataMeans, "processedData.txt",row.name=FALSE)