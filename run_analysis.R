library("dplyr")
library("reshape2")
##Read features_lables and activity_lables 
feature_labels<-read.table("./features.txt", header = FALSE, sep = "") 
activity_labels<-read.table("./activity_labels.txt", header = FALSE, sep = "")
colnames(activity_labels)<-c("activity","activity_label") ##Assign column names to activity
##Read test_files and combine them
subject_test<-read.table("./test/subject_test.txt", header = FALSE, sep = "")
X_test<-read.table("./test/X_test.txt", header = FALSE, sep = "")
Y_test<-read.table("./test/Y_test.txt", header = FALSE, sep = "")
test_combined<-cbind(X_test,Y_test,subject_test)
##Read training_files and combine them
subject_train<-read.table("./train/subject_train.txt", header = FALSE, sep = "")
X_train<-read.table("./train/X_train.txt", header = FALSE, sep = "")
Y_train<-read.table("./train/Y_train.txt", header = FALSE, sep = "")
train_combined<-cbind(X_train,Y_train,subject_train)
##Merge test and training sets
test_train<-rbind(test_combined,train_combined)
##Create combined label names for all columns (561 variables +activity+subject)
feature_labels$V2 <- as.character(feature_labels$V2)
labels_int <- rbind(feature_labels,c(562,"activity"))
labels<- rbind(labels_int,c(563,"subject"))
colnames(test_train)<-labels$V2
##Remove duplicate columns (out of 563 columns there are only 479 unique names)
test_train<-test_train[,!duplicated(colnames(test_train))]
##Extracts only the measurements on the mean and standard deviation for each measuremen along with subject and activity identifiers
meanstd_column_filter <- grep("mean\\()|std\\()|subject|activity",names(test_train), ignore.case =TRUE)
filtered_data<-select(test_train,meanstd_column_filter)
#Add activity labels for activity values
filtered_merged_data<-merge( x=filtered_data, y=activity_labels)
#Step5-Create average of each variable for each activity and each subject
data_melted<-melt(filtered_merged_data,id=c("activity_label","subject"),na.rm=TRUE)
#Convert value to numberic from character for calculating mean 
data_melted$value<-as.numeric(data_melted$value)
#Reshape data to create independent tidy data set with the average of each variable for each activity and each subject.
data_reshaped<-dcast(data_melted,activity_label+subject~variable,mean)
#Save table
write.table(data_reshaped,"reshaped_data.txt",row.name=FALSE)
#Generate varilable list for codebook
write.table(names(data_reshaped),"Codebook.txt")
