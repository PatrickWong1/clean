
### 1. Merges the training and the test sets to create one data set.
#Load training and test sets.
X_train<-read.table("X_train.txt")
X_test<-read.table("X_test.txt") 

#Do quick inspection of the two sets to see whether the variable orders are likely the same
#and that they do contain 561 features
print(X_train_summary<-summary(X_train[41:46,1:10]))
print(X_test_summary<-summary(X_test[41:46, 1:10]))
str(X_train,list.len=10)
str(X_test, list.len=10)
#Check to see whether the the train file does have roughly 7/3 as many obs as the test
dim(X_train)
dim(X_test)
(dim(X_train)/dim(X_test))[1]

#Merge the 2 data sets by appending the training set to the test set and check merged set
X_test_train<-rbind(X_train, X_test )
dim(X_test_train)
(dim(X_train)+  dim(X_test))[1]


### 2. Extracts only the measurements on the mean and standard deviation for each measurement.
#Read in the features, inspect data, and then use the feature names as the column names
features<-read.table("features.txt")
str(features)
features
features$V2<-as.character(features$V2)  #features were in factor, change to character

for (i in 1:dim(features)[1]){
  colnames(X_test_train)[i] <- features[i,2]
}
colnames(X_test_train)  #Check results
str(X_test_train)

#Extract all columns with mean() or std() to form new dataframe
X_test_train_ms<-NULL  #initialize dataframe with only means and std
colnumber=0   #initialize column number for new dataframe

for (i in 1:dim(features)[1]){
  if (grepl("mean()", colnames(X_test_train)[i])){
    X_test_train_ms<-cbind(X_test_train_ms,X_test_train[,i])
    colnumber=colnumber+1
    colnames(X_test_train_ms)[colnumber]<-colnames(X_test_train)[i]
  } 
      
  if (grepl("std()", colnames(X_test_train)[i])){
    X_test_train_ms<-cbind(X_test_train_ms,X_test_train[,i])
    colnumber=colnumber+1
    colnames(X_test_train_ms)[colnumber]<-colnames(X_test_train)[i]
  }
}

dim(X_test_train_ms)
summary(X_test_train_ms)

### 3. Uses descriptive activity names to name the activities in the data set
#Read in integer activity labels and then combine test and train.  Read in activity definitions
Train_labels<-read.table("y_train.txt")
Test_labels<-read.table("y_test.txt")
Train_Test_labels<-rbind(Train_labels,Test_labels)
activity_labels<-read.table("activity_labels.txt")

#Lookup the activity definitions from the integer acitivity labels
mergedActivity<-merge(Train_Test_labels, activity_labels, by.x="V1", by.y="V1",sort=FALSE)
head(mergedActivity)
colnames(mergedActivity)[2]="Activity"    #Rename the column 

#Add the Activity column to the overall data set 
X_test_train_ms_act <-cbind.data.frame(X_test_train_ms,mergedActivity$Activity)
head(X_test_train_ms_act[,80])
colnames(X_test_train_ms_act)[80]="Activity"   #Fix the column name from mergedActivity$Activity

### 4. Appropriately labels the data set with descriptive variable names. 
#3 transforms will be used to make the names more intuitive:
# mean()=>Average, std()=>StandarDev(), Gyro=>AngularVel

X_test_train_ms_act_renamed<-X_test_train_ms_act  #Create new df initialized to old
for (i in 1:dim(X_test_train_ms_act_renamed)[2]){
  colnames(X_test_train_ms_act_renamed)[i]=gsub("mean", "Average", colnames(X_test_train_ms_act_renamed)[i])
  colnames(X_test_train_ms_act_renamed)[i]=gsub("[()]", "", colnames(X_test_train_ms_act_renamed)[i])
  colnames(X_test_train_ms_act_renamed)[i]=gsub("std()", "StandarDev", colnames(X_test_train_ms_act_renamed)[i])
  colnames(X_test_train_ms_act_renamed)[i]=gsub("Gyro", "AngularVel", colnames(X_test_train_ms_act_renamed)[i])
}
colnames(X_test_train_ms_act_renamed)  #Check

### 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.

#First, subject identity needs to be added to the big data set
subject_train<-read.table("subject_train.txt")
subject_test<-read.table("subject_test.txt")
subject_train_test<-rbind(subject_train,subject_test)
head(subject_train_test);head(subject_train)
tail(subject_train_test);tail(subject_test)

X_test_train_ms_act_renamed_subj<-cbind.data.frame(X_test_train_ms_act_renamed,subject_train_test$V1 )
colnames(X_test_train_ms_act_renamed_subj)[81]="Subject"

#New data set creation, relying on carMelt example in lecture on reshaping data
library(reshape2)

a<-!(colnames(X_test_train_ms_act_renamed_subj) %in% c("Activity", "Subject"))
measure.variables<-colnames(X_test_train_ms_act_renamed_subj)[a]
sensorMelt<-melt(X_test_train_ms_act_renamed_subj,
                 id=c("Subject","Activity"),
                 measure.vars=measure.variables)

FinalDataSet<-dcast(sensorMelt,Subject+Activity~variable,mean)

write.table(FinalDataSet, file="FinalDataSet.txt", row.name=FALSE)

