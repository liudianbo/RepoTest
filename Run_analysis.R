#project 

# Url<-'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
# download.file(url =Url,destfile = './data/CleanDataTest.zip' )

#Test data 
Test.X<-read.table(file = './UCI HAR Dataset/test/X_test.txt')
object.size(Test.X)
dim(Test.X)
head(Test.X)
class(Test.X)

Test.Subject<-read.table(file = './UCI HAR Dataset/test/subject_test.txt')
dim(Test.Subject)

head(Test.Subject)
class(Test.Subject$V1)
nrow(Test.Subject)

Test.Y<-read.table(file = './UCI HAR Dataset/test/y_test.txt')
head(Test.Y)
dim(Test.Y)
unique(Test.Y$V1)

ActivityNames<-read.table(file = './UCI HAR Dataset/activity_labels.txt')
# ActivityVec<-merge(x = Test.Y,y = ActivityNames,by.x = 'V1',by.y = 'V1')[,2]
# head(ActivityVec)


library(dplyr)
# TestData<-mutate(.data = Test.X,Activity=ActivityVec,Subject=Test.Subject[,1])

TestData<-(Test.X%>%mutate(Activity=merge(x = Test.Y,y = ActivityNames,by.x = 'V1',by.y = 'V1')[,2],Subject=Test.Subject[,1]))


#Train data
Train.X<-read.table(file = './UCI HAR Dataset/train/X_train.txt')
object.size(Test.X)
dim(Test.X)
head(Test.X)
class(Test.X)

Train.Subject<-read.table(file = './UCI HAR Dataset/train/subject_train.txt')
dim(Test.Subject)

head(Test.Subject)
class(Test.Subject$V1)
nrow(Test.Subject)

Train.Y<-read.table(file = './UCI HAR Dataset/train/y_train.txt')
head(Test.Y)
dim(Test.Y)
unique(Test.Y$V1)

TrainData<-(Train.X%>%mutate(Activity=merge(x = Train.Y,y = ActivityNames,by.x = 'V1',by.y = 'V1')[,2],Subject=Train.Subject[,1]))
head(TrainData)


OutPutData<-rbind(TestData,TrainData)
head(OutPutData)
MeanAndSD<-OutPutData

# step 2 and 4
MeanAndSD<-(MeanAndSD%>%mutate(StandDev=apply(OutPutData[,1:561],MARGIN = 1,FUN = sd))%>%mutate(MeanValue=apply(OutPutData[,1:561],MARGIN = 1,FUN = mean)))%>%
  select(Subject,Activity,MeanValue,StandDev)

head(MeanAndSD)

MeanByActivity<-group_by(.data = MeanAndSD,Activity)

MeanByActivity<-summarise(.data = MeanByActivity,MeanValue=mean(MeanValue))#mean by acitivity 

MeanBySubject<-group_by(.data = MeanAndSD,Subject)
MeanBySubject<-summarise(.data = MeanBySubject,MeanValue=mean(MeanValue))#mean by subject 

ActivityAndSubject<-xtabs(formula = MeanValue~Activity+Subject,data = MeanAndSD)#activity vs. subject in xtab inform

ActivityAndSubjectSubmit<-select(.data = MeanAndSD,Subject,Activity,MeanValue) # in normal form , to submit. 
head(ActivityAndSubjectSubmit)

write.table(x = ActivityAndSubjectSubmit,file = 'Getting and Cleaning Date Project.txt',row.names = F)
