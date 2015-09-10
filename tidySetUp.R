setTidyData <- function ()
{
  
  library(reshape2)
  
  testSet=read.table("UCI.HAR.Dataset/test/X_test.txt")
  trainingSet=read.table("UCI.HAR.Dataset/train/X_train.txt")
  trainingY=read.table("UCI.HAR.Dataset/train/y_train.txt",col.names = "Activity")
  testY=read.table("UCI.HAR.Dataset/test/y_test.txt",col.names = "Activity")
  trainingSubject=read.table("UCI.HAR.Dataset/train/subject_train.txt",col.names = "Subject")
  testSubject=read.table("UCI.HAR.Dataset/test/subject_test.txt",col.names = "Subject")
  featureNames=read.table("UCI.HAR.Dataset/features.txt")
  activityNames=read.table("UCI.HAR.Dataset/activity_labels.txt")[,2]
  
  
  
  names(trainingSet)=as.character(featureNames[,2])
  names(testSet)=as.character(featureNames[,2])
  
  testCSet=cbind(testSet,testY,testSubject)
  trainCSet=cbind(trainingSet,trainingY,trainingSubject)
  dataSet=rbind(trainCSet,testCSet)
  
  subDataSet=dataSet[,grep("(.*)[Mm]ean\\(\\)(.*)|(.*)[sS]td(.*)|Activity|Subject",names(dataSet))]
  
  subDataSet$Activity=factor(subDataSet$Activity,labels = activityNames)
  subDataSet$Subject=factor(subDataSet$Subject)
  
  meltedDataSet=melt(subDataSet,c("Subject","Activity"))
  tidyDataSet=dcast(meltedDataSet,Subject+ Activity~variable,fun.aggregate = mean)
}