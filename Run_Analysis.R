cleaningData<-function(){


	features<-read.table("features.txt")

	mean_std_f<-grep("mean|std",features[,2])


	subject_train<-read.table("train/subject_train.txt")
	x_train<-read.table("train/X_train.txt")
	y_train<-read.table("train/y_train.txt")

	
	subject_test<-read.table("test/subject_test.txt")
	x_test<-read.table("test/X_test.txt")
	y_test<-read.table("test/y_test.txt")

	x_train<-x_train[,mean_std_f]
	x_test<-x_test[,mean_std_f]
	
	
	test_data<-cbind(subject_test,y_test,x_test)
	train_data<-cbind(subject_train,y_train,x_train)

	merge_data<-rbind(train_data,test_data)

	features[,2]<-gsub("-mean","Mean",features[,2])
	features[,2]<-gsub("-std","Std",features[,2])
	features[,2]<-gsub("\\(\\)","",features[,2])
	
	names(merge_data)<-c("Subject","Activities",features[mean_std_f,2])

	activities<-read.table("activity_labels.txt")
	merge_data[,2]<-activities[merge_data[,2],2]


 	melted_data <- melt(merge_data, id = c("Subject", "Activities"))

	tidy_data <- dcast(melted_data, Subject + Activities ~ variable, mean)

	
	write.table(tidy_data, "tidy_data.txt", row.names = FALSE, quote = FALSE)
}