library(readr)
forestfires <- read_csv("C:/Users/Admin/Desktop/Assignments/SVM/forestfires.csv")
View(forestfires)
str(forestfires)
summary(forestfires)
table(forestfires$size_category)

fire_train<-forestfires[1:370,c(3,4,5,6,7,8,9,10,11,31)]
fire_test<- forestfires[371:517,c(3,4,5,6,7,8,9,10,11,31)]

############# 
install.packages("kernlab")
library(kernlab)
area_classifier<- ksvm( size_category ~FFMC+DMC+DC+ISI+temp+RH +wind+rain+area,data=fire_train,kernel="vanilladot")
fire_pred<- predict(area_classifier,fire_test)
head(fire_pred)
view(fire_test)
table(fire_pred,fire_test$size_category)
CM = table(fire_pred,fire_test$size_category)
accuracy = (sum(diag(CM)))/sum(CM)
accuracy

#############   
library(kernlab)
area_classifier<- ksvm( size_category ~FFMC+DMC+DC+ISI+temp+RH +wind+rain+area,data=fire_train,kernel="polydot")
fire_pred<- predict(area_classifier,fire_test)
head(fire_pred)
view(fire_test)
table(fire_pred,fire_test$size_category)
CM = table(fire_pred,fire_test$size_category)
accuracy = (sum(diag(CM)))/sum(CM)
accuracy
#############   
library(kernlab)
area_classifier<- ksvm( size_category ~FFMC+DMC+DC+ISI+temp+RH +wind+rain+area,data=fire_train,kernel="tanhdot")
fire_pred<- predict(area_classifier,fire_test)
head(fire_pred)
view(fire_test)
table(fire_pred,fire_test$size_category)
CM = table(fire_pred,fire_test$size_category)
accuracy = (sum(diag(CM)))/sum(CM)
accuracy

############# 
library(kernlab)
area_classifier<- ksvm( size_category ~FFMC+DMC+DC+ISI+temp+RH +wind+rain+area,data=fire_train,kernel="rbfdot")
fire_pred<- predict(area_classifier,fire_test)
head(fire_pred)
view(fire_test)
table(fire_pred,fire_test$size_category)
CM = table(fire_pred,fire_test$size_category)
accuracy = (sum(diag(CM)))/sum(CM)
accuracy

#############
set.seed(1234)
indexes = sample(2, nrow(forestfires[,c(3,4,5,6,7,8,9,10,11,31)]), replace=TRUE, prob=c(0.7, 0.3))
indexes
fire_train =forestfires[indexes==1, c(3,4,5,6,7,8,9,10,11,31)]
fire_test = forestfires[indexes==2, c(3,4,5,6,7,8,9,10,11,31)]
fire_train_labels = forestfires[indexes==1, 31]
fire_test_labels = forestfires[indexes==2, 31]
fire_train_labels<-fire_train_labels[["size_category"]]
fire_test_labels<-fire_test_labels[["size_category"]]
area_classifier<- ksvm( size_category ~FFMC+DMC+DC+ISI+temp+RH +wind+rain+area,data=fire_train,kernel="vanilladot")
fire_pred<- predict(area_classifier,fire_test)
head(fire_pred)
view(fire_test)
table(fire_pred,fire_test_labels)
CM = table(fire_pred,fire_test_labels)
accuracy = (sum(diag(CM)))/sum(CM)
accuracy
