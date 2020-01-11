library(readr)
SalaryData_Train <- read_csv("C:/Users/Admin/Desktop/Assignments/SVM/SalaryData_Train.csv")
View(SalaryData_Train)
str(SalaryData_Train)
summary(SalaryData_Train)
table(SalaryData_Train$Salary)

SalaryData_Train_train<-SalaryData_Train[1:24000,]
SalaryData_Train_test<- SalaryData_Train[24001:30161,]

install.packages("kernlab")
library(kernlab)
salary_classifier<- ksvm( Salary  ~age+workclass +education+educationno+maritalstatus+occupation +relationship+race+sex+capitalgain+ capitalloss+ hoursperweek+native,data=SalaryData_Train_train,kernel="vanilladot")
salary_pred<- predict(salary_classifier,SalaryData_Train_test)

head(salary_pred)
view(SalaryData_Train_test)
table(salary_pred,SalaryData_Train_test$Salary)
CM = table(salary_pred,SalaryData_Train_test$Salary)
accuracy = (sum(diag(CM)))/sum(CM)
accuracy
