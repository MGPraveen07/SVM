library(readr)
SalaryData_Test <- read_csv("C:/Users/Admin/Desktop/Assignments/SVM/SalaryData_Test.csv")
View(SalaryData_Test)
str(SalaryData_Test)
summary(SalaryData_Test)
table(SalaryData_Test$Salary)

SalaryData_Test_train<-SalaryData_Test[1:12000,]
SalaryData_Test_test<- SalaryData_Test[12001:15060,]

install.packages("kernlab")
library(kernlab)
salary_classifier<- ksvm( Salary  ~age+workclass +education+educationno+maritalstatus+occupation +relationship+race+sex+capitalgain+ capitalloss+ hoursperweek+native,data=SalaryData_Test_train,kernel="vanilladot")
salary_pred<- predict(salary_classifier,SalaryData_Test_test)

head(salary_pred)
view(SalaryData_Test_test)
table(salary_pred,SalaryData_Test_test$Salary)
CM = table(salary_pred,SalaryData_Test_test$Salary)
accuracy = (sum(diag(CM)))/sum(CM)
accuracy
