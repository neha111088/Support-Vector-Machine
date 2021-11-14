library(kernlab)
library(caret)
library(plyr)
library(ggplot2)
library(psych)
library(e1071)


salary <-  rbind(`SalaryData_Test(1)`,`SalaryData_Train(1)`)
View(salary)

colnames(salary)<-c("workclass","education","maritalstatus","occupation","relationship","race","sex","native","age","educationno","capitalgain","capitalloss","hoursperweek","Salary")

train <- salary[1:30161,]
test <- salary[30162:15060,]


model1 <- ksvm(workclass ~.,data =train,kernel = "vanilladot") 
model1

pred_vanilldot<-predict(model1,newdata=test)
mean(pred_vanilldot==test$workclass)

table(test$workclass)
table(pred_vanilldot,test$workclass)


model2 <- ksvm(workclass ~.,data = train,kernel = "rbfdot")
model1

pred_rbfdot<-predict(model1,newdata=test)
mean(pred_rbfdot==test$size_category)

table(test$size_category)
table(pred_rbfdot,test$size_category)

