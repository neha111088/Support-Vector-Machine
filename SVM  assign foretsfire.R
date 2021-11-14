install.packages("kernlab")
install.packages("caret")
install.packages("plyr")

library(kernlab)
library(caret)
library(plyr)

View(forestfires)
class(forestfires)

f_data<- forestfires[-c(1,2,31)]
colnames(f_data)<-c("FFMC","DMC","DC","ISI","temp","RH","wind","rain","area","dayfri","daymon","daysat","daysun","daythu","daytue","daywed","monthapr","monthaug","monthdec","monthfeb","monthjan","monthjul","monthjun","monthmar","monthmay","monthnov","monthoct","monthsep")


train <- f_data[1:400,]
test <- f_data[401:517,]

model1 <- ksvm(area ~.,data = train,kernel = "rbfdot")
model1

pred_rbfdot<-predict(model1,newdata=test)
mean(pred_rbfdot==test$size_category)


table(test$size_category)
table(pred_rbfdot,test$area)

model2 <- ksvm(area ~.,data = train,kernel = "vanilladot")
model2


pred_vanilladot<-predict(model2,newdata=test)
mean(pred_vanilladot==test$area)
model2 <- ksvm(area ~.,data = train,kernel = "vanilladot")

model2


pred_vanilladot<-predict(model2,newdata=test)
mean(pred_vanilladot==test$area)
table(test$size_category)
table(pred_vanilladot,test$area)
model3 <- ksvm(area ~.,data = train,kernel = "polydot")
model3


pred_polydot<-predict(model3,newdata=test)
mean(pred_polydot==test$area)

table(test$area)
table(pred_polydot,test$area)
model4 <- ksvm(area ~.,data = train,kernel = "tanhdot")
model4

pred_tanhdot<-predict(model4,newdata=test)
mean(pred_tanhdot==test$area)

table(test$area)
table(pred_tanhdot,test$area)
model5 <- ksvm(area ~.,data = train,kernel = "laplacedot")
model5


pred_laplacedot<-predict(model5,newdata=test)
mean(pred_laplacedot==test$area)

table(test$area)
table(pred_laplacedot,test$area)

model6 <- ksvm(area ~.,data = train,kernel = "besseldot")
model6

pred_besseldot<-predict(model5,newdata=test)
mean(pred_besseldot==test$area)

table(test$area)

table(pred_laplacedot,test$area)