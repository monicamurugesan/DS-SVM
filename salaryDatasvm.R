
library(e1071)
library(caret)
setwd("H:\\RStudio\\assignment")
train <-read.csv(file.choose())
attach(train)
train$workclass<-as.factor(workclass)
train$education<-as.factor(education)
train$occupation<-as.factor(occupation)
train$relationship<-as.factor(relationship)
train$educationno <-as.integer(as.factor(educationno))
train$native<-as.factor(native)
train$race<-as.factor(race)
train$Salary<-as.factor(Salary)
train$sex<-as.factor(sex)
train$maritalstatus<-as.factor(maritalstatus)
str(train)
summary(train)

test <-read.csv(file.choose())
str(test)
View(test)
attach(test)
##Converting into factor
test$workclass<-as.factor(workclass)
test$education<-as.factor(education)
test$occupation<-as.factor(occupation)
test$relationship<-as.factor(relationship)
test$educationno <-as.integer(as.factor(educationno))
test$native<-as.factor(native)
test$race<-as.factor(race)
testSalary<-as.factor(Salary)
test$sex<-as.factor(sex)
test$maritalstatus<-as.factor(maritalstatus)


summary(test)

###kernel==sigmoid
svm_salary <-svm(Salary~.,data=train,kernel="sigmoid")
summary(svm_salary)
pred <-predict(svm_salary,test)

View(train)
confusionMatrix(table(pred,test$Salary)) ###84.46

##kernel==linear
svm_sal1 <-svm(Salary~.,data=train,kernel="linear")
summary(svm_sal1)
predict1<-predict(svm_sal1,test)
agg1 <-predict1==test$Salary
table(agg1)
confusionMatrix(table(predict1,test$Salary)) ###84.62
prop.table(agg1)

#kernel=radial
svm_sal2 <-svm(Salary~.,data=train,kernel="radial")
summary(svm_sal2)
predict2 <-predict(svm_sal2,test)
confusionMatrix(table(predict2,test$Salary))###84.72
agg2 <-predict2==test$Salary
table(agg2)
str(train)

plot(svm_sal2,data=train,age~capitalgain)

# svm_tune <- tune(svm, train.x=train$age+train$educationno, train.y=train$Salary, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
# 
# print(svm_tune)


