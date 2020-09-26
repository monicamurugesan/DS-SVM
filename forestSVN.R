library(caret)
library(e1071)
setwd("H:\\RStudio\\assignment")
forest <-read.csv(file.choose())
View(forest)
str(forest)
attach(forest)
forest$month <-as.factor(forest$month)
forest$size_category <-as.factor(forest$size_category)
forest$day <- as.factor(forest$day)
forest$temp <-as.integer(forest$temp)
forest$DMC <- as.integer(forest$DMC)
forest$ISI <-as.integer(forest$ISI)
forest$FFMC <-as.integer(forest$FFMC)
forest$wind <-as.integer(forest$wind)
forest$rain <-as.integer(forest$rain)
forest$area <-as.integer(forest$area)
forest$DC <-as.integer(forest$DC)
str(forest)
# x <-forest[,1:30]
# y <-forest$size_category
training <-createDataPartition(forest$size_category,p=0.5,list=F)
train_forest <-forest[training,]
test_forest <-forest[-training,]
length(train_forest)
length(test_forest)
##kernel=sigmoid
svm_forest <-svm(size_category~.,data=train_forest,kernel="sigmoid")
summary(svm_forest)
pred <-predict(svm_forest,test_forest)
agg <-pred==test_forest$size_category
table(agg)
length(pred)
print(svm_forest)
confusionMatrix(table(pred,test_forest$size_category))

###kernel=radial
svm_forest1 <-svm(size_category~.,data=train_forest,kernel="radial")
summary(svm_forest1)
pred1 <-predict(svm_forest1,test_forest)
agg1 <-pred1==test_forest$size_category
table(agg1)
length(pred1)
print(svm_forest1)
confusionMatrix(table(pred1,test_forest$size_category))


svm_forest2 <-svm(size_category~.,data=train_forest,kernel="linear")
summary(svm_forest2)
pred2 <-predict(svm_forest2,test_forest)
agg2 <-pred2==test_forest$size_category
table(agg2)
length(pred2)
print(svm_forest2)
confusionMatrix(table(pred2,test_forest$size_category))

svm_tune <- tune(svm, train.x=train_forest$FFMC+train_forest$temp+train_forest$RH+train_forest$ISI+train_forest$wind, train.y=train_forest$size_category, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune)
svm_model_after_tune <- svm(size_category ~ ., data=train_forest, kernel="radial", cost=1, gamma=0.5)
summary(svm_model_after_tune)
pred_tune <- predict(svm_model_after_tune,test_forest)
system.time(predict(svm_model_after_tune,test_forest))
confusionMatrix(table(pred_tune,test_forest$size_category))
plot(svm_forest,data=train_forest,temp~ISI)
