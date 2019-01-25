#import dataset
data(iris)
data<-iris
#check structure of dataset
str(iris)
#summary 
summary(iris)
#divide the data into traning and testing
sampledata<-sample(2,nrow(data),replace=TRUE,prob=c(0.7,0.3))
train<-data[sampledata==1,]
test<-data[sampledata==2,]
#for randomly selecting the dataset
set.seed(1234)
library(randomForest)
attach(data)
#starting building model for random forest model we use random forest package
modelrandomforest<-randomForest(Species~.,data = train)
modelrandomforest
#mtry means no of varuables tried to split at each node and ntree is the no of decision tree selected for random forest
modelrandomforest1<-randomForest(Species~.,data = train,mtry=8,ntree=500)
modelrandomforest1
modelrandomforest2<-randomForest(Species~.,data = train,mtry=8,ntree=500)
modelrandomforest2
#prediction on traning dataset
pred<-predict(modelrandomforest1,train)
pred
table(pred,train$Species)
#prediction on testing dataset
predtest<-predict(modelrandomforest1,test)
predtest
table(predtest,test$Species)
#accuracy of model prediction
mean(predtest==test$Species)
#loop to get best accuray of model by try all mtry values 
a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(Species ~ ., data = train, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, test, type = "class")
  a[i-2] = mean(predValid == test$Species)
}

a