library(readxl)
predictdatatransformer <- read_excel("C:/Users/Manpreet.saluja/Desktop/predictdatatransformer.xlsx")
View(predictdatatransformer)

#structure of data
str(predictdatatransformer)
predictdatatransformer$`Oil Leakage`<-as.factor(predictdatatransformer$`Oil Leakage`)
predictdatatransformer$`Fuel supply`<-as.factor(predictdatatransformer$`Fuel supply`)
predictdatatransformer$`Vector Group`<-as.factor(predictdatatransformer$`Vector Group`)
predictdatatransformer$Insulation<-as.factor(predictdatatransformer$Insulation)
predictdatatransformer$`Energy Losses`<-as.factor(predictdatatransformer$`Energy Losses`)
predictdatatransformer$`Pressure Relay`<-as.factor(predictdatatransformer$`Pressure Relay`)
predictdatatransformer$Core<-as.factor(predictdatatransformer$Core)
predictdatatransformer$Bushing<-as.factor(predictdatatransformer$Bushing)
predictdatatransformer$`Over Current Protection (OC)`<-as.factor(predictdatatransformer$`Over Current Protection (OC)`)
predictdatatransformer$`Fire Fighting Systems (FFS)`<-as.factor(predictdatatransformer$`Fire Fighting Systems (FFS)`)
predictdatatransformer$`Silica Gel Color`<-as.factor(predictdatatransformer$`Silica Gel Color`)
predictdatatransformer$Outage<-as.factor(predictdatatransformer$Outage)



attach(predictdatatransformer)

#EDA..
head(predictdatatransformer)
tail(predictdatatransformer)
summary(predictdatatransformer)
hist(predictdatatransformer$`Tap Changer`,prob = T)
library(car)
qqnorm(predictdatatransformer$`Tap Changer`)
qqline(predictdatatransformer$`Tap Changer`)
boxplot(predictdatatransformer$`Lamination thickness maintainence`)
boxplot(predictdatatransformer)
abline(predictdatatransformer)
apply(predictdatatransformer,2,function(x){sum(is.na(x))})
dim(predictdatatransformer)
library(DataExplorer)
plot_str(predictdatatransformer)
plot_missing(predictdatatransformer)
plot_histogram(predictdatatransformer)
plot_density(predictdatatransformer)
plot_correlation(predictdatatransformer)
plot_qq(predictdatatransformer)

library(PerformanceAnalytics)
chart.Correlation(predictdatatransformer, 
                  method="spearman",
                  histogram=TRUE,
                  pch=16)
plot_bar(predictdatatransformer)
create_report(predictdatatransformer)

#create training and validation data from given data
#install.packages('caTools')
library(caTools)
set.seed(88)
split <- sample.split(predictdatatransformer$Outage, SplitRatio = 0.75)

#get training and test data
train <- subset(predictdatatransformer, split == TRUE)
test <- subset(predictdatatransformer, split == FALSE)
set.seed(1234)
library(randomForest)
attach(train)
#starting building model for random forest model we use random forest package
modelrandomforest<-randomForest(factor(Outage) ~ `Lamination thickness maintainence`+factor(Bushing)+factor(`Over Current Protection (OC)`)+factor(`Fire Fighting Systems (FFS)`)+`Hot Spots (HS)`+`Breakdown voltage`+`Water content`+`Oil Acidity`+factor(`Silica Gel Color`)+factor(`Tap Changer`)+`Percentage Impedance (at 75°C)`,data = train)
modelrandomforest
#mtry means no of varuables tried to split at each node and ntree is the no of decision tree selected for random forest
modelrandomforest1<-randomForest(factor(Outage) ~ `Lamination thickness maintainence`+factor(Bushing)+factor(`Over Current Protection (OC)`)+factor(`Fire Fighting Systems (FFS)`)+`Hot Spots (HS)`+`Breakdown voltage`+`Water content`+`Oil Acidity`+factor(`Silica Gel Color`)+factor(`Tap Changer`)+`Percentage Impedance (at 75°C)`,data = train,mtry=8,ntree=500)
modelrandomforest1
modelrandomforest2<-randomForest(factor(Outage) ~ `Lamination thickness maintainence`+factor(Bushing)+factor(`Over Current Protection (OC)`)+factor(`Fire Fighting Systems (FFS)`)+`Hot Spots (HS)`+`Breakdown voltage`+`Water content`+`Oil Acidity`+factor(`Silica Gel Color`)+factor(`Tap Changer`)+`Percentage Impedance (at 75°C)`,data = train,mtry=8,ntree=500)
modelrandomforest2

#prediction on traning dataset
pred<-predict(modelrandomforest1,train)
pred
table(pred,train$Outage)

#prediction on testing dataset
predtest<-predict(modelrandomforest1,test)
predtest
table(predtest,test$Outage)

#accuracy of model prediction
mean(predtest==test$Outage)
#loop to get best accuray of model by try all mtry values 
#loop to get best accuray of model by try all mtry values 
a=c()
for (i in 1:40 ){
  model3 <- randomForest(factor(Outage) ~ `Lamination thickness maintainence`+factor(Bushing)+factor(`Over Current Protection (OC)`)+factor(`Fire Fighting Systems (FFS)`)+`Hot Spots (HS)`+`Breakdown voltage`+`Water content`+`Oil Acidity`+factor(`Silica Gel Color`)+factor(`Tap Changer`)+`Percentage Impedance (at 75°C)`, data = train, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, test, type = "class")
  a[i-2] = mean(predValid == test$Outage)
}
a

model31 <- randomForest(factor(Outage) ~ `Lamination thickness maintainence`+factor(Bushing)+factor(`Over Current Protection (OC)`)+factor(`Fire Fighting Systems (FFS)`)+`Hot Spots (HS)`+`Breakdown voltage`+`Water content`+`Oil Acidity`+factor(`Silica Gel Color`)+factor(`Tap Changer`)+`Percentage Impedance (at 75°C)`, data = train, ntree = 500, mtry = 7, importance = TRUE)
model31
summary(model31)
predValid <- predict(model31, test, type = "class")
predValid
 
mean(predValid == test$Outage)
 