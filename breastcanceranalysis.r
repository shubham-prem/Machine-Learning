library(caret)
library("e1071")
load("BreastCancer.rda")
head(BreastCancer)
library("e1071") 
library("mlbench") 

data(BreastCancer) 
head(BreastCancer)
dim(BreastCancer)
breastCancer <- na.omit(BreastCancer)
is.na(breastCancer)
breastCancer <- breastCancer[,-1]
head(breastCancer)
a = breastCancer[,-10]
head(breastCancer[,10])
b = breastCancer[,10]
modelBreastCancer = train(a,b,'nb',trControl = trainControl(method = 'cv',number = 10))

modelBreastCancer

predict(modelBreastCancer$finalModel,a)

table(predict(modelBreastCancer$finalModel,a)$class,b)

modelBreastCancer$results



																																									