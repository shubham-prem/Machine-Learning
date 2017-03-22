library(mlbench)
library(caret)
library(mice)
library(VIM)
load("C:/Users/Admin/Documents/R/win-library/3.3/mlbench/data/BreastCancer.rda")
View(BreastCancer)
head(BreastCancer)
str(BreastCancer)
md.pattern(BreastCancer)
aggr_plot <- aggr(BreastCancer, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(BreastCancer), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
imputingdata<-mice(BreastCancer,m=5,maxit = 10,method ="cart",seed=100)
summary(imputingdata)
BreastCancer<-complete(imputingdata,2)
View(BreastCancer)
BreastCancer<-BreastCancer[,-1]
View(BreastCancer)
attach(BreastCancer)
## visual representation of imputed data
aggr_plot <- aggr(BreastCancer, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(BreastCancer), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

set.seed(100)

trainIndex<-createDataPartition(BreastCancer$Class,p=0.8,list=FALSE,times=1)
BreastCancerTrainData<-BreastCancer[trainIndex,]
BreastCancerTestData<-BreastCancer[-trainIndex,]

logistic<-glm(Class~.,data=BreastCancerTrainData,family="binomial")
Prdct<-predict(logistic,BreastCancerTestData,type="response")
confusion=table(round(Prdct),BreastCancerTestData$Class)
confusion
logistic_acc<-sum(diag(confusion))/sum(confusion)
logistic_acc

## Accuracy is 92

## CTREE

library(party)
fit<-ctree(Class~.,data=BreastCancerTrainData,controls = ctree_control(maxdepth = 5))
plot(fit)
Prdct<-predict(fit,BreastCancerTestData,type="response")
confusion=table(Prdct,BreastCancerTestData$Class)
confusion
ctree_acc<-sum(diag(confusion))/sum(confusion)
ctree_acc

## Accuracy is 93.5

## RPART

library(rpart)
fit<-rpart(Class~.,method = "class",data =BreastCancerTrainData)
Prdct<-predict(fit,BreastCancerTestData,type="class")
confusion=table(Prdct,BreastCancerTestData$Class)
confusion
rpart_acc<-sum(diag(confusion))/sum(confusion)
rpart_acc

## Accuracy is 91.3

## RANDOM FOREST

library(randomForest)
rfmodel<-randomForest(Class~.,data = BreastCancerTrainData,method= "class")
predicted_data<-predict(rfmodel,BreastCancerTestData)
rf<-table(predicted_data,BreastCancerTestData$Class)
rf
rf_acc<-sum(diag(rf))/sum(rf)
rf_acc
## Accuracy is 94.2

## Naive Bayes

library(e1071)
NaiveB<- naiveBayes(Class ~.,data = BreastCancerTrainData)
NaiveB
pred<-predict(NaiveB,BreastCancerTestData)
tab<-table(pred,BreastCancerTestData$Class)
Naive_acc<-sum(diag(tab))/sum(tab)
Naive_acc

## Accuracy is 96.4

## KNN

library(class)
BreastCancerknn <- knn(train = BreastCancerTrainData[,-10], test = BreastCancerTestData[,-10],cl = BreastCancerTrainData[,10], k=4)
library(gmodels)
CrossTable(BreastCancerTestData[,10],BreastCancerknn,prop.chisq = F)
## sum of diagnols/Sum of total
knn_acc<- (88+44)/139
knn_acc

## Accuracy=94.2

Compare_Acc<- data.frame("Random Forest" = rf_acc,"Logistic" =logistic_acc,"Naive_Bayes" =Naive_acc,"rpart" =rpart_acc,"knn"= knn_acc,"Ctree" =ctree_acc)
Compare_Acc

