setwd("E:/important file/CENTRALE/BCI ET EEG/Rprocess/data/hugues_auto")
df=read.csv(file="MOHUstoryBioRes MOHUmeditationBioRes _df.csv")
df$MediOrStory<-factor(ifelse(df$MediOrStory%in%c('0'),'0','1'))

#add features
#install.packages("dplyr")
library(dplyr)
df_added=select(df,-c(HR,time,HRV,Resp,SC,ST))
for (i in 1:21){
  df_added[paste(i,"thetaDBeta")]=df_added [3*i]/df_added[2*i]
}
df=df_added
names(df) <- make.names(names(df))

#dfslted=df[c("SC","ST","MediOrStory")]
#print(summary(dfslted))
#plot(dfslted$SC,dfslted$ST,type="n")
#text(dfslted$SC,dfslted$ST,rownames(dfslted),col=c("blue","red")[dfslted$MediOrStory],cex=0.75)
#legend("topright", inset=.05, title="two state",
#       c("Story","Mdt"), fill=c("blue","red"), horiz=TRUE)
#install.packages("e1071")
#install.packages("tidyverse")
#install.packages("caret")

library(e1071)
library(tidyverse)
library(caret)
#select feature
#dfslted=df[c("FP1AlphaMean","FP2DeltaMean","FP1DeltaMean","F3DeltaMean","MediOrStory")]
#cross validation 
#set.seed(123)
training.samples <- df$FP1AlphaMean %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- df[training.samples, ]
test.data <- df[-training.samples, ]

# Model_svm=svm(formula = MediOrStory ~F7DeltaMean+FP1DeltaMean+T3BetaMean+T4DeltaMean+T5DeltaMean+O2BetaMean+A1DeltaMean+A2DeltaMean, data = train.data, kernel = "radial", scale = F)
# summary(Model_svm)
# 
# y_predict=predict(Model_svm,newdata = test.data)
# y_predict=as.numeric(levels(y_predict))[y_predict]
# y_label=as.numeric(levels(test.data$MediOrStory))[test.data$MediOrStory]
# accuracy_rate=1-sum(abs(y_predict-y_label))/length(y_label)

##save importance

#ImptMatrix=data.frame("INI"=FeatImpt)
library(randomForest)

Model_rf = randomForest(MediOrStory~., data=train.data)
FeatImpt=importance(Model_rf)
colnames(FeatImpt)=NULL
ImptMatrix["ZAMI"]=FeatImpt
# when you run through all the subjects
setwd("C:/Users/movie/Desktop/Projet Inno/R/savedf")
write.csv(ImptMatrix,file="FeatureImptMatrix.csv")

#to see which feature that we used
cfsMatrix=Model_rf$confusion
#print(importance(Model_rf,type = 2)) 


#accuracy function
#edit path and file name
accuracyMatrix=data.frame("name"="ini","accuracy"=0)
RfAccuracy=function(subjectName){
  setwd("E:/important file/CENTRALE/BCI ET EEG/Rprocess/data/hugues_auto")
  df=read.csv(file="MOHUstoryBioRes MOHUmeditationBioRes _df.csv")
  df$MediOrStory<-factor(ifelse(df$MediOrStory%in%c('0'),'0','1'))  
  library(e1071)
  library(tidyverse)
  library(caret)
  training.samples <- df$FP1AlphaMean %>%
    createDataPartition(p = 0.8, list = FALSE)
  train.data  <- df[training.samples, ]
  test.data <- df[-training.samples, ]
  library(randomForest)
  Model_rf = randomForest(MediOrStory~.-time-SC-ST-HR-HRV-Resp, data=train.data)
  FeatImpt=importance(Model_rf)
  #define number of feature
  num_feature=5
  FeatImptSorted=sort(FeatImpt)
  FeatImptOrdered=rev(order(FeatImpt))
  FeatImptOrdered=FeatImptOrdered[1:num_feature]
  FeatureUsed=c()
  for (i in FeatImptOrdered){
    FeatureUsed=c(FeatureUsed,row.names(FeatImpt)[i])}
  
  rfFrml <- as.formula(paste("MediOrStory", paste(row.names(FeatImpt)[FeatImptOrdered],sep = "", 
                                                  collapse = " + "), sep = " ~ "))
  Model_rf = randomForest(rfFrml, data=train.data)
  y_predict=predict(Model_rf,newdata = test.data)
  y_predict=as.numeric(levels(y_predict))[y_predict]
  y_label=as.numeric(levels(test.data$MediOrStory))[test.data$MediOrStory]
  accuracy_rate=1-sum(abs(y_predict-y_label))/length(y_label)
  accuracyMatrix=rbind(accuracyMatrix,data.frame("name"=subjectName,"accuracy"=accuracy_rate))
  return(accuracyMatrix)
}
accuracyMatrix=RfAccuracy("MOHU")

#install.packages("randomForest")

library(randomForest)
Model_rf = randomForest(MediOrStory~.-time-SC-ST-HR-HRV-Resp, data=train.data)
FeatImpt=importance(Model_rf)
#define number of feature
num_feature=5
FeatImptSorted=sort(FeatImpt)
FeatImptOrdered=rev(order(FeatImpt))
FeatImptOrdered=FeatImptOrdered[1:num_feature]

FeatureUsed=c()
for (i in FeatImptOrdered){
  FeatureUsed=c(FeatureUsed,row.names(FeatImpt)[i])}

rfFrml <- as.formula(paste("MediOrStory", paste(row.names(FeatImpt)[FeatImptOrdered],sep = "", 
                                                         collapse = " + "), sep = " ~ "))
Model_rf = randomForest(rfFrml, data=train.data)
#Model_rf = randomForest(MediOrStory ~F7DeltaMean+FP1DeltaMean+T3BetaMean+T4DeltaMean+T5DeltaMean+O2BetaMean+A1DeltaMean+A2DeltaMean, data=train.data)
y_predict=predict(Model_rf,newdata = test.data)
y_predict=as.numeric(levels(y_predict))[y_predict]
y_label=as.numeric(levels(test.data$MediOrStory))[test.data$MediOrStory]
accuracy_rate=1-sum(abs(y_predict-y_label))/length(y_label)
accuracyVect=c()
accuracyVect=c(accuracyVect,accuracy_rate)


#plot(MS_lin,dfslted)
#legend("top",title="two state",c("1 stands for meditation","0 stands for story phase"))
#sigmoid
#MS_poly=svm(formula = MediOrStory ~ SC + ST, data = dfslted, kernel = "poly", scale = F)
#plot(MS_poly,dfslted)


MS_lin=svm(formula = MediOrStory ~ FP1BetaMean + T4BetaMean+FP1DeltaMean, data = dfslted, kernel = "radial", scale = F)
plot(MS_lin,dfslted)
legend("top",title="two state",c("1 stands for meditation","0 stands for story phase"))

print(attributes(MS_lin))
points(dfslted$SC[MS_lin$index],dfslted$ST[MS_lin$index],cex=3,col=rgb(0,0,0))
beta.0 <- -MS_lin$rho
print(beta.0)
beta.1 <- sum(MS_lin$coefs*dfslted$SC[MS_lin$index])
beta.2 <- sum(MS_lin$coefs*dfslted$ST[MS_lin$index])
print(paste(beta.1,beta.2))
abline(-beta.0/beta.2,-beta.1/beta.2,col="green")


#example
data(cats, package = "MASS")
m <- svm(Sex~., data = cats)
plot(m, cats)
## more than two variables: fix 2 dimensions
data(iris)
m2 <- svm(Species~., data = iris)
plot(m2, iris, Petal.Width ~ Petal.Length,slice = list(Sepal.Width = 3, Sepal.Length = 4))
## plot with custom symbols and colors
plot(m, cats, svSymbol = 1, dataSymbol = 2, symbolPalette = rainbow(4),color.palette=terrain.colors)


#new test
install.packages("rattle")
library(rattle)
tc <- trainControl(method = "cv", 3)
modFitP <- train(MediOrStory ~., method = "rf", data = train.data, trControl = tc, allowParallel=TRUE, importance=TRUE, ntree = 250 )
modFitP
prdval <- predict(modFitP, test.data)
cfsMatrix=confusionMatrix(test.data$MediOrStory, prdval)
#qplot(MediOrStory, prdval, data=test.data,  colour= MediOrStory, geom = c("boxplot", "jitter"), main = "predicted vs. observed in test data", xlab = "Observed Classe", ylab = "Predicted Classe")
qplot(MediOrStory, prdval, data=test.data,colour= MediOrStory, geom = c("jitter"),
      main = "predicted vs. observed in test data", xlab = "Observed Classe",
      ylab = "Predicted Classe")+annotate("text", x=1, y=1, label= cfsMatrix$table[1])+
  annotate("text", x=2, y=1, label= cfsMatrix$table[2])+annotate("text", x=1, y=2, label= cfsMatrix$table[3])+
  annotate("text", x=2, y=2, label= cfsMatrix$table[4])


plot(cfsMatrix$table)

#ROC
install.packages("pROC")
require(pROC)
rf.roc=roc(test.data$MediOrStory,modFitP$)
Model_rf$votes[,2]
modFitP$



require(randomForest)
data(iris)

# This will make drop a class to make it a 2 class problem
iris<-iris[-which(iris$Species=="setosa"),]
iris$Species<-as.factor(as.character(iris$Species))

set.seed(71)
iris.rf<-randomForest(Species ~ ., data=iris,ntree=10)
require(pROC)
rf.roc<-roc(iris$Species,iris.rf$votes[,2])
plot(rf.roc)
auc(rf.roc)