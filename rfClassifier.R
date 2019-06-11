# function

#Accuracy and importance function
#initial two matrix
setwd("E:/important file/CENTRALE/BCI ET EEG/Rprocess/data/hugues_auto")
df=read.csv(file="MOHUstoryBioRes MOHUmeditationBioRes _df.csv")
df$MediOrStory<-factor(ifelse(df$MediOrStory%in%c('0'),'0','1')) 


#do a basis SVM using sc and HRV
par(mfrow=c(1,2))
ybounds <- function(y){  # y is the response variable in the dataframe
  bounds = quantile(y, probs=c(0.05, 0.95), type=3, names=FALSE)
  return(bounds + c(-1,1) * 0.2 * (bounds[2]-bounds[1]) )
}
plot(df$SC,df$HRV,type = "n",main = "general presentation of HRV against SC",xlab = "skin conductance(micro Siemens)",ylab="HRV(square of number of heart rate(beat per minute)")
text(df$SC,df$HRV,rownames(df),col=c("blue","red")[df$MediOrStory],cex=0.75)
legend( x="topright", 
        legend=c("meditation","story phase"),
        col=c("red","blue"), lwd=1, 
        pch=c(16,16) )
plot(df$SC,df$HRV,type = "n",ylim=ybounds(df$HRV),main = "detailed tendency of HRV against SC",xlab = "skin conductance(micro Siemens)",ylab="HRV(square of number of heart rate(beat per minute)")
text(df$SC,df$HRV,rownames(df),col=c("blue","red")[df$MediOrStory],cex=0.75)
legend( x="topright", 
        legend=c("meditation","story phase"),
        col=c("red","blue"), lwd=1, 
        pch=c(16,16) )
library(e1071)
mlin_cvtBioM <- svm(MediOrStory ~ SC+HRV, data=df, kernel="linear",scale=F)
print(mlin_cvtBioM)
par(mfrow=c(1,1))
#plot(df$SC,df$HRV,type = "n",main = "general presentation of Heart Rate Variability against Skin Conductance",xlab = "skin conductance(micro Siemens)",ylab="HRV(square of number of heart rate(beat per minute)")
plot(df$SC,df$HRV,type = "n",ylim=ybounds(df$HRV),main = "SVM classification using Heart Rate Variability and Skin Conductance",xlab = "skin conductance(micro Siemens)",ylab="HRV(square of number of heart rate(beat per minute)")
text(df$SC,df$HRV,rownames(df),col=c("blue","red")[df$MediOrStory],cex=0.75)
legend( x="topright", 
        legend=c("meditation","story phase","support vector","seperation line","maximum margin"),
        col=c("red","blue","black","green","grey"), lwd=1, 
        pch=c(16,16) )
points(df$SC[mlin_cvtBioM$index],df$HRV[mlin_cvtBioM$index],cex=3,col=rgb(0,0,0))
beta.0 <- -mlin_cvtBioM$rho
beta.1 <- sum(mlin_cvtBioM$coefs*df$SC[mlin_cvtBioM$index])
beta.2 <- sum(mlin_cvtBioM$coefs*df$HRV[mlin_cvtBioM$index])
abline(-beta.0/beta.2,-beta.1/beta.2,col="green")
abline((-beta.0-1.0)/beta.2,-beta.1/beta.2,col="gray")
abline((-beta.0+1.0)/beta.2,-beta.1/beta.2,col="gray")

#data slicing 
#http://dataaspirant.com/2017/01/19/support-vector-machine-classifier-implementation-r-caret-package/
library(caret)
set.seed(3033)
intrain <- createDataPartition(y = df$SC, p= 0.7, list = FALSE)
training <- df[intrain,]
testing <- df[-intrain,]
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)
svm_Linear <- train(MediOrStory ~SC+HRV, data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
test_pred <- predict(svm_Linear, newdata = testing)
cfsM=confusionMatrix(test_pred, testing$MediOrStory )
confusionSta=data.frame("accuracy"=cfsM$overall[1],"Kappa"=cfsM$overall[2],"Sensistivity"=cfsM$byClass[1],"Specificity"=cfsM$byClass[2],"Pos Pred Vlaue"=cfsM$byClass[3],"Neg Pred Value"=cfsM$byClass[4])
#set path and set filename
write.csv(confusionSta, file = "filename",row.names = FALSE)

#for those who did 10+8 then 4+4, use second experiment as test set
#import training set
setwd("E:/important file/CENTRALE/BCI ET EEG/Rprocess/data/hugues_auto")
df=read.csv(file="MOHUstoryBioRes MOHUmeditationBioRes _df.csv")
library(caret)
df$MediOrStory<-factor(ifelse(df$MediOrStory%in%c('0'),'0','1')) 
training = df
df=read.csv(file="MOHUstoryBioRes MOHUmeditationBioRes 4+4min_df.csv")
df$MediOrStory<-factor(ifelse(df$MediOrStory%in%c('0'),'0','1')) 
testing = df
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
svm_Linear <- train(MediOrStory ~SC+HRV, data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
test_pred <- predict(svm_Linear, newdata = testing)
confusionMatrix(test_pred, testing$MediOrStory )


library(dplyr)
library(e1071)
library(tidyverse)
library(caret)

df_added=select(df,-c(HR,time,HRV,Resp,SC,ST))
for(i in 1:21){
  df_added[paste(i,"thetaDbeta")]=df_added[3*i]/df_added[2*i]
  df_added[paste(i,"alphaDtheta")]=df_added[i]/df_added[3*i]
  
}
df=df_added
names(df) <- make.names(names(df))

training.samples <- df$FP1AlphaMean %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- df[training.samples, ]
test.data <- df[-training.samples, ]
library(randomForest)
Model_rf =randomForest:: randomForest(MediOrStory~., data=train.data)
FeatImpt=randomForest:: importance(Model_rf)
colnames(FeatImpt)=NULL
accuracyMatrix=data.frame("name"="ini","accuracy"=0)
ImptMatrix=data.frame("INI"=FeatImpt)
sumPrdval=c()
sumTest.data=c()
#end of initial

#attention, you need to check the path
#reactivate the second setwd()!
RfAccuracy=function(subjectName){
  setwd("C:/Users/movie/Desktop/Projet Inno/R/savedf")
  df=read.csv(file=paste0(subjectName,"storyBioRes ",subjectName,"meditationBioRes _df.csv") )
  #df=read.csv(file="MOHUstoryBio MOHUmeditationBio _df.csv")
  df$MediOrStory<-factor(ifelse(df$MediOrStory%in%c('0'),'0','1')) 
  library(dplyr)
  library(e1071)
  library(tidyverse)
  library(caret)
  
  df_added=select(df,-c(HR,time,HRV,Resp,SC,ST))
  for(i in 1:21){
    df_added[paste(i,"thetaDbeta")]=df_added[3*i]/df_added[2*i]
    df_added[paste(i,"alphaDtheta")]=df_added[i]/df_added[3*i]
    
  }
  df=df_added
  names(df) <- make.names(names(df))
  
  training.samples <- df$FP1AlphaMean %>%
    createDataPartition(p = 0.8, list = FALSE)
  train.data  <- df[training.samples, ]
  test.data <- df[-training.samples, ]
  library(randomForest)
  Model_rf = randomForest::randomForest(MediOrStory~., data=train.data)
  FeatImpt=randomForest::importance(Model_rf)
  colnames(FeatImpt)=NULL
  ImptMatrix[subjectName]=FeatImpt
  #define number of feature
  num_feature=5
  FeatImptSorted=sort(FeatImpt)
  FeatImptOrdered=rev(order(FeatImpt))
  FeatImptOrdered=FeatImptOrdered[1:num_feature]
  FeatureUsed=c()
  for (i in FeatImptOrdered){
    FeatureUsed=c(FeatureUsed,row.names(FeatImpt)[i])}
  print(FeatureUsed)
  
  rfFrml <- as.formula(paste("MediOrStory", paste(row.names(FeatImpt)[FeatImptOrdered],sepA = "", 
                                                  collapse = " + "), sep = " ~ "))
  
  tc <- trainControl(method = "cv", 3)
  modFitP <- train(rfFrml, method = "rf", data = train.data, trControl = tc, allowParallel=TRUE, importance=TRUE, ntree = 500 )
  print(modFitP)
  prdval=predict(modFitP, test.data)
  sumPrdval <- factor(c(sumPrdval,prdval))
  sumTest.data=factor(c(sumTest.data,test.data$MediOrStory))
  cfsMatrix=confusionMatrix(test.data$MediOrStory, prdval)
  #qplot(MediOrStory, prdval, data=test.data,  colour= MediOrStory, geom = c("boxplot", "jitter"), main = "predicted vs. observed in test data", xlab = "Observed Classe", ylab = "Predicted Classe")
  
  setwd("C:/Users/movie/Desktop/Projet Inno/R/saveqplot")
  
  qplot(MediOrStory, prdval, data=test.data,colour= MediOrStory, geom = c("jitter"),
        main = paste(subjectName, "predicted vs. observed in test data(1 stands for meditation)"), xlab = "Observed Classe",
        ylab = "Predicted Classe")+annotate("text", x=1, y=1, label= cfsMatrix$table[1])+
    annotate("text", x=2, y=1, label= cfsMatrix$table[2])+annotate("text", x=1, y=2, label= cfsMatrix$table[3])+
    annotate("text", x=2, y=2, label= cfsMatrix$table[4])
  ggsave(paste(subjectName,"cmatrix.png"))
  accuracy_rate=cfsMatrix$overall[[1]]
  accuracyMatrix=rbind(accuracyMatrix,data.frame("name"=subjectName,"accuracy"=accuracy_rate))
  ImptAccuracylist=list("accuracyMatrix"=accuracyMatrix,"ImptMatrix"=ImptMatrix,"sumPrdval"=sumPrdval,"sumTest.data"=sumTest.data)
  return(ImptAccuracylist)
}

#main step - change subject name and run the following codes
ImptAccuracy=RfAccuracy("PRBA")
accuracyMatrix=ImptAccuracy$accuracyMatrix
ImptMatrix=ImptAccuracy$ImptMatrix
sumPrdval=ImptAccuracy$sumPrdval
sumTest.data=ImptAccuracy$sumTest.data

# the overall confusion matrix
cfsMatrix=confusionMatrix(sumTest.data,sumPrdval)
qplot(sumTest.data, sumPrdval, colour= sumTest.data, geom = c("jitter"),
      main = paste("overall", "predicted vs. observed in test data(1 stands for meditation)"), xlab = "Observed Classe",
      ylab = "Predicted Classe")+annotate("text", x=1, y=1, label= cfsMatrix$table[1])+
  annotate("text", x=2, y=1, label= cfsMatrix$table[2])+annotate("text", x=1, y=2, label= cfsMatrix$table[3])+
  annotate("text", x=2, y=2, label= cfsMatrix$table[4])
ggsave("overall_cmatrix.png")


RfAccuracy_reExp=function(subjectName){
  setwd("C:/Users/movie/Desktop/Projet Inno/R/savedf")
  df=read.csv(file=paste0(subjectName,"storyBio ",subjectName,"meditationBio _df.csv") )
  df_test=read.csv(file=paste0(subjectName,"meditationBio2 ",subjectName,"storyBio2 4plus4min_df.csv") )
  #df=read.csv(file="MOHUstoryBio MOHUmeditationBio _df.csv")
  df$MediOrStory<-factor(ifelse(df$MediOrStory%in%c('0'),'0','1')) 
  df_test$MediOrStory<-factor(ifelse(df_test$MediOrStory%in%c('0'),'0','1')) 
  library(dplyr)
  library(e1071)
  library(tidyverse)
  library(caret)
  
  df_added=select(df,-c(HR,time,HRV,Resp,SC,ST))
  for(i in 1:21){
    df_added[paste(i,"thetaDbeta")]=df_added[3*i]/df_added[2*i]
    df_added[paste(i,"alphaDtheta")]=df_added[i]/df_added[3*i]
    
  }
  df=df_added
  names(df) <- make.names(names(df))
  
  df_added=select(df_test,-c(HR,time,HRV,Resp,SC,ST))
  for(i in 1:21){
    df_added[paste(i,"thetaDbeta")]=df_added[3*i]/df_added[2*i]
    df_added[paste(i,"alphaDtheta")]=df_added[i]/df_added[3*i]
    
  }
  df_test=df_added
  names(df_test) <- make.names(names(df_test))
  
  
  #training.samples <- df$FP1AlphaMean %>%
  #  createDataPartition(p = 0.8, list = FALSE)
  train.data  <- df
  test.data <- df_test
  library(randomForest)
  Model_rf = randomForest::randomForest(MediOrStory~., data=train.data)
  FeatImpt=randomForest::importance(Model_rf)
  colnames(FeatImpt)=NULL
  ImptMatrix[subjectName]=FeatImpt
  #define number of feature
  num_feature=5
  FeatImptSorted=sort(FeatImpt)
  FeatImptOrdered=rev(order(FeatImpt))
  FeatImptOrdered=FeatImptOrdered[1:num_feature]
  FeatureUsed=c()
  for (i in FeatImptOrdered){
    FeatureUsed=c(FeatureUsed,row.names(FeatImpt)[i])}
  print(FeatureUsed)
  
  rfFrml <- as.formula(paste("MediOrStory", paste(row.names(FeatImpt)[FeatImptOrdered],sepA = "", 
                                                  collapse = " + "), sep = " ~ "))
  
  tc <- trainControl(method = "cv", 3)
  modFitP <- train(rfFrml, method = "rf", data = train.data, trControl = tc, allowParallel=TRUE, importance=TRUE, ntree = 500 )
  print(modFitP)
  prdval=predict(modFitP, test.data)
  sumPrdval <- factor(c(sumPrdval,prdval))
  sumTest.data=factor(c(sumTest.data,test.data$MediOrStory))
  cfsMatrix=confusionMatrix(test.data$MediOrStory, prdval)
  #qplot(MediOrStory, prdval, data=test.data,  colour= MediOrStory, geom = c("boxplot", "jitter"), main = "predicted vs. observed in test data", xlab = "Observed Classe", ylab = "Predicted Classe")
  
  setwd("C:/Users/movie/Desktop/Projet Inno/R/saveqplot")
  
  qplot(MediOrStory, prdval, data=test.data,colour= MediOrStory, geom = c("jitter"),
        main = paste(subjectName, "predicted vs. observed in test data(1 stands for meditation)"), xlab = "Observed Classe",
        ylab = "Predicted Classe")+annotate("text", x=1, y=1, label= cfsMatrix$table[2])+
    annotate("text", x=1, y=2, label= cfsMatrix$table[4])
  ggsave(paste(subjectName,"cmatrix4min.png"))
  accuracy_rate=cfsMatrix$overall[[1]]
  accuracyMatrix=rbind(accuracyMatrix,data.frame("name"=subjectName,"accuracy"=accuracy_rate))
  ImptAccuracylist=list("accuracyMatrix"=accuracyMatrix,"ImptMatrix"=ImptMatrix,"sumPrdval"=sumPrdval,"sumTest.data"=sumTest.data)
  return(ImptAccuracylist)
}
ImptAccuracy=RfAccuracy_reExp("PRBA")
############################################################################################################################################################

RfAccuracy_reExp2=function(subjectName){
  setwd("E:/important file/CENTRALE/BCI ET EEG/Rprocess/data/hugues_auto")
  df=read.csv(file=paste0(subjectName,"storyBio ",subjectName,"meditationBio _df.csv") )
  df_test=read.csv(file=paste0(subjectName,"meditation2Bio 4min_df.csv") )
  #df=read.csv(file="MOHUstoryBio MOHUmeditationBio _df.csv")
  df$MediOrStory<-factor(ifelse(df$MediOrStory%in%c('0'),'0','1')) 
  df_test$MediOrStory<-factor(ifelse(df_test$MediOrStory%in%c('0'),'0','1')) 
  library(dplyr)
  library(e1071)
  library(tidyverse)
  library(caret)
  
  df_added=select(df,-c(HR,time,HRV,Resp,SC,ST))
  for(i in 1:21){
    df_added[paste(i,"thetaDbeta")]=df_added[3*i]/df_added[2*i]
    df_added[paste(i,"alphaDtheta")]=df_added[i]/df_added[3*i]
    
  }
  df=df_added
  names(df) <- make.names(names(df))
  
  df_added=select(df_test,-c(HR,time,HRV,Resp,SC,ST))
  for(i in 1:21){
    df_added[paste(i,"thetaDbeta")]=df_added[3*i]/df_added[2*i]
    df_added[paste(i,"alphaDtheta")]=df_added[i]/df_added[3*i]
    
  }
  df_test=df_added
  names(df_test) <- make.names(names(df_test))
  
  
  #training.samples <- df$FP1AlphaMean %>%
  #  createDataPartition(p = 0.8, list = FALSE)
  train.data  <- df
  test.data <- df_test
  library(randomForest)
  Model_rf = randomForest::randomForest(MediOrStory~., data=train.data)
  FeatImpt=randomForest::importance(Model_rf)
  colnames(FeatImpt)=NULL
  ImptMatrix[subjectName]=FeatImpt
  #define number of feature
  num_feature=5
  FeatImptSorted=sort(FeatImpt)
  FeatImptOrdered=rev(order(FeatImpt))
  FeatImptOrdered=FeatImptOrdered[1:num_feature]
  FeatureUsed=c()
  for (i in FeatImptOrdered){
    FeatureUsed=c(FeatureUsed,row.names(FeatImpt)[i])}
  print(FeatureUsed)
  
  rfFrml <- as.formula(paste("MediOrStory", paste(row.names(FeatImpt)[FeatImptOrdered],sepA = "", 
                                                  collapse = " + "), sep = " ~ "))
  
  tc <- trainControl(method = "cv", 3)
  modFitP <- train(rfFrml, method = "rf", data = train.data, trControl = tc, allowParallel=TRUE, importance=TRUE, ntree = 500 )
  print(modFitP)
  prdval=predict(modFitP, test.data)
  sumPrdval <- factor(c(sumPrdval,prdval))
  sumTest.data=factor(c(sumTest.data,test.data$MediOrStory))
  cfsMatrix=confusionMatrix(test.data$MediOrStory, prdval)
  #qplot(MediOrStory, prdval, data=test.data,  colour= MediOrStory, geom = c("boxplot", "jitter"), main = "predicted vs. observed in test data", xlab = "Observed Classe", ylab = "Predicted Classe")
  
  setwd("C:/Users/movie/Desktop/Projet Inno/R/saveqplot")
  
  qplot(MediOrStory, prdval, data=test.data,colour= MediOrStory, geom = c("jitter"),
        main = paste(subjectName, "predicted vs. observed in test data(1 stands for meditation)"), xlab = "Observed Classe",
        ylab = "Predicted Classe")+annotate("text", x=1, y=1, label= cfsMatrix$table[2])+
    annotate("text", x=1, y=2, label= cfsMatrix$table[4])
  ggsave(paste(subjectName,"cmatrix4min.png"))
  accuracy_rate=cfsMatrix$overall[[1]]
  accuracyMatrix=rbind(accuracyMatrix,data.frame("name"=subjectName,"accuracy"=accuracy_rate))
  ImptAccuracylist=list("accuracyMatrix"=accuracyMatrix,"ImptMatrix"=ImptMatrix,"sumPrdval"=sumPrdval,"sumTest.data"=sumTest.data)
  return(ImptAccuracylist)
}


# below can be used when needed
setwd("C:/Users/movie/Desktop/Projet Inno/R/savedf")
df=read.csv(file="MOHUstoryBio MOHUmeditationBio _df.csv")
df$MediOrStory<-factor(ifelse(df$MediOrStory%in%c('0'),'0','1'))


library(e1071)
library(tidyverse)
library(caret)

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
install.packages("dplyr")


ImptAccuracy=RfAccuracy("BOOU")

accuracyMatrix=ImptAccuracy$accuracyMatrix
ImptMatrix=ImptAccuracy$ImptMatrix


write.csv(accuracyMatrix,file="accuracyMatrix3.csv")
accuracyM=read.csv(file="accuracyMatrix3.csv")
accuracyM=accuracyM[3]
accuracyM=accuracyM[-c(1),]
boxplot(accuracyM,horizontal = T,main="Box Plot Accuracy")

write.csv(ImptMatrix,file="importantMatrix3.csv")
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





