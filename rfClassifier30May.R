# function

#Accuracy and importance function
setwd("C:/Users/movie/Desktop/Projet Inno/R/savedf")
df=read.csv(file="CACAstoryBio CACAmeditationBio _df.csv")
PACS=read.csv(file="CACA_storyPAC.csv",header = F)
PACM=read.csv(file="CACA_meditationPAC.csv", header = F)
PAC=rbind(PACS,PACM)
PAC=PAC[1:21]


#PACmatrix=data.frame("PACelectrode1"=PAC[1])

for (i in 1:21){
  names(PAC)[names(PAC) == paste0("V",i)] =paste0("PACelectrode",i)
} 
dfPAC=cbind(df,PAC)
write.csv(file = "CACAPAC.csv",dfPAC,row.names = F)

##initial
setwd("C:/Users/movie/Desktop/Projet Inno/R/savedf")
df=read.csv(file="CACAPAC.csv")
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
Model_rf =randomForest:: randomForest(MediOrStory~., data=train.data)
FeatImpt=randomForest:: importance(Model_rf)
colnames(FeatImpt)=NULL
accuracyMatrix=data.frame("name"="ini","accuracy"=0)
ImptMatrix=data.frame("INI"=FeatImpt)
sumPrdval=c()
sumTest.data=c()

###run MATA To TEST PAC#######
setwd("C:/Users/movie/Desktop/Projet Inno/R/savedf")
df=read.csv(file="CACAPAC.csv")
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
#ImptMatrix[subjectName]=FeatImpt
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
      main = paste("CACA", "predicted vs. observed in test data using PAC as feature"), xlab = "Observed Classe",
      ylab = "Predicted Classe")+annotate("text", x=1, y=1, label= cfsMatrix$table[1])+
  annotate("text", x=2, y=1, label= cfsMatrix$table[2])+annotate("text", x=1, y=2, label= cfsMatrix$table[3])+
  annotate("text", x=2, y=2, label= cfsMatrix$table[4])
ggsave(paste(subjectName,"cmatrix.png"))
accuracy_rate=cfsMatrix$overall[[1]]
accuracyMatrix=rbind(accuracyMatrix,data.frame("name"=subjectName,"accuracy"=accuracy_rate))
ImptAccuracylist=list("accuracyMatrix"=accuracyMatrix,"ImptMatrix"=ImptMatrix,"sumPrdval"=sumPrdval,"sumTest.data"=sumTest.data)



##################################################################################################################
#initial two matrix

setwd("C:/Users/movie/Desktop/Projet Inno/R/savedf")
df=read.csv(file="MOHUstoryBio MOHUmeditationBio _df.csv")
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
Model_rf =randomForest:: randomForest(MediOrStory~., data=train.data)
FeatImpt=randomForest:: importance(Model_rf)
colnames(FeatImpt)=NULL
accuracyMatrix=data.frame("name"="ini","accuracy"=0)
ImptMatrix=data.frame("INI"=FeatImpt)
sumPrdval=c()
sumTest.data=c()
#end of initial
############################################################################################################
#attention, you need to check the path
#reactivate the second setwd()!
RfAccuracy=function(subjectName){
  setwd("C:/Users/movie/Desktop/Projet Inno/R/savedf")
  df=read.csv(file=paste0(subjectName,"storyBio ",subjectName,"meditationBio _df.csv") )
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
  confusionSta=data.frame("accuracy"=cfsMatrix$overall[1],"Kappa"=cfsMatrix$overall[2],"Sensistivity"=cfsMatrix$byClass[1],"Specificity"=cfsMatrix$byClass[2],"Pos Pred Vlaue"=cfsMatrix$byClass[3],"Neg Pred Value"=cfsMatrix$byClass[4])
  setwd("C:/Users/movie/Desktop/Projet Inno/R/rfSta")
  write.csv(file=paste0(subjectName,"rfWtaWithoutPAC.csv"),confusionSta)
  
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
ImptAccuracy=RfAccuracy("CACA")
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
ImptAccuracy=RfAccuracy_reExp("PRBA")
############################################################################################################################################################

RfAccuracy_reExp2=function(subjectName){
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
        ylab = "Predicted Classe")+annotate("text", x=1, y=1, label= cfsMatrix$table[1])+
    annotate("text", x=2, y=1, label= cfsMatrix$table[2])+annotate("text", x=1, y=2, label= cfsMatrix$table[3])+
    annotate("text", x=2, y=2, label= cfsMatrix$table[4])
  ggsave(paste(subjectName,"cmatrix4min.png"))
  accuracy_rate=cfsMatrix$overall[[1]]
  accuracyMatrix=rbind(accuracyMatrix,data.frame("name"=subjectName,"accuracy"=accuracy_rate))
  ImptAccuracylist=list("accuracyMatrix"=accuracyMatrix,"ImptMatrix"=ImptMatrix,"sumPrdval"=sumPrdval,"sumTest.data"=sumTest.data)
  return(ImptAccuracylist)
}

ImptAccuracy=RfAccuracy_reExp2("ETLA")


#######################################################################################################################################################################
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
######################################### Tuning train##########################################################
##################################################################################################################
#initial two matrix

setwd("E:/important file/CENTRALE/BCI ET EEG/Rprocess/data/hugues_auto")
df=read.csv(file="MOHUstoryBioRes MOHUmeditationBioRes _df.csv")
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
Model_rf =randomForest:: randomForest(MediOrStory~., data=train.data)
FeatImpt=randomForest:: importance(Model_rf)
colnames(FeatImpt)=NULL
accuracyMatrix=data.frame("name"="ini","accuracy"=0)
ImptMatrix=data.frame("INI"=FeatImpt)
sumPrdval=c()
sumTest.data=c()
#end of initial
############################################################################################################
#attention, you need to check the path
#reactivate the second setwd()!




RfAccuracy=function(subjectName){
  setwd("E:/important file/CENTRALE/BCI ET EEG/Rprocess/data/hugues_auto")
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
  num_feature=10
  FeatImptSorted=sort(FeatImpt)
  FeatImptOrdered=rev(order(FeatImpt))
  FeatImptOrdered=FeatImptOrdered[1:num_feature]
  FeatureUsed=c()
  for (i in FeatImptOrdered){
    FeatureUsed=c(FeatureUsed,row.names(FeatImpt)[i])}
  print(FeatureUsed)
  
  rfFrml <- as.formula(paste("MediOrStory", paste(row.names(FeatImpt)[FeatImptOrdered],sepA = "", 
                                                  collapse = " + "), sep = " ~ "))
  
  tc <- trainControl(method = "cv", 10 ,savePredictions = "final",search = "random")
  set.seed(2456)
  modFitP <- train(rfFrml, method = "rf", data = train.data, ntree=700, trControl = tc, allowParallel=TRUE, importance=TRUE,tuneLength=10 )
  print(modFitP)
  plot(modFitP)
  prdval=predict(modFitP, test.data)
  sumPrdval <- factor(c(sumPrdval,prdval))
  sumTest.data=factor(c(sumTest.data,test.data$MediOrStory))
  cfsMatrix=confusionMatrix(test.data$MediOrStory, prdval)
  confusionSta=data.frame("accuracy"=cfsMatrix$overall[1],"Kappa"=cfsMatrix$overall[2],"Sensistivity"=cfsMatrix$byClass[1],"Specificity"=cfsMatrix$byClass[2],"Pos Pred Vlaue"=cfsMatrix$byClass[3],"Neg Pred Value"=cfsMatrix$byClass[4])
  setwd("C:/Users/movie/Desktop/Projet Inno/R/rfSta")
  write.csv(file=paste0(subjectName,"rfWtaWithoutPAC.csv"),confusionSta)
  
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





