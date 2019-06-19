#This function is aimed to obtain 
#an individual confusion matrix of rf classifier and extract stas
#the overall cfm
#the over all statistic
#Obtain top 5 important feature and its importance
#create a frequency matrix

#####################################Test setup##########################################
subjectName="BOOU"
###########################################################################################

RfPAC=function(subjectName,num_feature=5){
  setwd("D:/20182019EEG_CS/df_matrix")
  df=read.csv(file=paste0(subjectName,"storyBio ",subjectName,"meditationBio _df.csv"))
  setwd("D:/20182019EEG_CS/PACmatrix")
  PACS=read.csv(file=paste0("AUTO_",subjectName,"_story.csv"),header = F)
  PACM=read.csv(file=paste0("AUTO_",subjectName,"_meditation.csv"), header = F)
  PAC=rbind(PACS,PACM)
  PAC=PAC[1:21]
  List_eletrode=c("FP1","FP2","F7","F3","Fz","F4","F8","T3","C3","Cz","C4","T4","T5","P3","PZ","P4","T6","O1","O2","A1","A2")
  for (i in 1:21){
    names(PAC)[names(PAC) == paste0("V",i)] =paste0(List_eletrode[i],"PAC")
  } 
  dfPAC=cbind(df,PAC)
  setwd("D:/20182019EEG_CS/df_withPAC_matrix")
  write.csv(file = paste0(subjectName,"df_withPAC.csv"),dfPAC,row.names = F)
  library(dplyr)
  library(e1071)
  library(tidyverse)
  library(caret)
  df=dfPAC
  df_added=select(df,-c(HR,time,HRV,Resp,SC,ST))
  for(i in 1:21){
    df_added[paste0(List_eletrode[i],"TBR")]=df_added[3*i]/df_added[2*i]
    df_added[paste0(List_eletrode[i],"ATR")]=df_added[i]/df_added[3*i]
    
  }
  df=df_added
  names(df) <- make.names(names(df))
  df$MediOrStory<-factor(ifelse(df$MediOrStory%in%c('0'),'0','1')) 
  
  training.samples <- df$FP1AlphaMean %>%
    createDataPartition(p = 0.8, list = FALSE)
  train.data  <- df[training.samples, ]
  test.data <- df[-training.samples, ]
  library(randomForest)
  Model_rf =randomForest:: randomForest(MediOrStory~., data=train.data)
  FeatImpt=randomForest:: importance(Model_rf)
##################################### extract feature info ####################################  
  FeatImptSorted=sort(FeatImpt)
  FeatImptOrdered=rev(order(FeatImpt))
  FeatImptOrdered=FeatImptOrdered[1:num_feature]
  FeatureUsed=c()
  FeatureUsed_impt=c()
  FeatureUsed_rank=c()
  j=1
  for (i in FeatImptOrdered){
    FeatureUsed=c(FeatureUsed,row.names(FeatImpt)[i])
    FeatureUsed_impt=c(FeatureUsed_impt,FeatImpt[i])
    FeatureUsed_rank=c(FeatureUsed_rank,j)
    j=j+1
  }
  rm(j)
  Feature_cloud=cbind(FeatureUsed,FeatureUsed_impt,FeatureUsed_rank)
  #####################################################
  #colnames(FeatImpt)=NULL
  #accuracyMatrix=data.frame("name"="ini","accuracy"=0)
  #ImptMatrix=data.frame("INI"=FeatImpt)
  sumPrdval=c()
  sumTest.data=c()
  ################################################### start traning########################################
  rfFrml <- as.formula(paste("MediOrStory", paste(row.names(FeatImpt)[FeatImptOrdered],sepA = "", 
                                                  collapse = " + "), sep = " ~ "))
  
  tc <- trainControl(method = "cv", 3)
  modFitP <- train(rfFrml, method = "rf", data = train.data, trControl = tc, allowParallel=TRUE, importance=TRUE, ntree = 500 )
  print(modFitP)
  prdval=predict(modFitP, test.data)
  #sumPrdval <- factor(c(sumPrdval,prdval),c)
  Test.data=test.data$MediOrStory
  cfsMatrix=confusionMatrix(test.data$MediOrStory, prdval,positive='1')
  ####################################### visulise the confusion matrix ####################
  setwd("D:/20182019EEG_CS/plot_confm_withPAC")
  
  qplot(MediOrStory, prdval, data=test.data,colour= MediOrStory, geom = c("jitter"),
        main = paste("CACA", "predicted vs. observed in test data using PAC as feature"), xlab = "Observed Classe",
        ylab = "Predicted Classe")+annotate("text", x=1, y=1, label= cfsMatrix$table[1])+
    annotate("text", x=2, y=1, label= cfsMatrix$table[2])+annotate("text", x=1, y=2, label= cfsMatrix$table[3])+
    annotate("text", x=2, y=2, label= cfsMatrix$table[4])
  ggsave(paste0(subjectName,"cfsmatrix.png"))
  
  ####################################### obtain statistics #################################
  sta=c(cfsMatrix$overall[1],cfsMatrix$overall[2],cfsMatrix$byClass[1],cfsMatrix$byClass[2],
                            cfsMatrix$byClass[3],cfsMatrix$byClass[4])
  return(list(prdval,Test.data,sta,Feature_cloud))
}

RfPAC_main=function(){
  info=RfPAC("BOOU")
  sumPrdval=unlist(info[1])
  sumTest.data=unlist(info[2])
  sta=data.frame( "BOOU"=unlist(info[3]))
  Feature_cloud=unlist(info[4])
  Feature_cloud= matrix(Feature_cloud,nrow = 5,ncol = 3)
  colnames(Feature_cloud)=c("Fname","impt","rank")
  #subject_list=c("CACA","DEER")
  #we need to add ETLA
  subject_list=c("CACA","DEER","EZLE","GRCL","LENO","LOVA","MATA","MOHU","NIJO","PAMA","PRBA","TRWI","ZAMI")
  for (i in subject_list){
    info=RfPAC(i)
    sumPrdval=c(sumPrdval,unlist(info[1]))
    sumTest.data=c(sumTest.data,unlist(info[2]))
    sta=cbind(sta,data.frame(i= unlist(info[3])))
    Feature_cloud_new=unlist(info[4])
    Feature_cloud_new= matrix(Feature_cloud_new,nrow = 5,ncol = 3)
    Feature_cloud=rbind(Feature_cloud,Feature_cloud_new)
  }

  sumPrdval<-factor(ifelse(sumPrdval%in%c('1'),'0','1')) 
  sumTest.data<-factor(ifelse(sumTest.data%in%c('1'),'0','1')) 
  cfsMatrix=confusionMatrix(sumTest.data,sumPrdval)
  
  qplot(sumTest.data, sumPrdval, colour= sumTest.data, geom = c("jitter"),
        main = paste("overall", "predicted vs. observed in test data(1 stands for meditation)"), xlab = "Observed Classe",
        ylab = "Predicted Classe")+annotate("text", x=1, y=1, label= cfsMatrix$table[1])+
    annotate("text", x=2, y=1, label= cfsMatrix$table[2])+annotate("text", x=1, y=2, label= cfsMatrix$table[3])+
    annotate("text", x=2, y=2, label= cfsMatrix$table[4])
  setwd("D:/20182019EEG_CS/plot_confm_withPAC")
  ggsave("overall confusionM with PAC and ratio.png")
  boxplot(t(sta))
  write.csv(file="rf statistics with PAC ration 14 subjects.csv",t(sta))
  print(cfsMatrix$overall[1])
  print(cfsMatrix$overall[2])
  print(cfsMatrix$byClass[1])
  print(cfsMatrix$byClass[2])
  print(cfsMatrix$byClass[3])
  print(cfsMatrix$byClass[4])
  return(Feature_cloud)
}

#############################################statistic processing#################################################
staProcs=function(){
  setwd("D:/20182019EEG_CS/plot_confm_withPAC")
  sta=read.csv(file = "rf statistics with PAC ration 14 subjects.csv")
  sta=sta[,-1]
  summary(sta)
  sapply(sta,sd)
  
}

Feature_cloud= RfPAC_main()
