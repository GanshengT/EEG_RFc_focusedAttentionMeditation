#please set up file path before using this function
# ex: setwd("E:/important file/CENTRALE/BCI ET EEG/Rprocess/data")
# the data file (.txt) should be included in the file path
#also the txt file should be modified as following:
##delete [30] and "," at the end of this sentence, then dedlete <end of exported RAW data> at the end
#change SC/GSR S,Temp to simple SC, ST
# ex:using two filename as argument
#HU=df_generation("MOHUstoryBioRes","MOHUmeditationBioRes")

#!!go to the end of this fonction to modify the saving path
#at the end it will generate a csv file which  store all necessary information

df_generation=function(MdtFilename){
  setwd("E:/important file/CENTRALE/BCI ET EEG/Rprocess/data")
  #filename=readline(prompt = "Enter the data file name")
  HU=read.table(file=paste0(MdtFilename,".txt"),header = T,sep = ",",dec = ".",skip = 11,fill = T)
  #keep the same size 240S
  HU=HU[((nrow(HU)-242*256)/2+1):((nrow(HU)-242*256)/2+242*256),]
  library(signal)
  spec=specgram(x=HU$FP1,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)
  FP1Alpha=P[(2*8+1):(2*12),]
  FP1Beta=P[(2*12+1):(2*30),]
  FP1Theta=P[(2*4+1):(2*8),]
  FP1Delta=P[(2*1+1):(2*4),]
  
  # apply on FP2
  spec=specgram(x=HU$FP2,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)

  FP2Alpha=P[(2*8+1):(2*12),]
  FP2Beta=P[(2*12+1):(2*30),]
  FP2Theta=P[(2*4+1):(2*8),]
  FP2Delta=P[(2*1+1):(2*4),]
  
  # apply on F7
  spec=specgram(x=HU$F7,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)

  F7Alpha=P[(2*8+1):(2*12),]
  F7Beta=P[(2*12+1):(2*30),]
  F7Theta=P[(2*4+1):(2*8),]
  F7Delta=P[(2*1+1):(2*4),]
  
  # apply on F3
  spec=specgram(x=HU$F3,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)

  F3Alpha=P[(2*8+1):(2*12),]
  F3Beta=P[(2*12+1):(2*30),]
  F3Theta=P[(2*4+1):(2*8),]
  F3Delta=P[(2*1+1):(2*4),]
  
  # apply on Fz
  spec=specgram(x=HU$Fz,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)

  FzAlpha=P[(2*8+1):(2*12),]
  FzBeta=P[(2*12+1):(2*30),]
  FzTheta=P[(2*4+1):(2*8),]
  FzDelta=P[(2*1+1):(2*4),]
  
  # apply on F4
  spec=specgram(x=HU$F4,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)

  F4Alpha=P[(2*8+1):(2*12),]
  F4Beta=P[(2*12+1):(2*30),]
  F4Theta=P[(2*4+1):(2*8),]
  F4Delta=P[(2*1+1):(2*4),]
  
  # apply on F8
  spec=specgram(x=HU$F8,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)

  F8Alpha=P[(2*8+1):(2*12),]
  F8Beta=P[(2*12+1):(2*30),]
  F8Theta=P[(2*4+1):(2*8),]
  F8Delta=P[(2*1+1):(2*4),]
  
  # apply on T3
  spec=specgram(x=HU$T3,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)

  T3Alpha=P[(2*8+1):(2*12),]
  T3Beta=P[(2*12+1):(2*30),]
  T3Theta=P[(2*4+1):(2*8),]
  T3Delta=P[(2*1+1):(2*4),]
  
  # apply on C3
  spec=specgram(x=HU$C3,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)

  #calculate average band power
  C3Alpha=P[(2*8+1):(2*12),]
  C3Beta=P[(2*12+1):(2*30),]
  C3Theta=P[(2*4+1):(2*8),]
  C3Delta=P[(2*1+1):(2*4),]
  
  # apply on Cz,C4,T4,T5,P3,PZ,P4,T6,o1,o2,A1,A2
  spec=specgram(x=HU$Cz,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)

  #calculate average band power
  CzAlpha=P[(2*8+1):(2*12),]
  CzBeta=P[(2*12+1):(2*30),]
  CzTheta=P[(2*4+1):(2*8),]
  CzDelta=P[(2*1+1):(2*4),]
  
  spec=specgram(x=HU$C4,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)

  #calculate average band power
  C4Alpha=P[(2*8+1):(2*12),]
  C4Beta=P[(2*12+1):(2*30),]
  C4Theta=P[(2*4+1):(2*8),]
  C4Delta=P[(2*1+1):(2*4),]
  
  spec=specgram(x=HU$T4,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)

  #library(caret)
  #calculate average band power
  T4Alpha=P[(2*8+1):(2*12),]
  T4Beta=P[(2*12+1):(2*30),]
  T4Theta=P[(2*4+1):(2*8),]
  T4Delta=P[(2*1+1):(2*4),]
  
  spec=specgram(x=HU$T5,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)
  #calculate average band power
  T5Alpha=P[(2*8+1):(2*12),]
  T5Beta=P[(2*12+1):(2*30),]
  T5Theta=P[(2*4+1):(2*8),]
  T5Delta=P[(2*1+1):(2*4),]
  
  spec=specgram(x=HU$P3,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)

  #calculate average band power
  P3Alpha=P[(2*8+1):(2*12),]
  P3Beta=P[(2*12+1):(2*30),]
  P3Theta=P[(2*4+1):(2*8),]
  P3Delta=P[(2*1+1):(2*4),]
  
  spec=specgram(x=HU$PZ,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)

  #calculate average band power
  PZAlpha=P[(2*8+1):(2*12),]
  PZBeta=P[(2*12+1):(2*30),]
  PZTheta=P[(2*4+1):(2*8),]
  PZDelta=P[(2*1+1):(2*4),]
  
  spec=specgram(x=HU$P4,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)

  #calculate average band power
  P4Alpha=P[(2*8+1):(2*12),]
  P4Beta=P[(2*12+1):(2*30),]
  P4Theta=P[(2*4+1):(2*8),]
  P4Delta=P[(2*1+1):(2*4),]
  
  spec=specgram(x=HU$T6,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)

  #calculate average band power
  T6Alpha=P[(2*8+1):(2*12),]
  T6Beta=P[(2*12+1):(2*30),]
  T6Theta=P[(2*4+1):(2*8),]
  T6Delta=P[(2*1+1):(2*4),]
  
  spec=specgram(x=HU$O1,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)

  #calculate average band power
  O1Alpha=P[(2*8+1):(2*12),]
  O1Beta=P[(2*12+1):(2*30),]
  O1Theta=P[(2*4+1):(2*8),]
  O1Delta=P[(2*1+1):(2*4),]
  
  spec=specgram(x=HU$O2,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)

  #calculate average band power
  O2Alpha=P[(2*8+1):(2*12),]
  O2Beta=P[(2*12+1):(2*30),]
  O2Theta=P[(2*4+1):(2*8),]
  O2Delta=P[(2*1+1):(2*4),]
  
  spec=specgram(x=HU$A1,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)

  #calculate average band power
  A1Alpha=P[(2*8+1):(2*12),]
  A1Beta=P[(2*12+1):(2*30),]
  A1Theta=P[(2*4+1):(2*8),]
  A1Delta=P[(2*1+1):(2*4),]
  
  spec=specgram(x=HU$A2,Fs=256,n=512)
  #window=2sec, overlap=half window
  P=abs(spec$S)

  #calculate average band power
  A2Alpha=P[(2*8+1):(2*12),]
  A2Beta=P[(2*12+1):(2*30),]
  A2Theta=P[(2*4+1):(2*8),]
  A2Delta=P[(2*1+1):(2*4),]
  
  #one might find an interative way
  
  #interpolation of the HR
  x=seq(length(HU$Heart.Rate))
  df=data.frame(time=seq(240))
  HR=approx(x,HU$Heart.Rate,n=480)$y
  HR=HR[-1]
  HR=HR[-length(HR)]
  x=seq(length(HR))
  df$HR=approx(x,HR,n=240)$y
  
  df$FP1AlphaMean=colMeans(FP1Alpha)
  df$FP1BetaMean=colMeans(FP1Beta)
  df$FP1ThetaMean=colMeans(FP1Theta)
  df$FP1DeltaMean=colMeans(FP1Delta)
  
  df$FP2AlphaMean=colMeans(FP2Alpha)
  df$FP2BetaMean=colMeans(FP2Beta)
  df$FP2ThetaMean=colMeans(FP2Theta)
  df$FP2DeltaMean=colMeans(FP2Delta)
  
  
  df$F7AlphaMean=colMeans(F7Alpha)
  df$F7BetaMean=colMeans(F7Beta)
  df$F7ThetaMean=colMeans(F7Theta)
  df$F7DeltaMean=colMeans(F7Delta)
  
  df$F3AlphaMean=colMeans(F3Alpha)
  df$F3BetaMean=colMeans(F3Beta)
  df$F3ThetaMean=colMeans(F3Theta)
  df$F3DeltaMean=colMeans(F3Delta)
  
  df$FzAlphaMean=colMeans(FzAlpha)
  df$FzBetaMean=colMeans(FzBeta)
  df$FzThetaMean=colMeans(FzTheta)
  df$FzDeltaMean=colMeans(FzDelta)
  
  df$F4AlphaMean=colMeans(F4Alpha)
  df$F4BetaMean=colMeans(F4Beta)
  df$F4ThetaMean=colMeans(F4Theta)
  df$F4DeltaMean=colMeans(F4Delta)
  
  df$F8AlphaMean=colMeans(F8Alpha)
  df$F8BetaMean=colMeans(F8Beta)
  df$F8ThetaMean=colMeans(F8Theta)
  df$F8DeltaMean=colMeans(F8Delta)
  
  df$T3AlphaMean=colMeans(T3Alpha)
  df$T3BetaMean=colMeans(T3Beta)
  df$T3ThetaMean=colMeans(T3Theta)
  df$T3DeltaMean=colMeans(T3Delta)
  
  df$C3AlphaMean=colMeans(C3Alpha)
  df$C3BetaMean=colMeans(C3Beta)
  df$C3ThetaMean=colMeans(C3Theta)
  df$C3DeltaMean=colMeans(C3Delta)
  
  df$CzAlphaMean=colMeans(CzAlpha)
  df$CzBetaMean=colMeans(CzBeta)
  df$CzThetaMean=colMeans(CzTheta)
  df$CzDeltaMean=colMeans(CzDelta)
  
  df$C4AlphaMean=colMeans(C4Alpha)
  df$C4BetaMean=colMeans(C4Beta)
  df$C4ThetaMean=colMeans(C4Theta)
  df$C4DeltaMean=colMeans(C4Delta)
  
  df$T4AlphaMean=colMeans(T4Alpha)
  df$T4BetaMean=colMeans(T4Beta)
  df$T4ThetaMean=colMeans(T4Theta)
  df$T4DeltaMean=colMeans(T4Delta)
  
  df$T5AlphaMean=colMeans(T5Alpha)
  df$T5BetaMean=colMeans(T5Beta)
  df$T5ThetaMean=colMeans(T5Theta)
  df$T5DeltaMean=colMeans(T5Delta)
  
  
  df$P3AlphaMean=colMeans(P3Alpha)
  df$P3BetaMean=colMeans(P3Beta)
  df$P3ThetaMean=colMeans(P3Theta)
  df$P3DeltaMean=colMeans(P3Delta)
  
  df$PZAlphaMean=colMeans(PZAlpha)
  df$PZBetaMean=colMeans(PZBeta)
  df$PZThetaMean=colMeans(PZTheta)
  df$PZDeltaMean=colMeans(PZDelta)
  
  df$P4AlphaMean=colMeans(P4Alpha)
  df$P4BetaMean=colMeans(P4Beta)
  df$P4ThetaMean=colMeans(P4Theta)
  df$P4DeltaMean=colMeans(P4Delta)
  
  df$T6AlphaMean=colMeans(T6Alpha)
  df$T6BetaMean=colMeans(T6Beta)
  df$T6ThetaMean=colMeans(T6Theta)
  df$T6DeltaMean=colMeans(T6Delta)
  
  df$O1AlphaMean=colMeans(O1Alpha)
  df$O1BetaMean=colMeans(O1Beta)
  df$O1ThetaMean=colMeans(O1Theta)
  df$O1DeltaMean=colMeans(O1Delta)
  
  df$O2AlphaMean=colMeans(O2Alpha)
  df$O2BetaMean=colMeans(O2Beta)
  df$O2ThetaMean=colMeans(O2Theta)
  df$O2DeltaMean=colMeans(O2Delta)
  
  df$A1AlphaMean=colMeans(A1Alpha)
  df$A1BetaMean=colMeans(A1Beta)
  df$A1ThetaMean=colMeans(A1Theta)
  df$A1DeltaMean=colMeans(A1Delta)
  
  df$A2AlphaMean=colMeans(A2Alpha)
  df$A2BetaMean=colMeans(A2Beta)
  df$A2ThetaMean=colMeans(A2Theta)
  df$A2DeltaMean=colMeans(A2Delta)
  
  #medi=1 story=0
  df$MediOrStory=rep(1,240)
  
  #+respiration +HRV(see later) +skinc + skint
  
  #interpolation of the SC
  x=seq(length(HU$SC))
  #df=data.frame(time=seq(450))
  SC=approx(x,HU$SC,n=480)$y
  SC=SC[-1]
  SC=SC[-length(SC)]
  x=seq(length(SC))
  df$SC=approx(x,SC,n=240)$y
  
  #interpolation of the St
  x=seq(length(HU$Temp))
  #df=data.frame(time=seq(450))
  ST=approx(x,HU$Temp,n=480)$y
  ST=ST[-1]
  ST=ST[-length(ST)]
  x=seq(length(ST))
  df$ST=approx(x,ST,n=240)$y
  
  #interpolation of the Respiration
  x=seq(length(HU$Respiration.Rate))
  #df=data.frame(time=seq(450))
  Resp=approx(x,HU$Respiration.Rate,n=480)$y
  Resp=Resp[-1]
  Resp=Resp[-length(Resp)]
  x=seq(length(Resp))
  df$Resp=approx(x,Resp,n=240)$y
 
  df$MediOrStory<-factor(ifelse(df$MediOrStory%in%c('0'),'0','1'))
  
  library(psych)
  #x=c(3,2,4)
  #mssd(x) is to calculate de RMSSD
  HRV=c(0)
  for (i in 2:length(df$HR) ){
    HRuse=df$HR
    HRV=c(HRV,mssd(HRuse[1:i]))
  }
  #notice the first HRV is zero
  df$HRV=HRV
  
  #statistic part
  setwd("E:/important file/CENTRALE/BCI ET EEG/Rprocess/data/hugues_auto")
  #export training matrix
  export_df_name=paste(MdtFilename,"5min_df.csv")
  write.csv(df, file = export_df_name,row.names = FALSE)
  
  return(df)
}

#df_test=df_generation(MdtFilename)
#HU=df_generation("MOHUstoryBioRes","MOHUmeditationBioRes")
