#objective=construct a matrix df
#process data_wangmin
#read story data
rm(list = objects())
setwd("E:/important file/CENTRALE/BCI ET EEG/Rprocess/data")
wang=read.table("M.WangStoryHrateFP1.txt",header = T,sep = ",",dec = ".",skip = 11,fill = T)
library(signal)
library(oce)
spec=specgram(x=wang$FP1,Fs=256,n=512)
#window=2sec, overlap=half window
P=abs(spec$S)
t=spec$t
imagep(x = t,
       y = spec$f,
       z = t(P),
       col = oce.colorsViridis,
       ylab = 'Frequency [Hz]',
       xlab = 'Time [s]',
       drawPalette = T,
       decimate = F)
#install.packages("caret")
#library(caret)
#calculate average band power
FP1Alpha=P[(2*8+1):(2*12),]
FP1Beta=P[(2*12+1):(2*30),]
FP1Theta=P[(2*4+1):(2*8),]
FP1Delta=P[(2*1+1):(2*4),]


#interpolation of the HR
x=seq(length(wang$Heart.Rate))
df=data.frame(time=seq(460))
HR=approx(x,wang$Heart.Rate,n=920)$y
HR=HR[-1]
HR=HR[-length(HR)]
x=seq(length(HR))
df$HR=approx(x,HR,n=460)$y
#delete unrelevant variable
rm(HR)
rm(t)
rm(x)
df$FP1AlphaMean=colMeans(FP1Alpha)
df$FP1BetaMean=colMeans(FP1Beta)
df$FP1ThetaMean=colMeans(FP1Theta)
df$FP1DeltaMean=colMeans(FP1Delta)
#medi=1 story=0
df$MediOrStory=rep(0,460)

#+respiration +HRV +skinc + skint

#statistical analyse on df
#select 12 sec of df
df100to108=df[which(df[,'time']>99),]
df100to108=df100to108[which(df100to108[,'time']<112),]
library(GGally)
ggpairs(df100to108)

#read meditation data
wang=read.table("M.Wang_MeditationCutLast10secHrateFP1.txt",header = T,sep = ",",dec = ".",skip = 11,fill = T)
wang=wang[-seq(150001,154112),]
spec=specgram(x=wang$FP1,Fs=256,n=512)
#window=2sec, overlap=half window
P=abs(spec$S)
t=spec$t
imagep(x = t,
       y = spec$f,
       z = t(P),
       col = oce.colorsViridis,
       ylab = 'Frequency [Hz]',
       xlab = 'Time [s]',
       drawPalette = T,
       decimate = F)
FP1Alpha=P[(2*8+1):(2*12),]
FP1Beta=P[(2*12+1):(2*30),]
FP1Theta=P[(2*4+1):(2*8),]
FP1Delta=P[(2*1+1):(2*4),]

#interpolation of the HR
x=seq(length(wang$Heart.Rate))
df1=data.frame(time=seq(461,460+584))
HR=approx(x,wang$Heart.Rate,n=1168)$y
HR=HR[-1]
HR=HR[-length(HR)]
x=seq(length(HR))
df1$HR=approx(x,HR,n=584)$y
#delete unrelevant variable
rm(HR)
rm(t)
rm(x)
df1$FP1AlphaMean=colMeans(FP1Alpha)
df1$FP1BetaMean=colMeans(FP1Beta)
df1$FP1ThetaMean=colMeans(FP1Theta)
df1$FP1DeltaMean=colMeans(FP1Delta)
#medi=1 story=0
df1$MediOrStory=rep(1,584)

#merge two df
df=rbind(df,df1)

#build HRV biomarker
#definition  Root Mean Square of the Successive Differences (RMSSD) 
#please consult https://www.biopac.com/application/ecg-cardiology/advanced-feature/rmssd-for-hrv-analysis/#tabs
install.packages("psych")
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

#select 12 sec of df
df100to108=df[which(df[,'time']>200),]
df100to108=df100to108[which(df100to108[,'time']<220),]
library(GGally)
ggpairs(df100to108)



#too see the HR in two phase
lo=loess(df$HR~df$time)
plot(df$time,df$HR,type="l",ylim=c(70,110))
lines(predict(lo), col='red', lwd=2)

#too see alpha power in two phase
lo=loess(df$FP1AlphaMean~df$time)
plot(df$time,df$FP1AlphaMean,type="l",ylim=c(50,200))
lines(predict(lo), col='red', lwd=2)

#HRV plus interessant et voir la relation entre meditation et HR
