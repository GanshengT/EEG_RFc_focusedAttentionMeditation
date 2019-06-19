#objective=construct a matrix df
#process data_wangmin
#read story data
rm(list = objects())
setwd("E:/important file/CENTRALE/BCI ET EEG/Rprocess/data")
#delete [30] and "," at the end of this sentence, then dedlete <end of exported RAW data> at the end
HU=read.table("MOHUstoryBioRes.txt",header = T,sep = ",",dec = ".",skip = 11,fill = T)
#450 sec 
HU=HU[((nrow(HU)-452*256)/2+1):((nrow(HU)-452*256)/2+452*256),]
library(signal)
library(oce)
# apply on FP1
spec=specgram(x=HU$FP1,Fs=256,n=512)
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

# apply on FP2
spec=specgram(x=HU$FP2,Fs=256,n=512)
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
FP2Alpha=P[(2*8+1):(2*12),]
FP2Beta=P[(2*12+1):(2*30),]
FP2Theta=P[(2*4+1):(2*8),]
FP2Delta=P[(2*1+1):(2*4),]

# apply on F7
spec=specgram(x=HU$F7,Fs=256,n=512)
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
F7Alpha=P[(2*8+1):(2*12),]
F7Beta=P[(2*12+1):(2*30),]
F7Theta=P[(2*4+1):(2*8),]
F7Delta=P[(2*1+1):(2*4),]

# apply on F3
spec=specgram(x=HU$F3,Fs=256,n=512)
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
F3Alpha=P[(2*8+1):(2*12),]
F3Beta=P[(2*12+1):(2*30),]
F3Theta=P[(2*4+1):(2*8),]
F3Delta=P[(2*1+1):(2*4),]

# apply on Fz
spec=specgram(x=HU$Fz,Fs=256,n=512)
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
FzAlpha=P[(2*8+1):(2*12),]
FzBeta=P[(2*12+1):(2*30),]
FzTheta=P[(2*4+1):(2*8),]
FzDelta=P[(2*1+1):(2*4),]

# apply on F4
spec=specgram(x=HU$F4,Fs=256,n=512)
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
F4Alpha=P[(2*8+1):(2*12),]
F4Beta=P[(2*12+1):(2*30),]
F4Theta=P[(2*4+1):(2*8),]
F4Delta=P[(2*1+1):(2*4),]

# apply on F8
spec=specgram(x=HU$F8,Fs=256,n=512)
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
F8Alpha=P[(2*8+1):(2*12),]
F8Beta=P[(2*12+1):(2*30),]
F8Theta=P[(2*4+1):(2*8),]
F8Delta=P[(2*1+1):(2*4),]

# apply on T3
spec=specgram(x=HU$T3,Fs=256,n=512)
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
T3Alpha=P[(2*8+1):(2*12),]
T3Beta=P[(2*12+1):(2*30),]
T3Theta=P[(2*4+1):(2*8),]
T3Delta=P[(2*1+1):(2*4),]

# apply on C3
spec=specgram(x=HU$C3,Fs=256,n=512)
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

#library(caret)
#calculate average band power
C3Alpha=P[(2*8+1):(2*12),]
C3Beta=P[(2*12+1):(2*30),]
C3Theta=P[(2*4+1):(2*8),]
C3Delta=P[(2*1+1):(2*4),]

# apply on Cz,C4,T4,T5,P3,PZ,P4,T6,o1,o2,A1,A2
spec=specgram(x=HU$Cz,Fs=256,n=512)
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

#library(caret)
#calculate average band power
CzAlpha=P[(2*8+1):(2*12),]
CzBeta=P[(2*12+1):(2*30),]
CzTheta=P[(2*4+1):(2*8),]
CzDelta=P[(2*1+1):(2*4),]

spec=specgram(x=HU$C4,Fs=256,n=512)
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

#library(caret)
#calculate average band power
C4Alpha=P[(2*8+1):(2*12),]
C4Beta=P[(2*12+1):(2*30),]
C4Theta=P[(2*4+1):(2*8),]
C4Delta=P[(2*1+1):(2*4),]

spec=specgram(x=HU$T4,Fs=256,n=512)
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

#library(caret)
#calculate average band power
T4Alpha=P[(2*8+1):(2*12),]
T4Beta=P[(2*12+1):(2*30),]
T4Theta=P[(2*4+1):(2*8),]
T4Delta=P[(2*1+1):(2*4),]

spec=specgram(x=HU$T5,Fs=256,n=512)
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

#library(caret)
#calculate average band power
T5Alpha=P[(2*8+1):(2*12),]
T5Beta=P[(2*12+1):(2*30),]
T5Theta=P[(2*4+1):(2*8),]
T5Delta=P[(2*1+1):(2*4),]

spec=specgram(x=HU$P3,Fs=256,n=512)
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

#library(caret)
#calculate average band power
P3Alpha=P[(2*8+1):(2*12),]
P3Beta=P[(2*12+1):(2*30),]
P3Theta=P[(2*4+1):(2*8),]
P3Delta=P[(2*1+1):(2*4),]

spec=specgram(x=HU$PZ,Fs=256,n=512)
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

#library(caret)
#calculate average band power
PZAlpha=P[(2*8+1):(2*12),]
PZBeta=P[(2*12+1):(2*30),]
PZTheta=P[(2*4+1):(2*8),]
PZDelta=P[(2*1+1):(2*4),]

spec=specgram(x=HU$P4,Fs=256,n=512)
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

#library(caret)
#calculate average band power
P4Alpha=P[(2*8+1):(2*12),]
P4Beta=P[(2*12+1):(2*30),]
P4Theta=P[(2*4+1):(2*8),]
P4Delta=P[(2*1+1):(2*4),]

spec=specgram(x=HU$T6,Fs=256,n=512)
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

#library(caret)
#calculate average band power
T6Alpha=P[(2*8+1):(2*12),]
T6Beta=P[(2*12+1):(2*30),]
T6Theta=P[(2*4+1):(2*8),]
T6Delta=P[(2*1+1):(2*4),]

spec=specgram(x=HU$O1,Fs=256,n=512)
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

#library(caret)
#calculate average band power
O1Alpha=P[(2*8+1):(2*12),]
O1Beta=P[(2*12+1):(2*30),]
O1Theta=P[(2*4+1):(2*8),]
O1Delta=P[(2*1+1):(2*4),]

spec=specgram(x=HU$O2,Fs=256,n=512)
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

#library(caret)
#calculate average band power
O2Alpha=P[(2*8+1):(2*12),]
O2Beta=P[(2*12+1):(2*30),]
O2Theta=P[(2*4+1):(2*8),]
O2Delta=P[(2*1+1):(2*4),]

spec=specgram(x=HU$A1,Fs=256,n=512)
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

#library(caret)
#calculate average band power
A1Alpha=P[(2*8+1):(2*12),]
A1Beta=P[(2*12+1):(2*30),]
A1Theta=P[(2*4+1):(2*8),]
A1Delta=P[(2*1+1):(2*4),]

spec=specgram(x=HU$A2,Fs=256,n=512)
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

#library(caret)
#calculate average band power
A2Alpha=P[(2*8+1):(2*12),]
A2Beta=P[(2*12+1):(2*30),]
A2Theta=P[(2*4+1):(2*8),]
A2Delta=P[(2*1+1):(2*4),]

#one might find an interative way

#interpolation of the HR
x=seq(length(HU$Heart.Rate))
df=data.frame(time=seq(450))
HR=approx(x,HU$Heart.Rate,n=900)$y
HR=HR[-1]
HR=HR[-length(HR)]
x=seq(length(HR))
df$HR=approx(x,HR,n=450)$y

#delete unrelevant variable
rm(HR)
rm(t)
rm(x)
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
df$MediOrStory=rep(0,450)
rm(FP1Alpha,FP1Beta,FP1Delta,FP1Theta)
rm(FP2Alpha,FP2Beta,FP2Delta,FP2Theta)
rm(F7Alpha,F7Beta,F7Delta,F7Theta)
rm(F3Alpha,F3Beta,F3Delta,F3Theta)
rm(FzAlpha,FzBeta,FzDelta,FzTheta)
rm(F4Alpha,F4Beta,F4Delta,F4Theta)
rm(F8Alpha,F8Beta,F8Delta,F8Theta)
rm(T3Alpha,T3Beta,T3Delta,T3Theta)
rm(C3Alpha,C3Beta,C3Delta,C3Theta)
rm(CzAlpha,CzBeta,CzDelta,CzTheta)
rm(C4Alpha,C4Beta,C4Delta,C4Theta)
rm(T4Alpha,T4Beta,T4Delta,T4Theta)
rm(T5Alpha,T5Beta,T5Delta,T5Theta)
rm(P3Alpha,P3Beta,P3Delta,P3Theta)
rm(PZAlpha,PZBeta,PZDelta,PZTheta)
rm(P4Alpha,P4Beta,P4Delta,P4Theta)
rm(T6Alpha,T6Beta,T6Delta,T6Theta)
rm(O1Alpha,O1Beta,O1Delta,O1Theta)
rm(O2Alpha,O2Beta,O2Delta,O2Theta)
rm(A1Alpha,A1Beta,A1Delta,A1Theta)
rm(A2Alpha,A2Beta,A2Delta,A2Theta)


#+respiration +HRV(see later) +skinc + skint

#interpolation of the SC
x=seq(length(HU$SC))
#df=data.frame(time=seq(450))
SC=approx(x,HU$SC,n=900)$y
SC=SC[-1]
SC=SC[-length(SC)]
x=seq(length(SC))
df$SC=approx(x,SC,n=450)$y

#interpolation of the St
x=seq(length(HU$Temp))
#df=data.frame(time=seq(450))
ST=approx(x,HU$Temp,n=900)$y
ST=ST[-1]
ST=ST[-length(ST)]
x=seq(length(ST))
df$ST=approx(x,ST,n=450)$y

#interpolation of the Respiration
x=seq(length(HU$Respiration.Rate))
#df=data.frame(time=seq(450))
Resp=approx(x,HU$Respiration.Rate,n=900)$y
Resp=Resp[-1]
Resp=Resp[-length(Resp)]
x=seq(length(Resp))
df$Resp=approx(x,Resp,n=450)$y

rm(SC,ST,Resp)
rm(x)


#statistical analyse on df
#select 12 sec of df
#df100to108=df[which(df[,'time']>99),]
#df100to108=df100to108[which(df100to108[,'time']<112),]
#library(GGally)
#ggpairs(df100to108)

#read meditation data
#580 sec 
HUm=read.table("MOHUmeditationBioRes.txt",header = T,sep = ",",dec = ".",skip = 11,fill = T)
HUm=HUm[((nrow(HUm)-582*256)/2+1):((nrow(HUm)-582*256)/2+582*256),]

#onFP1
spec=specgram(x=HUm$FP1,Fs=256,n=512)
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

# apply on FP2
spec=specgram(x=HUm$FP2,Fs=256,n=512)
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
FP2Alpha=P[(2*8+1):(2*12),]
FP2Beta=P[(2*12+1):(2*30),]
FP2Theta=P[(2*4+1):(2*8),]
FP2Delta=P[(2*1+1):(2*4),]

# apply on F7
spec=specgram(x=HUm$F7,Fs=256,n=512)
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
F7Alpha=P[(2*8+1):(2*12),]
F7Beta=P[(2*12+1):(2*30),]
F7Theta=P[(2*4+1):(2*8),]
F7Delta=P[(2*1+1):(2*4),]

# apply on F3
spec=specgram(x=HUm$F3,Fs=256,n=512)
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
F3Alpha=P[(2*8+1):(2*12),]
F3Beta=P[(2*12+1):(2*30),]
F3Theta=P[(2*4+1):(2*8),]
F3Delta=P[(2*1+1):(2*4),]

# apply on Fz
spec=specgram(x=HUm$Fz,Fs=256,n=512)
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
FzAlpha=P[(2*8+1):(2*12),]
FzBeta=P[(2*12+1):(2*30),]
FzTheta=P[(2*4+1):(2*8),]
FzDelta=P[(2*1+1):(2*4),]

# apply on F4
spec=specgram(x=HUm$F4,Fs=256,n=512)
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
F4Alpha=P[(2*8+1):(2*12),]
F4Beta=P[(2*12+1):(2*30),]
F4Theta=P[(2*4+1):(2*8),]
F4Delta=P[(2*1+1):(2*4),]

# apply on F8
spec=specgram(x=HUm$F8,Fs=256,n=512)
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
#calculate average band power
F8Alpha=P[(2*8+1):(2*12),]
F8Beta=P[(2*12+1):(2*30),]
F8Theta=P[(2*4+1):(2*8),]
F8Delta=P[(2*1+1):(2*4),]

# apply on T3
spec=specgram(x=HUm$T3,Fs=256,n=512)
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
T3Alpha=P[(2*8+1):(2*12),]
T3Beta=P[(2*12+1):(2*30),]
T3Theta=P[(2*4+1):(2*8),]
T3Delta=P[(2*1+1):(2*4),]

# apply on C3
spec=specgram(x=HUm$C3,Fs=256,n=512)
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
C3Alpha=P[(2*8+1):(2*12),]
C3Beta=P[(2*12+1):(2*30),]
C3Theta=P[(2*4+1):(2*8),]
C3Delta=P[(2*1+1):(2*4),]


# apply on Cz,C4,T4,T5,P3,PZ,P4,T6,o1,o2,A1,A2
spec=specgram(x=HUm$Cz,Fs=256,n=512)
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

#library(caret)
#calculate average band power
CzAlpha=P[(2*8+1):(2*12),]
CzBeta=P[(2*12+1):(2*30),]
CzTheta=P[(2*4+1):(2*8),]
CzDelta=P[(2*1+1):(2*4),]

spec=specgram(x=HUm$C4,Fs=256,n=512)
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

#library(caret)
#calculate average band power
C4Alpha=P[(2*8+1):(2*12),]
C4Beta=P[(2*12+1):(2*30),]
C4Theta=P[(2*4+1):(2*8),]
C4Delta=P[(2*1+1):(2*4),]

spec=specgram(x=HUm$T4,Fs=256,n=512)
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

#library(caret)
#calculate average band power
T4Alpha=P[(2*8+1):(2*12),]
T4Beta=P[(2*12+1):(2*30),]
T4Theta=P[(2*4+1):(2*8),]
T4Delta=P[(2*1+1):(2*4),]

spec=specgram(x=HUm$T5,Fs=256,n=512)
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

#library(caret)
#calculate average band power
T5Alpha=P[(2*8+1):(2*12),]
T5Beta=P[(2*12+1):(2*30),]
T5Theta=P[(2*4+1):(2*8),]
T5Delta=P[(2*1+1):(2*4),]

spec=specgram(x=HUm$P3,Fs=256,n=512)
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

#library(caret)
#calculate average band power
P3Alpha=P[(2*8+1):(2*12),]
P3Beta=P[(2*12+1):(2*30),]
P3Theta=P[(2*4+1):(2*8),]
P3Delta=P[(2*1+1):(2*4),]

spec=specgram(x=HUm$PZ,Fs=256,n=512)
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

#library(caret)
#calculate average band power
PZAlpha=P[(2*8+1):(2*12),]
PZBeta=P[(2*12+1):(2*30),]
PZTheta=P[(2*4+1):(2*8),]
PZDelta=P[(2*1+1):(2*4),]

spec=specgram(x=HUm$P4,Fs=256,n=512)
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

#library(caret)
#calculate average band power
P4Alpha=P[(2*8+1):(2*12),]
P4Beta=P[(2*12+1):(2*30),]
P4Theta=P[(2*4+1):(2*8),]
P4Delta=P[(2*1+1):(2*4),]

spec=specgram(x=HUm$T6,Fs=256,n=512)
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

#library(caret)
#calculate average band power
T6Alpha=P[(2*8+1):(2*12),]
T6Beta=P[(2*12+1):(2*30),]
T6Theta=P[(2*4+1):(2*8),]
T6Delta=P[(2*1+1):(2*4),]

spec=specgram(x=HUm$O1,Fs=256,n=512)
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

#library(caret)
#calculate average band power
O1Alpha=P[(2*8+1):(2*12),]
O1Beta=P[(2*12+1):(2*30),]
O1Theta=P[(2*4+1):(2*8),]
O1Delta=P[(2*1+1):(2*4),]

spec=specgram(x=HUm$O2,Fs=256,n=512)
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

#library(caret)
#calculate average band power
O2Alpha=P[(2*8+1):(2*12),]
O2Beta=P[(2*12+1):(2*30),]
O2Theta=P[(2*4+1):(2*8),]
O2Delta=P[(2*1+1):(2*4),]

spec=specgram(x=HUm$A1,Fs=256,n=512)
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

#library(caret)
#calculate average band power
A1Alpha=P[(2*8+1):(2*12),]
A1Beta=P[(2*12+1):(2*30),]
A1Theta=P[(2*4+1):(2*8),]
A1Delta=P[(2*1+1):(2*4),]

spec=specgram(x=HUm$A2,Fs=256,n=512)
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

#library(caret)
#calculate average band power
A2Alpha=P[(2*8+1):(2*12),]
A2Beta=P[(2*12+1):(2*30),]
A2Theta=P[(2*4+1):(2*8),]
A2Delta=P[(2*1+1):(2*4),]




#interpolation of the HR
x=seq(length(HUm$Heart.Rate))
df1=data.frame(time=seq(461,460+580))
HR=approx(x,HUm$Heart.Rate,n=1160)$y
HR=HR[-1]
HR=HR[-length(HR)]
x=seq(length(HR))
df1$HR=approx(x,HR,n=580)$y

#interpolation of the SC
x=seq(length(HUm$SC))
#df=data.frame(time=seq(450))
SC=approx(x,HUm$SC,n=1160)$y
SC=SC[-1]
SC=SC[-length(SC)]
x=seq(length(SC))
df1$SC=approx(x,SC,n=580)$y

#interpolation of the St
x=seq(length(HUm$Temp))
#df=data.frame(time=seq(450))
ST=approx(x,HUm$Temp,n=1160)$y
ST=ST[-1]
ST=ST[-length(ST)]
x=seq(length(ST))
df1$ST=approx(x,ST,n=580)$y

#interpolation of the Respiration
x=seq(length(HUm$Respiration.Rate))
#df=data.frame(time=seq(450))
Resp=approx(x,HUm$Respiration.Rate,n=1160)$y
Resp=Resp[-1]
Resp=Resp[-length(Resp)]
x=seq(length(Resp))
df1$Resp=approx(x,Resp,n=580)$y

rm(SC,ST,Resp)

#delete unrelevant variable
rm(HR)
rm(t)
rm(x)
df1$FP1AlphaMean=colMeans(FP1Alpha)
df1$FP1BetaMean=colMeans(FP1Beta)
df1$FP1ThetaMean=colMeans(FP1Theta)
df1$FP1DeltaMean=colMeans(FP1Delta)

df1$FP2AlphaMean=colMeans(FP2Alpha)
df1$FP2BetaMean=colMeans(FP2Beta)
df1$FP2ThetaMean=colMeans(FP2Theta)
df1$FP2DeltaMean=colMeans(FP2Delta)

df1$F7AlphaMean=colMeans(F7Alpha)
df1$F7BetaMean=colMeans(F7Beta)
df1$F7ThetaMean=colMeans(F7Theta)
df1$F7DeltaMean=colMeans(F7Delta)

df1$F3AlphaMean=colMeans(F3Alpha)
df1$F3BetaMean=colMeans(F3Beta)
df1$F3ThetaMean=colMeans(F3Theta)
df1$F3DeltaMean=colMeans(F3Delta)

df1$FzAlphaMean=colMeans(FzAlpha)
df1$FzBetaMean=colMeans(FzBeta)
df1$FzThetaMean=colMeans(FzTheta)
df1$FzDeltaMean=colMeans(FzDelta)

df1$F4AlphaMean=colMeans(F4Alpha)
df1$F4BetaMean=colMeans(F4Beta)
df1$F4ThetaMean=colMeans(F4Theta)
df1$F4DeltaMean=colMeans(F4Delta)

df1$F8AlphaMean=colMeans(F8Alpha)
df1$F8BetaMean=colMeans(F8Beta)
df1$F8ThetaMean=colMeans(F8Theta)
df1$F8DeltaMean=colMeans(F8Delta)

df1$T3AlphaMean=colMeans(T3Alpha)
df1$T3BetaMean=colMeans(T3Beta)
df1$T3ThetaMean=colMeans(T3Theta)
df1$T3DeltaMean=colMeans(T3Delta)

df1$C3AlphaMean=colMeans(C3Alpha)
df1$C3BetaMean=colMeans(C3Beta)
df1$C3ThetaMean=colMeans(C3Theta)
df1$C3DeltaMean=colMeans(C3Delta)

df1$CzAlphaMean=colMeans(CzAlpha)
df1$CzBetaMean=colMeans(CzBeta)
df1$CzThetaMean=colMeans(CzTheta)
df1$CzDeltaMean=colMeans(CzDelta)

df1$C4AlphaMean=colMeans(C4Alpha)
df1$C4BetaMean=colMeans(C4Beta)
df1$C4ThetaMean=colMeans(C4Theta)
df1$C4DeltaMean=colMeans(C4Delta)

df1$T4AlphaMean=colMeans(T4Alpha)
df1$T4BetaMean=colMeans(T4Beta)
df1$T4ThetaMean=colMeans(T4Theta)
df1$T4DeltaMean=colMeans(T4Delta)

df1$T5AlphaMean=colMeans(T5Alpha)
df1$T5BetaMean=colMeans(T5Beta)
df1$T5ThetaMean=colMeans(T5Theta)
df1$T5DeltaMean=colMeans(T5Delta)


df1$P3AlphaMean=colMeans(P3Alpha)
df1$P3BetaMean=colMeans(P3Beta)
df1$P3ThetaMean=colMeans(P3Theta)
df1$P3DeltaMean=colMeans(P3Delta)

df1$PZAlphaMean=colMeans(PZAlpha)
df1$PZBetaMean=colMeans(PZBeta)
df1$PZThetaMean=colMeans(PZTheta)
df1$PZDeltaMean=colMeans(PZDelta)

df1$P4AlphaMean=colMeans(P4Alpha)
df1$P4BetaMean=colMeans(P4Beta)
df1$P4ThetaMean=colMeans(P4Theta)
df1$P4DeltaMean=colMeans(P4Delta)

df1$T6AlphaMean=colMeans(T6Alpha)
df1$T6BetaMean=colMeans(T6Beta)
df1$T6ThetaMean=colMeans(T6Theta)
df1$T6DeltaMean=colMeans(T6Delta)

df1$O1AlphaMean=colMeans(O1Alpha)
df1$O1BetaMean=colMeans(O1Beta)
df1$O1ThetaMean=colMeans(O1Theta)
df1$O1DeltaMean=colMeans(O1Delta)

df1$O2AlphaMean=colMeans(O2Alpha)
df1$O2BetaMean=colMeans(O2Beta)
df1$O2ThetaMean=colMeans(O2Theta)
df1$O2DeltaMean=colMeans(O2Delta)

df1$A1AlphaMean=colMeans(A1Alpha)
df1$A1BetaMean=colMeans(A1Beta)
df1$A1ThetaMean=colMeans(A1Theta)
df1$A1DeltaMean=colMeans(A1Delta)

df1$A2AlphaMean=colMeans(A2Alpha)
df1$A2BetaMean=colMeans(A2Beta)
df1$A2ThetaMean=colMeans(A2Theta)
df1$A2DeltaMean=colMeans(A2Delta)

rm(FP1Alpha,FP1Beta,FP1Delta,FP1Theta)
rm(FP2Alpha,FP2Beta,FP2Delta,FP2Theta)
rm(F7Alpha,F7Beta,F7Delta,F7Theta)
rm(F3Alpha,F3Beta,F3Delta,F3Theta)
rm(FzAlpha,FzBeta,FzDelta,FzTheta)
rm(F4Alpha,F4Beta,F4Delta,F4Theta)
rm(F8Alpha,F8Beta,F8Delta,F8Theta)
rm(T3Alpha,T3Beta,T3Delta,T3Theta)
rm(C3Alpha,C3Beta,C3Delta,C3Theta)
rm(CzAlpha,CzBeta,CzDelta,CzTheta)
rm(C4Alpha,C4Beta,C4Delta,C4Theta)
rm(T4Alpha,T4Beta,T4Delta,T4Theta)
rm(T5Alpha,T5Beta,T5Delta,T5Theta)
rm(P3Alpha,P3Beta,P3Delta,P3Theta)
rm(PZAlpha,PZBeta,PZDelta,PZTheta)
rm(P4Alpha,P4Beta,P4Delta,P4Theta)
rm(T6Alpha,T6Beta,T6Delta,T6Theta)
rm(O1Alpha,O1Beta,O1Delta,O1Theta)
rm(O2Alpha,O2Beta,O2Delta,O2Theta)
rm(A1Alpha,A1Beta,A1Delta,A1Theta)
rm(A2Alpha,A2Beta,A2Delta,A2Theta)


#medi=1 story=0
df1$MediOrStory=rep(1,580)

#merge two df
df=rbind(df,df1)
rm(df1)
rm(HUm)
rm(HU)
#change to factor
df$MediOrStory<-factor(ifelse(df$MediOrStory%in%c('0'),'0','1'))

#build HRV biomarker
#definition  Root Mean Square of the Successive Differences (RMSSD) 
#please consult https://www.biopac.com/application/ecg-cardiology/advanced-feature/rmssd-for-hrv-analysis/#tabs
#install.packages("psych")
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
write.csv(df, file = "hugues_df.csv")

#selected df 600 sec to 900 sec and change to 120 and 360 to represente the story phase
dfslted=df[which(df[,'time']>120),]
dfslted=dfslted[which(dfslted[,'time']<360),]
library(GGally)
#ggpairs(dfslted,columns=c("HR", "SC", "HRV", "Resp","ST","FP1ThetaMean"))
pdf(file = "pairs_betweenBioMAndFP1ThetaStory.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HR", "SC", "HRV", "Resp","ST","FP1ThetaMean")])
dev.off()
pdf(file = "pairs_HRVFP1Story.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","FP1ThetaMean","FP1AlphaMean","FP1BetaMean","FP1DeltaMean")])
dev.off()
pdf(file = "pairs_HRVFP2Story.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","FP2ThetaMean","FP2AlphaMean","FP2BetaMean","FP2DeltaMean")])
dev.off()
pdf(file = "pairs_HRVF7Story.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","F7ThetaMean","F7AlphaMean","F7BetaMean","F7DeltaMean")])
dev.off()
pdf(file = "pairs_HRVF3Story.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","F3ThetaMean","F3AlphaMean","F3BetaMean","F3DeltaMean")])
dev.off()
pdf(file = "pairs_HRVFzStory.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","FzThetaMean","FzAlphaMean","FzBetaMean","FzDeltaMean")])
dev.off()
pdf(file = "pairs_HRVF4Story.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","F4ThetaMean","F4AlphaMean","F4BetaMean","F4DeltaMean")])
dev.off()
pdf(file = "pairs_HRVF8Story.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","F8ThetaMean","F8AlphaMean","F8BetaMean","F8DeltaMean")])
dev.off()
pdf(file = "pairs_HRVT3Story.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","T3ThetaMean","T3AlphaMean","T3BetaMean","T3DeltaMean")])
dev.off()
pdf(file = "pairs_HRVC3Story.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","C3ThetaMean","C3AlphaMean","C3BetaMean","C3DeltaMean")])
dev.off()
pdf(file = "pairs_HRVCzStory.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","CzThetaMean","CzAlphaMean","CzBetaMean","CzDeltaMean")])
dev.off()
pdf(file = "pairs_HRVC4Story.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","C4ThetaMean","C4AlphaMean","C4BetaMean","C4DeltaMean")])
dev.off()
pdf(file = "pairs_HRVT4Story.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","T4ThetaMean","T4AlphaMean","T4BetaMean","T4DeltaMean")])
dev.off()
pdf(file = "pairs_HRVT5Story.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","T5ThetaMean","T5AlphaMean","T5BetaMean","T5DeltaMean")])
dev.off()
pdf(file = "pairs_HRVP3Story.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","P3ThetaMean","P3AlphaMean","P3BetaMean","P3DeltaMean")])
dev.off()
pdf(file = "pairs_HRVP4Story.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","P4ThetaMean","P4AlphaMean","P4BetaMean","P4DeltaMean")])
dev.off()
pdf(file = "pairs_HRVT6Story.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","T6ThetaMean","T6AlphaMean","T6BetaMean","T6DeltaMean")])
dev.off()
pdf(file = "pairs_HRVO1Story.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","O1ThetaMean","O1AlphaMean","O1BetaMean","O1DeltaMean")])
dev.off()
pdf(file = "pairs_HRVO2Story.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","O2ThetaMean","O2AlphaMean","O2BetaMean","O2DeltaMean")])
dev.off()
pdf(file = "pairs_HRV1Story.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","A1ThetaMean","A1AlphaMean","A1BetaMean","A1DeltaMean")])
dev.off()
pdf(file = "pairs_HRVA2Story.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","A2ThetaMean","A2AlphaMean","A2BetaMean","A2DeltaMean")])
dev.off()
pdf(file = "pairs_HRVPZStory.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","PZThetaMean","PZAlphaMean","PZBetaMean","PZDeltaMean")])
dev.off()

#alpha,theta,beta,delta
pdf(file = "pairs_AlphaPrePluaFrontalStory.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("FP1AlphaMean","FP2AlphaMean","F7AlphaMean","F3AlphaMean","FzAlphaMean","F4AlphaMean","F8AlphaMean")])
dev.off()
#conclusion alpha is highly correlated in frontal
pdf(file = "pairs_AlphaCentralTemporalStory.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("T3AlphaMean","T4AlphaMean","T5AlphaMean","T6AlphaMean","CzAlphaMean","C3AlphaMean","C4AlphaMean")])
dev.off()
pdf(file = "pairs_AlphaParietalOccipitalStory.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("P3AlphaMean","P4AlphaMean","PZAlphaMean","O1AlphaMean","O2AlphaMean")])
dev.off()

#Beta
pdf(file = "pairs_BetaPrePluaFrontalStory.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("FP1BetaMean","FP2BetaMean","F7BetaMean","F3BetaMean","FzBetaMean","F4BetaMean","F8BetaMean")])
dev.off()
#conclusion beta
pdf(file = "pairs_BetaCentralTemporalStory.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("T3BetaMean","T4BetaMean","T5BetaMean","T6BetaMean","CzBetaMean","C3BetaMean","C4BetaMean")])
dev.off()
pdf(file = "pairs_BetaParietalOccipitalStory.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("P3BetaMean","P4BetaMean","PZBetaMean","O1BetaMean","O2BetaMean")])
dev.off()

#Theta
pdf(file = "pairs_ThetaPrePluaFrontalStory.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("FP1ThetaMean","FP2ThetaMean","F7ThetaMean","F3ThetaMean","FzThetaMean","F4ThetaMean","F8ThetaMean")])
dev.off()
#conclusion beta
pdf(file = "pairs_ThetaCentralTemporalStory.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("T3ThetaMean","T4ThetaMean","T5ThetaMean","T6ThetaMean","CzThetaMean","C3ThetaMean","C4ThetaMean")])
dev.off()
pdf(file = "pairs_ThetaParietalOccipitalStory.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("P3ThetaMean","P4ThetaMean","PZThetaMean","O1ThetaMean","O2ThetaMean")])
dev.off()

#Delta
pdf(file = "pairs_DeltaPrePluaFrontalStory.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("FP1DeltaMean","FP2DeltaMean","F7DeltaMean","F3DeltaMean","FzDeltaMean","F4DeltaMean","F8DeltaMean")])
dev.off()
#conclusion beta
pdf(file = "pairs_DeltaCentralTemporalStory.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("T3DeltaMean","T4DeltaMean","T5DeltaMean","T6DeltaMean","CzDeltaMean","C3DeltaMean","C4DeltaMean")])
dev.off()
pdf(file = "pairs_DeltaParietalOccipitalStory.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("P3DeltaMean","P4DeltaMean","PZDeltaMean","O1DeltaMean","O2DeltaMean")])
dev.off()
#attention to cz and pZ

#meditation
dfslted=df[which(df[,'time']>600),]
dfslted=dfslted[which(dfslted[,'time']<900),]
library(GGally)
#ggpairs(dfslted,columns=c("HR", "SC", "HRV", "Resp","ST","FP1ThetaMean"))
pdf(file = "pairs_betweenBioMAndFP1ThetaMdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HR", "SC", "HRV", "Resp","ST","FP1ThetaMean")])
dev.off()
pdf(file = "pairs_HRVFP1Mdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","FP1ThetaMean","FP1AlphaMean","FP1BetaMean","FP1DeltaMean")])
dev.off()
pdf(file = "pairs_HRVFP2Mdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","FP2ThetaMean","FP2AlphaMean","FP2BetaMean","FP2DeltaMean")])
dev.off()
pdf(file = "pairs_HRVF7Mdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","F7ThetaMean","F7AlphaMean","F7BetaMean","F7DeltaMean")])
dev.off()
pdf(file = "pairs_HRVF3Mdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","F3ThetaMean","F3AlphaMean","F3BetaMean","F3DeltaMean")])
dev.off()
pdf(file = "pairs_HRVFzMdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","FzThetaMean","FzAlphaMean","FzBetaMean","FzDeltaMean")])
dev.off()
pdf(file = "pairs_HRVF4Mdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","F4ThetaMean","F4AlphaMean","F4BetaMean","F4DeltaMean")])
dev.off()
pdf(file = "pairs_HRVF8Mdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","F8ThetaMean","F8AlphaMean","F8BetaMean","F8DeltaMean")])
dev.off()
pdf(file = "pairs_HRVT3Mdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","T3ThetaMean","T3AlphaMean","T3BetaMean","T3DeltaMean")])
dev.off()
pdf(file = "pairs_HRVC3Mdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","C3ThetaMean","C3AlphaMean","C3BetaMean","C3DeltaMean")])
dev.off()
pdf(file = "pairs_HRVCzMdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","CzThetaMean","CzAlphaMean","CzBetaMean","CzDeltaMean")])
dev.off()
pdf(file = "pairs_HRVC4Mdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","C4ThetaMean","C4AlphaMean","C4BetaMean","C4DeltaMean")])
dev.off()
pdf(file = "pairs_HRVT4Mdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","T4ThetaMean","T4AlphaMean","T4BetaMean","T4DeltaMean")])
dev.off()
pdf(file = "pairs_HRVT5Mdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","T5ThetaMean","T5AlphaMean","T5BetaMean","T5DeltaMean")])
dev.off()
pdf(file = "pairs_HRVP3Mdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","P3ThetaMean","P3AlphaMean","P3BetaMean","P3DeltaMean")])
dev.off()
pdf(file = "pairs_HRVP4Mdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","P4ThetaMean","P4AlphaMean","P4BetaMean","P4DeltaMean")])
dev.off()
pdf(file = "pairs_HRVT6Mdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","T6ThetaMean","T6AlphaMean","T6BetaMean","T6DeltaMean")])
dev.off()
pdf(file = "pairs_HRVO1Mdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","O1ThetaMean","O1AlphaMean","O1BetaMean","O1DeltaMean")])
dev.off()
pdf(file = "pairs_HRVO2Mdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","O2ThetaMean","O2AlphaMean","O2BetaMean","O2DeltaMean")])
dev.off()
pdf(file = "pairs_HRV1Mdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","A1ThetaMean","A1AlphaMean","A1BetaMean","A1DeltaMean")])
dev.off()
pdf(file = "pairs_HRVA2Mdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","A2ThetaMean","A2AlphaMean","A2BetaMean","A2DeltaMean")])
dev.off()
pdf(file = "pairs_HRVPZMdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("HRV","PZThetaMean","PZAlphaMean","PZBetaMean","PZDeltaMean")])
dev.off()

#alpha,theta,beta,delta
pdf(file = "pairs_AlphaPrePluaFrontalMdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("FP1AlphaMean","FP2AlphaMean","F7AlphaMean","F3AlphaMean","FzAlphaMean","F4AlphaMean","F8AlphaMean")])
dev.off()
#conclusion alpha is highly correlated in frontal
pdf(file = "pairs_AlphaCentralTemporalMdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("T3AlphaMean","T4AlphaMean","T5AlphaMean","T6AlphaMean","CzAlphaMean","C3AlphaMean","C4AlphaMean")])
dev.off()
pdf(file = "pairs_AlphaParietalOccipitalMdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("P3AlphaMean","P4AlphaMean","PZAlphaMean","O1AlphaMean","O2AlphaMean")])
dev.off()

#Beta
pdf(file = "pairs_BetaPrePluaFrontalMdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("FP1BetaMean","FP2BetaMean","F7BetaMean","F3BetaMean","FzBetaMean","F4BetaMean","F8BetaMean")])
dev.off()
#conclusion beta
pdf(file = "pairs_BetaCentralTemporalMdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("T3BetaMean","T4BetaMean","T5BetaMean","T6BetaMean","CzBetaMean","C3BetaMean","C4BetaMean")])
dev.off()
pdf(file = "pairs_BetaParietalOccipitalMdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("P3BetaMean","P4BetaMean","PZBetaMean","O1BetaMean","O2BetaMean")])
dev.off()

#Theta
pdf(file = "pairs_ThetaPrePluaFrontalMdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("FP1ThetaMean","FP2ThetaMean","F7ThetaMean","F3ThetaMean","FzThetaMean","F4ThetaMean","F8ThetaMean")])
dev.off()
#conclusion beta
pdf(file = "pairs_ThetaCentralTemporalMdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("T3ThetaMean","T4ThetaMean","T5ThetaMean","T6ThetaMean","CzThetaMean","C3ThetaMean","C4ThetaMean")])
dev.off()
pdf(file = "pairs_ThetaParietalOccipitalMdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("P3ThetaMean","P4ThetaMean","PZThetaMean","O1ThetaMean","O2ThetaMean")])
dev.off()

#Delta
pdf(file = "pairs_DeltaPrePluaFrontalMdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("FP1DeltaMean","FP2DeltaMean","F7DeltaMean","F3DeltaMean","FzDeltaMean","F4DeltaMean","F8DeltaMean")])
dev.off()
#conclusion beta
pdf(file = "pairs_DeltaCentralTemporalMdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("T3DeltaMean","T4DeltaMean","T5DeltaMean","T6DeltaMean","CzDeltaMean","C3DeltaMean","C4DeltaMean")])
dev.off()
pdf(file = "pairs_DeltaParietalOccipitalMdt.pdf", width = 9, height = 9, pointsize = 10)
pairs(dfslted[c("P3DeltaMean","P4DeltaMean","PZDeltaMean","O1DeltaMean","O2DeltaMean")])
dev.off()
#attention to cz and pZ




#library(corrplot)
#M=cor(dfslted[c("HR", "SC", "HRV", "Resp","ST","FP1ThetaMean","FP1AlphaMean","FP1DeltaMean","FP1BetaMean")])
#corrplot(M,method = "circle")

#too see the HR in two phase moving average combined with regression
#http://perso.ens-lyon.fr/lise.vaudor/regression-loess/
#go to this site to see, span is the window next to x locally
pdf(file = "HRAgainstTime.pdf", width = 9, height = 9, pointsize = 10)
plot(df$time,df$HR,type="l",ylim=c(60,105),main="Heart Rate according to time")
#install.packages("pracma")
library(pracma)
Y=movavg(df$HR,20,type="s")
lines(Y,col="red")
#Y1=movavg(df$HR,30,type="s")
#lines(Y1,col="blue")

#lo=loess(df$HR~df$time,span = 0.1)
#lines(predict(lo), col='blue', lwd=2)
abline(v=450, col="Deeppink",lty=2)
legend("topright", legend=c("moving average window=20 data points", "original data","transition of phase"),
       col=c("red", "black","Deeppink"), lty=c(1,1,2), cex=0.8)
dev.off()

#watching HRV
pdf(file = "HRVAgainstTime.pdf", width = 9, height = 9, pointsize = 10)
Y=movavg(df$HRV[3:length(df$HRV)],20,type="s")
#plot(df$time[3:length(df$time)],Y,type="l",ylim=c(100,500),main="Heart Rate variability according to time")
#plot original data
plot(df$time[3:length(df$time)],df$HRV[3:length(df$HRV)],type="l",ylab="HRV",xlab="time", ylim=c(100,500), main="Heart Rate variability according to time")
#lo=loess(df$HRV~df$time,span = 0.5)
#lines(predict(lo), col='red', lwd=2)
#lo=loess(df$HRV~df$time,span = 0.3)
#lines(predict(lo), col='blue', lwd=2)
abline(v=450, col="Deeppink",lty=2)
legend("right", legend=c("transition of phase","HRV raw data"),
       col=c("Deeppink","black"), lty=c(2,1), cex=0.8)
dev.off()

#respiration
pdf(file = "RespAgainstTime.pdf", width = 9, height = 9, pointsize = 10)
plot(df$time,df$Resp,type="l",ylim=c(0,40),ylab="respiration(BPM)",xlab="time",main="respiration(BPM) according to time")
#lo=loess(df$Resp~df$time,span = 0.5)
#lines(predict(lo), col='red', lwd=2)
#lo=loess(df$Resp~df$time,span = 0.3)
#lines(predict(lo), col='blue', lwd=2)
abline(v=450, col="Deeppink",lty=2)
legend("right", legend=c("transition of phase","respiration raw data"),
       col=c("Deeppink","black"), lty=c(2,1), cex=0.8)
#legend("right", legend=c("Line span=0.5", "Line span=0.3","transition of phase"),
#       col=c("red", "blue","Deeppink"), lty=c(1,1,2), cex=0.8)
dev.off()

#skin conductance
pdf(file = "SCAgainstTime.pdf", width = 9, height = 9, pointsize = 10)
plot(df$time,df$SC,type="l",ylim=c(1.4,2.4),xlab="time",ylab="skin conductance (micro Siemens)",main="Skin conductance according to time")
#lo=loess(df$SC~df$time,span = 0.5)
#lines(predict(lo), col='red', lwd=2)
#lo=loess(df$SC~df$time,span = 0.3)
#lines(predict(lo), col='blue', lwd=2)
abline(v=450, col="Deeppink",lty=2)
legend("topright", legend=c("transition of phase","skin conductance raw data"),
       col=c("Deeppink","black"), lty=c(2,1), cex=0.8)
dev.off()

#skin temp
pdf(file = "HRAgainstTime.pdf", width = 9, height = 9, pointsize = 10)
plot(df$time,df$ST,type="l",ylim=c(18.4,19),ylab="skin temperature",xlab="time",main="Skin temperature according to time")
#lo=loess(df$ST~df$time,span = 0.5)
#lines(predict(lo), col='red', lwd=2)
#lo=loess(df$ST~df$time,span = 0.3)
#lines(predict(lo), col='blue', lwd=2)
abline(v=450, col="Deeppink",lty=2)
legend("topright", legend=c("transition of phase","skin temperature raw data"),
       col=c("Deeppink","black"), lty=c(2,1), cex=0.8)
dev.off()


#too see alpha power in two phase
#for loop
#library(tidyverse)
#define which element that I want to plot


#define universal ybounds func
ybounds <- function(y){  # y is the response variable in the dataframe
  bounds = quantile(y, probs=c(0.05, 0.95), type=3, names=FALSE)
  return(bounds + c(-1,1) * 0.2 * (bounds[2]-bounds[1]) )
}
#select eeg signals
df_eeg=subset(df,select=-c(time,HR,HRV,Resp,ST,SC,MediOrStory))
dev.off()
for (col in 1:(ncol(df_eeg))){
  pdf(file = paste(colnames(df_eeg[col]),"average power against time.pdf"), width = 9, height = 9, pointsize = 10)
  par(mfrow=c(1,2))
  plot(df$time,df_eeg[,col],type="l",ylim=ybounds(df_eeg[,col]),ylab= paste(colnames(df_eeg[col]),"average power(micro volt)"),main=paste(colnames(df_eeg[col])," power against time(raw data)"))
  Y=movavg(df_eeg[,col],20,type="s")
  plot(df$time,Y,type="l",col="red",xlab="time",ylab= paste(colnames(df_eeg[col]),"average power(micro volt)"),ylim=ybounds(df_eeg[,col]),main=paste(colnames(df_eeg[col])," power according to time"))
  abline(v=450, col="Deeppink",lty=2)
  legend("topright", legend=c("transition of phase","moving average line"),
         col=c("Deeppink","red"), lty=c(2,1), cex=0.8)
  dev.off()
  }

#plot(df$time,df_eeg[,1],type="l",ylim=ybounds(df_eeg[1]),main=paste(df_eeg[1]," power according to time"))
#pdf(file="test", width = 9, height = 9, pointsize = 10)
#par(mfrow=c(1,2))
#plot(df$time,df$FP1BetMean,ylim=ybounds(df$FP1AlphaMean),type="l",main="FP1Alphamean power according to t#ime")
#plot(df$time,df$FP1AlphaMean,ylim=ybounds(df$FP1AlphaMean),type="l",main="FP1Alphamean power according to t#ime")
#dev.off()
#Y=movavg(df$FP1AlphaMean,20,type="s")
#plot(df$time,Y,type="l",col="red",xlab="time",ylab="Alpha mean power in FP1 electrode(micro volt) ",ylim=c(90,400),main="FP1Alphamean power according to time")
#lines(Y,col="red")
#lo=loess(df$FP1AlphaMean~df$time,span = 0.3)
#lines(predict(lo), col='blue', lwd=2)
#abline(v=450, col="Deeppink",lty=2)
#legend("topright", legend=c("transition of phase","moving average line"),
#       col=c("Deeppink","red"), lty=c(2,1), cex=0.8)

#too see FP1 beta power in two phase
#Y=movavg(df$FP1BetaMean,20,type="s")
#plot(df$time,Y,type="l",col="red",ylim=c(65,125),xlab="time",ylab="Beta mean power in FP1 electrode(micro volt) ",main="FP1Betamean power according to time")
#lo=loess(df$FP1BetaMean~df$time,span = 0.5)
#lines(predict(lo), col='red', lwd=2)
#lo=loess(df$FP1BetaMean~df$time,span = 0.3)
#lines(predict(lo), col='blue', lwd=2)
#abline(v=450, col="Deeppink",lty=2)
#legend("topright", legend=c("transition of phase","moving average line"),
#       col=c("Deeppink","red"), lty=c(2,1), cex=0.8)

#too see FP1 delta power in two phase
#Y=movavg(df$FP1DeltaMean,20,type="s")
#plot(df$time,Y,type="l",ylim=c(90,700),col="red",xlab="time",ylab="Delta mean power in FP1 electrode(micro volt) ",main="FP1Deltamean power according to time")
#lo=loess(df$FP1DeltaMean~df$time,span = 0.5)
#lines(predict(lo), col='red', lwd=2)
#lo=loess(df$FP1DeltaMean~df$time,span = 0.3)
#lines(predict(lo), col='blue', lwd=2)
#abline(v=450, col="Deeppink",lty=2)
#legend("topright", legend=c("transition of phase","moving average line"),
#       col=c("Deeppink","red"), lty=c(2,1), cex=0.8)


#too see FP1 theta power in two phase
#Y=movavg(df$FP1ThetaMean,20,type="s")
#plot(df$time,Y,type="l",ylim=c(150,570),col="red",xlab="time",ylab="Theta mean power in FP1 electrode(micro volt) ",main="FP1 Theta mean power according to time")
#plot(df$time,df$FP1ThetaMean,type="l",ylim=c(90,500),main="FP1Thetamean power according to time")
#lo=loess(df$FP1ThetaMean~df$time,span = 0.5)
#lines(predict(lo), col='red', lwd=2)
#lo=loess(df$FP1ThetaMean~df$time,span = 0.3)
#lines(predict(lo), col='blue', lwd=2)
#abline(v=450, col="Deeppink",lty=2)
#legend("topright", legend=c("transition of phase","moving average line"),
#       col=c("Deeppink","red"), lty=c(2,1), cex=0.8)

#build model between sc and HRV analyse bivarie AND univarie
boxplot(df$SC)
abline(h = median(df$SC), col = "navy", lty = 2)
text(1.35, median(df$SC) + 0.05, "Mediane", col = "navy")
Q1 <- quantile(df$SC, probs = 0.25, na.rm = TRUE)
abline(h = Q1, col = "darkred")
text(1.35, Q1 + 0.05, "Q1 : first quartile", col = "darkred", lty = 2)
Q3 <- quantile(df$SC, probs = 0.75, na.rm = TRUE)
abline(h = Q3, col = "darkred")
text(1.35, Q3 + 0.05, "Q3 : third quartile", col = "darkred", lty = 2)
arrows(x0 = 0.7, y0 = quantile(df$SC, probs = 0.75, na.rm = TRUE), x1 = 0.7, 
       y1 = quantile(df$SC, probs = 0.25, na.rm = TRUE), length = 0.1, code = 3)
#text(0.7, Q1 + (Q3 - Q1)/2 + 0.15, "h", pos = 2)
mtext("boxplot of skin conductance", side = 1)
# analyse between 1.7 to 2

boxplot(df$HRV,ylim=c(80,250))
abline(h = median(df$HRV), col = "navy", lty = 2)
text(1.35, median(df$HRV) + 0.05, "Median", col = "navy")
Q1 <- quantile(df$HRV, probs = 0.25, na.rm = TRUE)
abline(h = Q1, col = "darkred")
text(1.35, Q1 + 0.05, "Q1 : first quartile", col = "darkred", lty = 2)
Q3 <- quantile(df$HRV, probs = 0.75, na.rm = TRUE)
abline(h = Q3, col = "darkred")
text(1.35, Q3 + 0.05, "Q3 : third quartile", col = "darkred", lty = 2)
arrows(x0 = 0.7, y0 = quantile(df$SC, probs = 0.75, na.rm = TRUE), x1 = 0.7, 
       y1 = quantile(df$SC, probs = 0.25, na.rm = TRUE), length = 0.1, code = 3)
#text(0.7, Q1 + (Q3 - Q1)/2 + 0.15, "h", pos = 2)
mtext("boxplot of heart rate variability", side = 1)
# analyse between 110 to 160

#theta~sc or HRV is promissing
#overall looking
dev.off(dev.list()["RStudioGD"])
par(mfrow=c(1,2))
library(car)
plot(df$HRV,df$SC,ylab="skin conductance", xlab="heart rate variability",col=c("red","blue")[df$MediOrStory])
legend( x="topright", 
        legend=c("story phase","meditation phase"),
        col=c("red","blue"), lwd=1, 
        pch=c(16,16) )
#legend("topright", legend=c("meditation phase","story phase"),
#       col=c("blue","red"), lty=c(2,1), cex=0.8)
#locally
#plot(df$HRV,df$SC,ylab="skin conductance", xlab="heart rate variability",col=c("red","blue")[df$MediOrStory], ylim=c(1.6,2.14),xlim=c(110,400))
plot(df$HRV,df$SC,ylab="skin conductance", xlab="heart rate variability",col=c("red","blue")[df$MediOrStory], ylim=ybounds(df$SC),xlim=ybounds( df$HRV))

legend( x="right", 
        legend=c("story phase","meditation phase"),
        col=c("red","blue"), lwd=1, 
        pch=c(16,16) )
#correlation plot between conventional biomarker in different phase
df_bio=subset(df,select=c(HR,HRV,Resp,ST,SC))
library(car)
par(mfrow=c(1,1))
for (col in 1:(ncol(df_bio))){
  for(col1 in col:(ncol(df_bio))){
    if(col!=col1){
      #print(col)
      #print(col1)
      pdf(file = paste(colnames(df_bio[col]),"correlated with",colnames(df_bio[col1]),".pdf"), width = 9, height = 9, pointsize = 10)
      plot(df_bio[,col],df_bio[,col1],ylab=colnames(df_bio[col1]), xlim=ybounds(df_bio[,col]),ylim=ybounds(df_bio[,col1]),xlab=colnames(df_bio[col1]),col=c("red","blue")[df$MediOrStory])
      legend( x="topright", 
          legend=c("story phase","meditation phase"),
          col=c("red","blue"), lwd=1, 
          pch=c(16,16) )
      dev.off()
    }
    }}
dev.off()
dev.set(dev.next())
#plot(df_bio[,1],df_bio[,2],ylab=colnames(df_bio[1]), xlab=colnames(df_bio[2]),col=c("red","blue")[df$MediOrStory])
#df_bio[,2]
#plot(df_bio$SC,df_bio$HRV)


#change axes
#plot(df$SC,df$HRV,xlab="skin conductance", ylab="heart rate variability")
#plot(df$SC,df$HRV,xlab="skin conductance", ylab="heart rate variability",xlim=c(1.6,2.1),ylim=c(110,150))
regSC_HRV=lm(SC~HRV,data = df)
abline(regSC_HRV,col="red")
summary(regSC_HRV)

#select data
dfslted=df[which(df[,'SC']>1.78),]
dfslted=dfslted[which(dfslted[,'SC']<2.1),]
dfslted=dfslted[which(dfslted[,'HRV']<200),]
regSC_HRV_Fine=lm(SC~HRV,data = dfslted)
dev.off(dev.list()["RStudioGD"])
par(mfrow=c(1,2))
#all selected data
plot(dfslted$HRV,dfslted$SC,ylab="skin conductance", xlab="heart rate variability")
abline(regSC_HRV_Fine,col="red")
plot(df$HRV,df$SC,ylab="skin conductance", xlab="heart rate variability", ylim=c(1.6,2.1),xlim=c(110,150))
abline(regSC_HRV_Fine,col="red")

#identify two phases
graphics.off()
par(mfrow=c(1,1))
dfslted=df[which(df[,'MediOrStory']==1),]
regSC_HRV_Medi=lm(SC~HRV,data = dfslted)
dfslted=df[which(df[,'MediOrStory']==0),]
dfslted=dfslted[which(dfslted[,'HRV']<250),]
regSC_HRV_Story_Fine=lm(SC~HRV,data = dfslted)
dfslted=df[which(df[,'MediOrStory']==0),]
regSC_HRV_Story=lm(SC~HRV,data = dfslted)
plot(df$HRV,df$SC,ylab="skin conductance", xlab="heart rate variability",col=c("red","blue")[df$MediOrStory], ylim=c(1.6,2.14),xlim=c(110,400))
legend( x="topright", 
        legend=c("scatter plot of story phase","scatter plot of meditation","regression during story phase","regression during meditation","regression during story phase with HRV<250"),
        col=c("red","blue","darkseagreen","goldenrod","darkslateblue"), lwd=1, 
        pch=c(16,16) )
abline(regSC_HRV_Medi,col="goldenrod")
abline(regSC_HRV_Story,col="darkseagreen")
abline(regSC_HRV_Story_Fine,col="darkslateblue")


summary(regSC_HRV_Medi)
summary(regSC_HRV_Story)
summary(regSC_HRV_Story_Fine)

#theta and HRV
#overall looking
dev.off(dev.list()["RStudioGD"])
par(mfrow=c(1,2))
plot(df$HRV,df$FP1ThetaMean,ylab="FP1theta", xlab="heart rate variability")
#locally
plot(df$HRV,df$FP1ThetaMean,ylab="FP1theta", xlab="heart rate variability", ylim=c(1,400),xlim=c(110,150))
dfslted=df[which(df[,'FP1ThetaMean']<400),]
dfslted=dfslted[which(dfslted[,'HRV']>110),]
dfslted=dfslted[which(dfslted[,'HRV']<150),]
#plot(dfslted$HRV,dfslted$FP1ThetaMean)
regFP1Theta_HRV=lm(FP1ThetaMean~HRV,data = dfslted)
abline(regFP1Theta_HRV,col="red")
summary(regFP1Theta_HRV)
#HRV plus interessant et voir la relation entre meditation et HR

#needto be done:
#complete the other eletrodes
#try conventional bio-marker, seperable, color differenciated mind state 0-1 to test whether we can use SVM
#do the same things on eeg signal
#ajoute new derived variable
