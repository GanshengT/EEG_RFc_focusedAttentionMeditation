# EEG signals R-language analysis and classification 

This project refers to the 2018-2019 **Brain-Computer Interface based on the Relaxation state** in CentraleSupélec L2S laboratory. Here aresome available functions for analysing EEG signals as well as skin conductance, skin temperature, heart rate and respiration rate. These functions are helpful to build the machine-learning classifiers, two classifier random forest and SVM are embedded. These R scripts are meant to run in your local computer with downloaded data.

## Getting Started

### Prerequisites
* install R-studio and appopriate [R environment] (https://www.rstudio.com/)
* the data should be in txt format of the developped protocol stated in the report with sample rate of 256, you can take a look at *MOHUstoryBioRes.txt* in storing data. The recording data should last more than 10 minutes for meditation and more than 8 minutes for story phase
* make sure every `setwd()` is changed and adapt to the path of the data in your local computer.



### Installing packages
The functions use certain R packages, make sure you install them before runing
install library using `install.package("signal")` for example



## Function argument and output

### correlation_MOHU

This is a script that help you be acquainted with R-processing of EEG signals. 
* first use `setwd(to be completed)` to set up the path to the recording data
* then `HU=read.table(to be completed)` enter the data's filename
* after runing the script you will have *df* containing the extracting *MBA* (please refer to the final report) for four wave bands of each electrode
* some images will be shown in plot window to let you have a quick look of the tendency of each signal

### df_generation
This function serve as a part in main function, it will return a matrix *df* containing the value of *MBA, SC, ST, HR, Resp, HRV* (please refer to the final report) for each second. You can change the inteval by modifying the Fs and n in `spec=specgram(x=HU$FP2,Fs=256,n=512)`
* df_generation_4plus4 is used for subjects who do second experiment with 4 minutes' meditation and 4 minutes' story phase
* df_generation_5min is used for subjects who do another meditation which last more than 5 minutes <br>
below is an example:
```
HU=df_generation("MOHUstoryBioRes","MOHUmeditationBioRes")
```
### graph_generation
This function will save graphs as pdf format representing the correlation between EEG signals and tradictional biomarkers. These graphs include the tendency of *MBA* value and tradictional biomarks during the experiment, the scatter plot between EEG signals and *HRV* with respect the spatial position and wave band of the EEG signals, the regression and concerning statistic of each two signals.

below is an example:
```
graph_generation(df matrix's name,"subject name")
```

### rfclassifier
This script contains a SVM model using *SC* and *HRV* to predict the meditation and its graphic representations. From line 113, the codes are identical with *rfclassifier30MAY*, you can ignore this part.
 
### rfclassifier30MAY
This script contains the steps to obtains PAC values' importance (MEANREDUCEDGINI):<br>
firstly you can read *df* .csv and two csv file that store *PAC* (phase-amplitude coupling) values respectively of meditation and story phase into working environment.
```
setwd(your path)
df=read.csv(file="CACAstoryBio CACAmeditationBio _df.csv")
PACS=read.csv(file="CACA_storyPAC.csv",header = F)
PACM=read.csv(file="CACA_meditationPAC.csv", header = F)
```
Then run the script, in this step `FeatImpt=randomForest::importance(Model_rf)`(line 76), FeatImpt gives out the importance<br>

* RfAccuracy function of this script takes *subjectname* as argument and give out the confustion matrix using built random forest model
In order to obtain a csv containing the statistics for all subjects, please follow the instruction on line 219

* RfAccuracy_reExp function has the same goal as RfAccuracy, but it is designed to use the data of the subjects who did a meditation validation experiment.

* RfAccuracy_reExp2 function has the same goal as RfAccuracy, but it is designed to use the data of the subjects who did a meditation validation experiment and a faked story reference experiment.

### rf_PAC
This script is used to feed random forest with MBA, ratios and PAC features and return the statistical results
* Input: PAC meditation matrix, PAC story matrix, df matrix, those three matrix should be consistent on time
example for file naming rule:
```
setwd("D:/20182019EEG_CS/df_matrix")
  df=read.csv(file=paste0(subjectName,"storyBio ",subjectName,"meditationBio _df.csv"))
  setwd("D:/20182019EEG_CS/PACmatrix")
  PACS=read.csv(file=paste0("AUTO_",subjectName,"_story.csv"),header = F)
  PACM=read.csv(file=paste0("AUTO_",subjectName,"_meditation.csv"), header = F)
```

In rf_PAC_main you have the possibility to set the subject list which includes subjects that you want to apply the random forest model.
example:
```
subject_list=c("CACA","DEER","EZLE","GRCL","LENO","LOVA","MATA","MOHU","NIJO","PAMA","PRBA","TRWI","ZAMI")
```
The function staProc is used to further analyse the statistics.

* output: the visualisation of confusion matrix, the overall confusion matrix, infomation for feature importance EEG topographie, concerning statistics for random forest model. These images or csv file will be saved at the path that you set


### Main
It is the main function that integrate all the functionality of stated function. you can download the main.R to see the example of one subject.



## Deployment and Extention

* The deployment needs real-time storation, that's to say the explotation of the recording signal from mind media device to computer should be real-time. In the computer, you can add additional function in main function to construct a *df* matrix for an interval (by changing parameters) and use built classifier to predict, then use JAVA-built (one possible option) graphical interface to give feedback 
* The analysis of PAC value's importance is not yet automated for each subject, nor does the confusion matrix of the model taking PAC values as features.



## Authors

* **Gansheng TAN** 

See also the list of [contributors](https://github.com/GanshengT/EEG_R_ml_corr20182019) who participated in this project.

## Acknowledgments
* Antoine Chaillet, Hugues Mounier, Luca Greco who gave critical comments on processing EEG signals and tradictional biomarks' signal
* Wei Mu who provided recording data and help run the test


## Contact
If you have any question or advices of improvement please contact Gansheng TAN: aegean0045@outlook.com

If you are interested in [connecting Rstudio with this repository]https://happygitwithr.com/rstudio-git-github.html
