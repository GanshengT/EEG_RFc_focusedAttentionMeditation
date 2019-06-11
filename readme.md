# EEG signals R-language analysis and classification 

This project refers to the 2018-2019 **Brain-Computer Interface based on the Relaxation state** in CentraleSupélec L2S laboratory. Here are
some available functions for analysing EEG signals as well as skin conductance, skin temperature, heart rate and respiration rate. These 
functions are helpful to build a machine-learning classifiers, two classifier random forest and SVM are embedded.

## Getting Started


### Prerequisites
* install R-studio and appopriate [R environment] (https://www.rstudio.com/)
* the data should be in txt format of the developped protocol stated in the report with sample rate of 256, you can take a look at *MOHUstoryBioRes.txt* in storing data. The recording data should last more than 10 minutes for meditation and more than 8 minutes for story phase


### Installing packages
The functions use certain R packages, make sure you install them before runing
install library using `install.package("signal")` for example



## Function argument and output



### correlation_MOHU

This is a script that help you be acquainted with R-processing of EEG signals. 
* first use `setwd(to be completed)` to set up the path to the recording data
* then `HU=read.table(to be completed)` enter the data's filename
* after runing the script you will have *df* containing the extracting *MBA* (please refer to the final report) for four bands for each electrods 
* some images will be shown in plot window to let you have a quick look of the tendency of each signal

### df_generation
This function serve as a part in main function, it will return a matrix *df* containing the value of *MBA, SC, ST, HR, Resp, HRV* (refer to the final report) for each second. You can change the inteval by modifying the Fs and n in `spec=specgram(x=HU$FP2,Fs=256,n=512)`
* df_generation_4plus4 is used for subjects who do second experiment with 4 minutes' meditation and 4 minutes' story phase
* df_generation_5min is used for subjects who do another meditation which last more than 5 minutes
below is an example:
```
HU=df_generation("MOHUstoryBioRes","MOHUmeditationBioRes")
```
### graph_generation


## Deployment

The deployment needs real-time storation, that's to say the explotation of the recording signal from mind media device to computer should be real-time. In the computer, you can add additional function in main function to construct a *df* matrix for an interval (by changing parameters) and use built classifier to predict, then use JAVA-built (one possible option) graphical interface to give feedback 

## Contributing
Gansheng TAN
Wei MU

## Contact
If you have any question or advices of improvement please contact Gansheng TAN: aegean0045@outlook.com

## Authors

* **Billie Thompson** - *Initial work* - [PurpleBooth](https://github.com/PurpleBooth)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Hat tip to anyone whose code was used
* Inspiration
* etc

If you are interested in [connecting Rstudio with this repository]https://happygitwithr.com/rstudio-git-github.html
