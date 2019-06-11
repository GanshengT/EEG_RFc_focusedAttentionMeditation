#this is the whole process function
#il can be perfected if you add function to it
#check the path is well set(the source function (df_generation,graph_generation) should be in this path)
setwd("E:/important file/CENTRALE/BCI ET EEG/Rprocess")


if(!exists("df_generation", mode="function")) source("df_generation.R")
if(!exists("graph_generation", mode="function")) source("graph_generation.R")
#import one time these two function is enough

main=function(){

  #csv construction
  df=df_generation("MOHUstoryBioRes","MOHUmeditationBioRes")
  #set the path to where you want to save image file
  setwd("E:/important file/CENTRALE/BCI ET EEG/Rprocess/data/hugues_auto")
  #plz go to graph_generation to understand more about arguement
  pValTable=graph_generation(df,"Hugues")
  rm(list = objects())
}

main()

