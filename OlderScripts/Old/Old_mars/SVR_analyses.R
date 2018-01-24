############################
##### RUN SVR ANALYSES #####
############################

#Load libraries
library(nlme)
library(reshape2)
library(visreg)
library(e1071)
library(mgcv)
library(gamm4)
library(RLRsim)
library(ggplot2)

#Load data
go1Name<-"/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/n1385_MARS_datarel_020716_ravens.rds"
subjData<-readRDS(go1Name)

##Exclude based on healthExclude, averageRating, and mprageSbiaMarsQaExclude
subjData <- subjData[which(subjData$ACROSS.INCLUDE == 1), ]

#Remove the 17 people in the file that are missing medu data
subjData<-subjData[which(is.na(subjData$meduCnbGo1)==FALSE),]


##Preprocess variables

#Need to define age squared
subjData$ageSq<-(subjData$age-mean(subjData$age))^2

#Define vars
subjData$icv<-subjData$mprageMassICV
subjData$medu<-subjData$meduCnbGo1

#Separate into "feature" -- just ROI values -- and "data" --- everything else, but includes "outcome" (overallPsych)
featuresVol<-subjData[which(subjData$mprageSbiaExclude==0),grep("mprage_mars_vol",names(subjData))]
dataVol<-subjData



##REGRESS OUT ALL COVARIATES-- anything in your mass univariate model

#regress covariates out of predicted variable (goassessItemBifactor4FactorOverallPsy)
dataVol$OverallPsychRegressed<-resid(lm(dataVol$goassessItemBifactor4FactorOverallPsy~dataVol$age+dataVol$ageSq+dataVol$sex+dataVol$white))+mean(dataVol$goassessItemBifactor4FactorOverallPsy)


#regress out covariate  volumetric data-- same covariates as above
featuresVolRegressed<-matrix(NA,nrow=dim(featuresVol)[1],ncol=dim(featuresVol)[2])
featuresVolRegressed<-apply(featuresVol,2, function(x) resid(lm(x~dataVol$age+dataVol$ageSq+dataVol$sex+dataVol$white))+mean(x))


### FUNCTIONS ###
#this is a function to run 10 fold SVR

svr.10fold<-function(y,features){
  nfolds=10
  n<-length(y)
  fits<-rep(NA,n)
  rand.vec<-sample( ceiling(seq(0.0001,(nfolds-0.0001), length.out=n)) )
  for(fold in 1:nfolds){
    y.sub<-y[which(rand.vec!=fold)]
    features.sub<-features[which(rand.vec!=fold),]
    svr.fold<-svm(y=y.sub, x=features.sub, cross=1, kernel='linear')
    fits[which(rand.vec==fold)]<-predict(svr.fold,features[which(rand.vec==fold),])
    cat('Done fold', paste(fold, nfolds, sep='/'), '\n')
  }
  return(fits)
}


##Run Models
dataVol$predictedOverallPsych<-NA
dataVol$predictedOverallPsych<-svr.10fold(dataVol$OverallPsychRegressed,featuresVolRegressed)
cor(dataVol$predictedOverallPsych,dataVol$OverallPsychRegressed) #what is correlation between predicted overall psych from SVR and actual overallPsych (with covariates regressed out)

