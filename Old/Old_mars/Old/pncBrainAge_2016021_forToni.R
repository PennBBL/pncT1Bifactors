
#libraries
library(nlme)
library(reshape2)
library(visreg)
library(e1071)
library(mgcv)
library(gamm4)
library(RLRsim)
library(ggplot2)

#load data
go1Name<-"/Users/sattertt/Documents/Magic Briefcase/PROJECTS/pncDataReleases/go1_20160207/n1601_go1_datarel_020716.csv"

dataIn<-read.csv(go1Name)

#remove health excludes from all
data<-dataIn[which(dataIn$healthExclude==0),]

#fix certain variables
data$male<-NA
data$male[which(data$sex==2)]<-"female"
data$male[which(data$sex==1)]<-"male"
data$male<-as.factor(data$male)

data$age<-data$ageAtGo1Scan/12
data$ageSq<-(data$age-mean(data$age))^2
data$ageCub<-(data$age-mean(data$age))^3


data$white<-NA
data$white[which(data$race==1)]<-"caucasian"
data$white[which(data$race!=1)]<-"notCaucaisian"
data$white<-as.factor(data$white)

data$healthy<-"notHealthy"
data$healthy[which(data$goassessSmryPsychOverallRtg<4 & data$goassessPstd!="4PS" & data$ltnExclude==0)]<-"healthy"
data$health<-as.factor(data$healthy)

data$icv<-data$mprageMassICV


#separate into "feature" -- just ROI values -- and "data" --- everything else, but includes "outcome"-- here for you overallPsych
featuresVol<-data[which(data$mprageSbiaExclude==0),grep("mprage_mars_vol",names(data))]
dataVol<-data[which(data$mprageSbiaExclude==0),]


##REGRESS OUT ALL COVARIATES-- anything in your mass univariate model

#regress covariates out of predicted variable-- here age, but for you will be overallPsych
dataVol$ageRegressed<-resid(lm(dataVol$age~dataVol$white+dataVol$male+dataVol$icv))+mean(data$age)


#regress out covariate  volumetric data-- same covaraites as above
featuresVolRegressed<-matrix(NA,nrow=dim(featuresVol)[1],ncol=dim(featuresVol)[2])
featuresVolRegressed<-apply(featuresVol,2, function(x) resid(lm(x~dataVol$white+dataVol$icv+dataVol$male))+mean(x))


###FUNCTIONS ###
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


###RUN MODELS
dataVol$predictedOverallPsych<-NA
dataVol$predictedOverallPsych<-svr.10fold(dataVol$ageRegressed,featuresVolRegressed) #need to replace "ageRegressed" with "overallPsychRegressed"
cor(dataVol$predictedOverallPsych,dataVol$age) #what is correlation between predicted overall psych from SVR and actual overallPsych

