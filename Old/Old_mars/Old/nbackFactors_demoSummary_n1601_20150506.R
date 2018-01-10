#READ IN FILES
data.final<-readRDS("/import/speedy/eons/progs/frac2back/shanmuganBifactorPaper/subjectData/nbackFactors_n1601_subjData_20150506.rds")

#td
data.final$Td<-0
data.final$Td[which(data.final$dxpmr4=="1TD" & data.final$nback_excluded==0)]<-1  
data.final$Td[which(data.final$ACROSS.INCLUDE==0)]<-0
numTd<-sum(data.final$Td)
ageTdMean<-mean(data.final$age[which(data.final$Td==1)])
ageTdSd<-sd(data.final$age[which(data.final$Td==1)])
femTd<-(length(which(data.final$sex==2 & data.final$Td==1)))/numTd
whiteTd<-(length(which(data.final$race==1 & data.final$Td==1)))/numTd
meduTdMean<-mean(data.final$medu_cnb_go1[which(data.final$Td==1)],na.rm=T)
meduTdSd<-sd(data.final$medu_cnb_go1[which(data.final$Td==1)],na.rm=T)
dprimeTdMean<-mean(data.final$nback_beh_all_dprime[which(data.final$Td==1)],na.rm=T)
dprimeTdSd<-sd(data.final$nback_beh_all_dprime[which(data.final$Td==1)],na.rm=T)



#rename PS for consistency
data.final$Ps<-0
data.final$Ps[which(data.final$dxpmr4=="4PS" & data.final$nback_excluded==0)]<-1 
data.final$Ps[which(data.final$ACROSS.INCLUDE==0)]<-0
numPs<-sum(data.final$Ps)
agePsMean<-mean(data.final$age[which(data.final$Ps==1)])
agePsSd<-sd(data.final$age[which(data.final$Ps==1)])
femPs<-length(which(data.final$sex==2 & data.final$Ps==1))/numPs
whitePs<-(length(which(data.final$race==1 & data.final$Ps==1)))/numPs
meduPsMean<-mean(data.final$medu_cnb_go1[which(data.final$Ps==1)],na.rm=T)
meduPsSd<-sd(data.final$medu_cnb_go1[which(data.final$Ps==1)],na.rm=T)
dprimePsMean<-mean(data.final$nback_beh_all_dprime[which(data.final$Ps==1)],na.rm=T)
dprimePsSd<-sd(data.final$nback_beh_all_dprime[which(data.final$Ps==1)],na.rm=T)


#ADD
data.final$Add<-0
data.final$Add[which(data.final$smry_add==4 & data.final$nback_excluded==0)]<-1 
data.final$Add[which(data.final$ACROSS.INCLUDE==0)]<-0
numAdd<-sum(data.final$Add)
ageAddMean<-mean(data.final$age[which(data.final$Add==1)])
ageAddSd<-sd(data.final$age[which(data.final$Add==1)])
femAdd<-length(which(data.final$sex==2 & data.final$Add==1))/numAdd
whiteAdd<-(length(which(data.final$race==1 & data.final$Add==1)))/numAdd
meduAddMean<-mean(data.final$medu_cnb_go1[which(data.final$Add==1)],na.rm=T)
meduAddSd<-sd(data.final$medu_cnb_go1[which(data.final$Add==1)],na.rm=T)
dprimeAddMean<-mean(data.final$nback_beh_all_dprime[which(data.final$Add==1)],na.rm=T)
dprimeAddSd<-sd(data.final$nback_beh_all_dprime[which(data.final$Add==1)],na.rm=T)

#AGR
data.final$Agr<-0
data.final$Agr[which(data.final$smry_agr==4 & data.final$nback_excluded==0)]<-1  
data.final$Agr[which(data.final$ACROSS.INCLUDE==0)]<-0
numAgr<-sum(data.final$Agr)
ageAgrMean<-mean(data.final$age[which(data.final$Agr==1)])
ageAgrSd<-sd(data.final$age[which(data.final$Agr==1)])
femAgr<-length(which(data.final$sex==2 & data.final$Agr==1))/numAgr
whiteAgr<-(length(which(data.final$race==1 & data.final$Agr==1)))/numAgr
meduAgrMean<-mean(data.final$medu_cnb_go1[which(data.final$Agr==1)],na.rm=T)
meduAgrSd<-sd(data.final$medu_cnb_go1[which(data.final$Agr==1)],na.rm=T)
dprimeAgrMean<-mean(data.final$nback_beh_all_dprime[which(data.final$Agr==1)],na.rm=T)
dprimeAgrSd<-sd(data.final$nback_beh_all_dprime[which(data.final$Agr==1)],na.rm=T)


#ANO
data.final$Ano<-0
data.final$Ano[which(data.final$smry_ano==4 & data.final$nback_excluded==0)]<-1  
data.final$Ano[which(data.final$ACROSS.INCLUDE==0)]<-0
numAno<-sum(data.final$Ano)
ageAnoMean<-mean(data.final$age[which(data.final$Ano==1)])
ageAnoSd<-sd(data.final$age[which(data.final$Ano==1)])
femAno<-length(which(data.final$sex==2 & data.final$Ano==1))/numAno
whiteAno<-(length(which(data.final$race==1 & data.final$Ano==1)))/numAno
meduAnoMean<-mean(data.final$medu_cnb_go1[which(data.final$Ano==1)],na.rm=T)
meduAnoSd<-sd(data.final$medu_cnb_go1[which(data.final$Ano==1)],na.rm=T)
dprimeAnoMean<-mean(data.final$nback_beh_all_dprime[which(data.final$Ano==1)],na.rm=T)
dprimeAnoSd<-sd(data.final$nback_beh_all_dprime[which(data.final$Ano==1)],na.rm=T)

#BUL
data.final$Bul<-0
data.final$Bul[which(data.final$smry_bul==4 & data.final$nback_excluded==0)]<-1  
data.final$Bul[which(data.final$ACROSS.INCLUDE==0)]<-0
numBul<-sum(data.final$Bul)
ageBulMean<-mean(data.final$age[which(data.final$Bul==1)])
ageBulSd<-sd(data.final$age[which(data.final$Bul==1)])
femBul<-length(which(data.final$sex==2 & data.final$Bul==1))/numBul
whiteBul<-(length(which(data.final$race==1 & data.final$Bul==1)))/numBul
meduBulMean<-mean(data.final$medu_cnb_go1[which(data.final$Bul==1)],na.rm=T)
meduBulSd<-sd(data.final$medu_cnb_go1[which(data.final$Bul==1)],na.rm=T)
dprimeBulMean<-mean(data.final$nback_beh_all_dprime[which(data.final$Bul==1)],na.rm=T)
dprimeBulSd<-sd(data.final$nback_beh_all_dprime[which(data.final$Bul==1)],na.rm=T)


#CON
data.final$Con<-0
data.final$Con[which(data.final$smry_con==4 & data.final$nback_excluded==0)]<-1  
data.final$Con[which(data.final$ACROSS.INCLUDE==0)]<-0
numCon<-sum(data.final$Con)
ageConMean<-mean(data.final$age[which(data.final$Con==1)])
ageConSd<-sd(data.final$age[which(data.final$Con==1)])
femCon<-length(which(data.final$sex==2 & data.final$Con==1))/numCon
whiteCon<-(length(which(data.final$race==1 & data.final$Con==1)))/numCon
meduConMean<-mean(data.final$medu_cnb_go1[which(data.final$Con==1)],na.rm=T)
meduConSd<-sd(data.final$medu_cnb_go1[which(data.final$Con==1)],na.rm=T)
dprimeConMean<-mean(data.final$nback_beh_all_dprime[which(data.final$Con==1)],na.rm=T)
dprimeConSd<-sd(data.final$nback_beh_all_dprime[which(data.final$Con==1)],na.rm=T)

#DEP
data.final$Dep<-0
data.final$Dep[which(data.final$smry_dep==4 & data.final$nback_excluded==0)]<-1  
data.final$Dep[which(data.final$ACROSS.INCLUDE==0)]<-0
numDep<-sum(data.final$Dep)
ageDepMean<-mean(data.final$age[which(data.final$Dep==1)])
ageDepSd<-sd(data.final$age[which(data.final$Dep==1)])
femDep<-length(which(data.final$sex==2 & data.final$Dep==1))/numDep
whiteDep<-(length(which(data.final$race==1 & data.final$Dep==1)))/numDep
meduDepMean<-mean(data.final$medu_cnb_go1[which(data.final$Dep==1)],na.rm=T)
meduDepSd<-sd(data.final$medu_cnb_go1[which(data.final$Dep==1)],na.rm=T)
dprimeDepMean<-mean(data.final$nback_beh_all_dprime[which(data.final$Dep==1)],na.rm=T)
dprimeDepSd<-sd(data.final$nback_beh_all_dprime[which(data.final$Dep==1)],na.rm=T)

#GAD
data.final$Gad<-0
data.final$Gad[which(data.final$smry_gad==4 & data.final$nback_excluded==0)]<-1  
data.final$Gad[which(data.final$ACROSS.INCLUDE==0)]<-0
numGad<-sum(data.final$Gad)
ageGadMean<-mean(data.final$age[which(data.final$Gad==1)])
ageGadSd<-sd(data.final$age[which(data.final$Gad==1)])
femGad<-length(which(data.final$sex==2 & data.final$Gad==1))/numGad
whiteGad<-(length(which(data.final$race==1 & data.final$Gad==1)))/numGad
meduGadMean<-mean(data.final$medu_cnb_go1[which(data.final$Gad==1)],na.rm=T)
meduGadSd<-sd(data.final$medu_cnb_go1[which(data.final$Gad==1)],na.rm=T)
dprimeGadMean<-mean(data.final$nback_beh_all_dprime[which(data.final$Gad==1)],na.rm=T)
dprimeGadSd<-sd(data.final$nback_beh_all_dprime[which(data.final$Gad==1)],na.rm=T)

#MAN
data.final$Man<-0
data.final$Man[which(data.final$smry_man==4 & data.final$nback_excluded==0)]<-1  
data.final$Man[which(data.final$ACROSS.INCLUDE==0)]<-0
numMan<-sum(data.final$Man)
ageManMean<-mean(data.final$age[which(data.final$Man==1)])
ageManSd<-sd(data.final$age[which(data.final$Man==1)])
femMan<-length(which(data.final$sex==2 & data.final$Man==1))/numMan
whiteMan<-(length(which(data.final$race==1 & data.final$Man==1)))/numMan
meduManMean<-mean(data.final$medu_cnb_go1[which(data.final$Man==1)],na.rm=T)
meduManSd<-sd(data.final$medu_cnb_go1[which(data.final$Man==1)],na.rm=T)
dprimeManMean<-mean(data.final$nback_beh_all_dprime[which(data.final$Man==1)],na.rm=T)
dprimeManSd<-sd(data.final$nback_beh_all_dprime[which(data.final$Man==1)],na.rm=T)

#OCD
data.final$Ocd<-0
data.final$Ocd[which(data.final$smry_ocd==4 & data.final$nback_excluded==0)]<-1  
data.final$Ocd[which(data.final$ACROSS.INCLUDE==0)]<-0
numOcd<-sum(data.final$Ocd)
ageOcdMean<-mean(data.final$age[which(data.final$Ocd==1)])
ageOcdSd<-sd(data.final$age[which(data.final$Ocd==1)])
femOcd<-length(which(data.final$sex==2 & data.final$Ocd==1))/numOcd
whiteOcd<-(length(which(data.final$race==1 & data.final$Ocd==1)))/numOcd
meduOcdMean<-mean(data.final$medu_cnb_go1[which(data.final$Ocd==1)],na.rm=T)
meduOcdSd<-sd(data.final$medu_cnb_go1[which(data.final$Ocd==1)],na.rm=T)
dprimeOcdMean<-mean(data.final$nback_beh_all_dprime[which(data.final$Ocd==1)],na.rm=T)
dprimeOcdSd<-sd(data.final$nback_beh_all_dprime[which(data.final$Ocd==1)],na.rm=T)

#ODD
data.final$Odd<-0
data.final$Odd[which(data.final$smry_odd==4 & data.final$nback_excluded==0)]<-1  
data.final$Odd[which(data.final$ACROSS.INCLUDE==0)]<-0
numOdd<-sum(data.final$Odd)
ageOddMean<-mean(data.final$age[which(data.final$Odd==1)])
ageOddSd<-sd(data.final$age[which(data.final$Odd==1)])
femOdd<-length(which(data.final$sex==2 & data.final$Odd==1))/numOdd
whiteOdd<-(length(which(data.final$race==1 & data.final$Odd==1)))/numOdd
meduOddMean<-mean(data.final$medu_cnb_go1[which(data.final$Odd==1)],na.rm=T)
meduOddSd<-sd(data.final$medu_cnb_go1[which(data.final$Odd==1)],na.rm=T)
dprimeOddMean<-mean(data.final$nback_beh_all_dprime[which(data.final$Odd==1)],na.rm=T)
dprimeOddSd<-sd(data.final$nback_beh_all_dprime[which(data.final$Odd==1)],na.rm=T)

#PAN
data.final$Pan<-0
data.final$Pan[which(data.final$smry_pan==4 & data.final$nback_excluded==0)]<-1  
data.final$Pan[which(data.final$ACROSS.INCLUDE==0)]<-0
numPan<-sum(data.final$Pan)
agePanMean<-mean(data.final$age[which(data.final$Pan==1)])
agePanSd<-sd(data.final$age[which(data.final$Pan==1)])
femPan<-length(which(data.final$sex==2 & data.final$Pan==1))/numPan
whitePan<-(length(which(data.final$race==1 & data.final$Pan==1)))/numPan
meduPanMean<-mean(data.final$medu_cnb_go1[which(data.final$Pan==1)],na.rm=T)
meduPanSd<-sd(data.final$medu_cnb_go1[which(data.final$Pan==1)],na.rm=T)
dprimePanMean<-mean(data.final$nback_beh_all_dprime[which(data.final$Pan==1)],na.rm=T)
dprimePanSd<-sd(data.final$nback_beh_all_dprime[which(data.final$Pan==1)],na.rm=T)


#PHB
data.final$Phb<-0
data.final$Phb[which(data.final$smry_phb==4 & data.final$nback_excluded==0)]<-1  
data.final$Phb[which(data.final$ACROSS.INCLUDE==0)]<-0
numPhb<-sum(data.final$Phb)
agePhbMean<-mean(data.final$age[which(data.final$Phb==1)])
agePhbSd<-sd(data.final$age[which(data.final$Phb==1)])
femPhb<-length(which(data.final$sex==2 & data.final$Phb==1))/numPhb
whitePhb<-(length(which(data.final$race==1 & data.final$Phb==1)))/numPhb
meduPhbMean<-mean(data.final$medu_cnb_go1[which(data.final$Phb==1)],na.rm=T)
meduPhbSd<-sd(data.final$medu_cnb_go1[which(data.final$Phb==1)],na.rm=T)
dprimePhbMean<-mean(data.final$nback_beh_all_dprime[which(data.final$Phb==1)],na.rm=T)
dprimePhbSd<-sd(data.final$nback_beh_all_dprime[which(data.final$Phb==1)],na.rm=T)

#PTD
data.final$Ptd<-0
data.final$Ptd[which(data.final$smry_ptd==4 & data.final$nback_excluded==0)]<-1  
data.final$Ptd[which(data.final$ACROSS.INCLUDE==0)]<-0
numPtd<-sum(data.final$Ptd)
agePtdMean<-mean(data.final$age[which(data.final$Ptd==1)])
agePtdSd<-sd(data.final$age[which(data.final$Ptd==1)])
femPtd<-length(which(data.final$sex==2 & data.final$Ptd==1))/numPtd
whitePtd<-(length(which(data.final$race==1 & data.final$Ptd==1)))/numPtd
meduPtdMean<-mean(data.final$medu_cnb_go1[which(data.final$Ptd==1)],na.rm=T)
meduPtdSd<-sd(data.final$medu_cnb_go1[which(data.final$Ptd==1)],na.rm=T)
dprimePtdMean<-mean(data.final$nback_beh_all_dprime[which(data.final$Ptd==1)],na.rm=T)
dprimePtdSd<-sd(data.final$nback_beh_all_dprime[which(data.final$Ptd==1)],na.rm=T)


#SEP
data.final$Sep<-0
data.final$Sep[which(data.final$smry_sep==4 & data.final$nback_excluded==0)]<-1  
data.final$Sep[which(data.final$ACROSS.INCLUDE==0)]<-0
numSep<-sum(data.final$Sep)
ageSepMean<-mean(data.final$age[which(data.final$Sep==1)])
ageSepSd<-sd(data.final$age[which(data.final$Sep==1)])
femSep<-length(which(data.final$sex==2 & data.final$Sep==1))/numSep
whiteSep<-(length(which(data.final$race==1 & data.final$Sep==1)))/numSep
meduSepMean<-mean(data.final$medu_cnb_go1[which(data.final$Sep==1)],na.rm=T)
meduSepSd<-sd(data.final$medu_cnb_go1[which(data.final$Sep==1)],na.rm=T)
dprimeSepMean<-mean(data.final$nback_beh_all_dprime[which(data.final$Sep==1)],na.rm=T)
dprimeSepSd<-sd(data.final$nback_beh_all_dprime[which(data.final$Sep==1)],na.rm=T)


#SOC
data.final$Soc<-0
data.final$Soc[which(data.final$smry_soc==4 & data.final$nback_excluded==0)]<-1  
data.final$Soc[which(data.final$ACROSS.INCLUDE==0)]<-0
numSoc<-sum(data.final$Soc)
ageSocMean<-mean(data.final$age[which(data.final$Soc==1)])
ageSocSd<-sd(data.final$age[which(data.final$Soc==1)])
femSoc<-length(which(data.final$sex==2 & data.final$Soc==1))/numSoc
whiteSoc<-(length(which(data.final$race==1 & data.final$Soc==1)))/numSoc
meduSocMean<-mean(data.final$medu_cnb_go1[which(data.final$Soc==1)],na.rm=T)
meduSocSd<-sd(data.final$medu_cnb_go1[which(data.final$Soc==1)],na.rm=T)
dprimeSocMean<-mean(data.final$nback_beh_all_dprime[which(data.final$Soc==1)],na.rm=T)
dprimeSocSd<-sd(data.final$nback_beh_all_dprime[which(data.final$Soc==1)],na.rm=T)


###MERGE ACROSS DISORDERS

#get total number of diagnoses
dxNames<-c("bblid","ACROSS.INCLUDE","nback_excluded","Add","Agr","Ano","Bul","Con","Dep","Gad","Man","Ocd","Odd","Pan","Phb","Ps","Ptd","Sep","Soc")
dxDf<-data.final[,dxNames]
dxDf<-dxDf[which(dxDf$ACROSS.INCLUDE==1 & dxDf$nback_excluded==0),]
totDx<-rowSums(dxDf[,3:19]) 
length(which(totDx==0)) #351
length(which(totDx==1)) #249
length(which(totDx>1)) #529

stop()

numComb<-c(numTd,numAdd,numAgr,numAno,numBul,numCon,numDep,numGad,numMan,numOcd,numOdd,numPan,numPhb,numPs,numPtd,numSep,numSoc)
ageMeanComb<-round(c(ageTdMean,ageAddMean,ageAgrMean,ageAnoMean,ageBulMean,ageConMean,ageDepMean,ageGadMean,ageManMean,ageOcdMean,ageOddMean,agePanMean,agePhbMean,agePsMean,agePtdMean,ageSepMean,ageSocMean),2)

ageSdComb<-round(c(ageTdSd,ageAddSd,ageAgrSd,ageAnoSd,ageBulSd,ageConSd,ageDepSd,ageGadSd,ageManSd,ageOcdSd,ageOddSd,agePanSd,agePhbSd,agePsSd,agePtdSd,ageSepSd,ageSocSd),2)

femComb<-round(c(femTd,femAdd,femAgr,femAno,femBul,femCon,femDep,femGad,femMan,femOcd,femOdd,femPan,femPhb,femPs,femPtd,femSep,femSoc),3)*100

whiteComb<-round(c(whiteTd,whiteAdd,whiteAgr,whiteAno,whiteBul,whiteCon,whiteDep,whiteGad,whiteMan,whiteOcd,whiteOdd,whitePan,whitePhb,whitePs,whitePtd,whiteSep,whiteSoc),3)*100

meduMeanComb<-round(c(meduTdMean,meduAddMean,meduAgrMean,meduAnoMean,meduBulMean,meduConMean,meduDepMean,meduGadMean,meduManMean,meduOcdMean,meduOddMean,meduPanMean,meduPhbMean,meduPsMean,meduPtdMean,meduSepMean,meduSocMean),2)

meduSdComb<-round(c(meduTdSd,meduAddSd,meduAgrSd,meduAnoSd,meduBulSd,meduConSd,meduDepSd,meduGadSd,meduManSd,meduOcdSd,meduOddSd,meduPanSd,meduPhbSd,meduPsSd,meduPtdSd,meduSepSd,meduSocSd),2)

dprimeMeanComb<-round(c(dprimeTdMean,dprimeAddMean,dprimeAgrMean,dprimeAnoMean,dprimeBulMean,dprimeConMean,dprimeDepMean,dprimeGadMean,dprimeManMean,dprimeOcdMean,dprimeOddMean,dprimePanMean,dprimePhbMean,dprimePsMean,dprimePtdMean,dprimeSepMean,dprimeSocMean),2)

dprimeSdComb<-round(c(dprimeTdSd,dprimeAddSd,dprimeAgrSd,dprimeAnoSd,dprimeBulSd,dprimeConSd,dprimeDepSd,dprimeGadSd,dprimeManSd,dprimeOcdSd,dprimeOddSd,dprimePanSd,dprimePhbSd,dprimePsSd,dprimePtdSd,dprimeSepSd,dprimeSocSd),2)

#MAKE TABLE
dxNamesFull<-c("Typically Developing","ADHD","Agoraphobia","Anorexia","Bulimia","Conduct Disorder","Major Depression","GAD","Mania","OCD","ODD","Panic","Phobias","Psychosis-spectrum","PTSD","Separation Anxiety","Social Phobia")
table1<-as.data.frame(matrix(nrow=17,ncol=6))
row.names(table1)<-dxNamesFull
colnames(table1)[1]<-"n"
colnames(table1)[2]<-"Mean Age (S.D.)"
colnames(table1)[3]<-"Female (%)"
colnames(table1)[4]<-"Caucasian (%)"
colnames(table1)[5]<-"Years Maternal Education (S.D.)"
colnames(table1)[6]<-"Mean N-back Performance (S.D.)"
table1[,1]<-numComb
table1[,3]<-femComb
table1[,4]<-whiteComb

#combine age mean/sd
for (i in 1:17){
	table1[i,2]<-paste(ageMeanComb[i]," (",ageSdComb[i],") ", sep='')
        table1[i,5]<-paste(meduMeanComb[i]," (",meduSdComb[i],") ", sep='')
        table1[i,6]<-paste(dprimeMeanComb[i]," (",dprimeSdComb[i],") ", sep='')
}
	
write.csv(table1,"/import/speedy/eons/progs/frac2back/shanmuganBifactorPaper/subjectData/nbackFactors_table1_n1601_20150506.csv",row.names=TRUE,quote=FALSE)


######MAKE BAR CHART BY FACTOR SCORE
facTbl<-as.data.frame(matrix(nrow=17,ncol=10))
colnames(facTbl)[1]<-"OverallMean"
colnames(facTbl)[2]<-"OverallSem"
colnames(facTbl)[3]<-"MoodMean"
colnames(facTbl)[4]<-"MoodSem"
colnames(facTbl)[5]<-"PsychosisMean"
colnames(facTbl)[6]<-"PsychosisSem"
colnames(facTbl)[7]<-"ExternalizingMean"
colnames(facTbl)[8]<-"ExternalizingSem"
colnames(facTbl)[9]<-"PhobiasMean"
colnames(facTbl)[10]<-"PhobiasSem"

row.names(facTbl)<-dxNamesFull

dxs<-c("Td","Add","Agr","Ano","Bul","Con","Dep","Gad","Man","Ocd","Odd","Pan","Phb","Ps","Ptd","Sep","Soc")


for (i in 1:17){
	dx<-dxs[i]
	print(dx)
	y<-data.final[,dx]

        facTbl[i,1]<-mean(data.final$overall_psychopathology_4factor[which(y==1)],na.rm=TRUE)
        facTbl[i,2]<-sd(data.final$overall_psychopathology_4factor[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

	facTbl[i,3]<-mean(data.final$mood_4factor[which(y==1)],na.rm=TRUE)
        facTbl[i,4]<-sd(data.final$mood_4factor[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

        facTbl[i,5]<-mean(data.final$psychosis_4factor[which(y==1)],na.rm=TRUE)
        facTbl[i,6]<-sd(data.final$psychosis_4factor[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

        facTbl[i,7]<-mean(data.final$externalizing_4factor[which(y==1)],na.rm=TRUE)
        facTbl[i,8]<-sd(data.final$externalizing_4factor[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

        facTbl[i,9]<-mean(data.final$phobias_4factor[which(y==1)],na.rm=TRUE)
        facTbl[i,10]<-sd(data.final$phobias_4factor[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

}


#resahpe to long format
library(reshape2)
facTbl$group<-as.factor(dxs)
facTblSem<-facTbl[,c(2,4,6,8,10,11)]
facTblMean<-facTbl[,c(1,3,5,7,9,11)]
facTblMeanLong<-melt(facTblMean,id.vars="group",variable.name="factor",value.name="meanScore")
facTblSemLong<-melt(facTblSem,id.vars="group",variable.name="factor",value.name="semScore")
facTblLong<-facTblMeanLong
facTblLong$semScore<-facTblSemLong$semScore

#remove those with less than 20 sujbects
facTblLong<-facTblLong[-c(grep("Bul",facTblLong$group),grep("Ano",facTblLong$group),grep("Man",facTblLong$group),grep("Pan",facTblLong$group)),]
facTblLong$group<-factor(facTblLong$group) #remove empty factors

#plot w/ ggplot
library(ggplot2)
ggplot(facTblLong, aes(x=group, y=meanScore,fill=factor)) + 
	geom_bar(stat="identity",position=position_dodge(), color="black",size=0.3) +
	scale_fill_manual(values=c("#006400","#0000FF","#4B0082","#DC143C","#FFFF00")) +
	geom_errorbar(aes(ymin=meanScore-semScore, ymax=meanScore+semScore),  width=0.2, position=position_dodge(.9)) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) 

