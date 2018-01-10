#READ IN FILES
data.final<-readRDS("/import/monstrum2/Users/antoniak/PNC_asl/subject_data/n875_PNCData.rds")
#NOTE: n875_PNCData.rds only includes the 875 subjects with good asl data included in the final analyses, so no ACROSS.INCLUDE exclusion variables are needed when using n875_PNCData.rds 


#Psychosis Spectrum (Ps)
data.final$Ps<-0
data.final$Ps[which(data.final$goassessDxpmr4=="4PS")]<-1 
#data.final$Ps[which(data.final$ACROSS.INCLUDE.12==0)]<-0
numPs<-sum(data.final$Ps)
agePsMean<-mean(data.final$age[which(data.final$Ps==1)])
agePsSd<-sd(data.final$age[which(data.final$Ps==1)])
femPs<-length(which(data.final$sex==2 & data.final$Ps==1))/numPs
whitePs<-(length(which(data.final$race==1 & data.final$Ps==1)))/numPs
meduPsMean<-mean(data.final$meduCnbGo1[which(data.final$Ps==1)],na.rm=T)
meduPsSd<-sd(data.final$meduCnbGo1[which(data.final$Ps==1)],na.rm=T)


#ADHD (Add)
data.final$Add<-0
data.final$Add[which(data.final$goassessSmryAdd==4)]<-1 
#data.final$Add[which(data.final$ACROSS.INCLUDE.12==0)]<-0
numAdd<-sum(data.final$Add)
ageAddMean<-mean(data.final$age[which(data.final$Add==1)])
ageAddSd<-sd(data.final$age[which(data.final$Add==1)])
femAdd<-length(which(data.final$sex==2 & data.final$Add==1))/numAdd
whiteAdd<-(length(which(data.final$race==1 & data.final$Add==1)))/numAdd
meduAddMean<-mean(data.final$meduCnbGo1[which(data.final$Add==1)],na.rm=T)
meduAddSd<-sd(data.final$meduCnbGo1[which(data.final$Add==1)],na.rm=T)


#Agoraphobia (Agr)
data.final$Agr<-0
data.final$Agr[which(data.final$goassessSmryAgr==4)]<-1  
#data.final$Agr[which(data.final$ACROSS.INCLUDE.12==0)]<-0
numAgr<-sum(data.final$Agr)
ageAgrMean<-mean(data.final$age[which(data.final$Agr==1)])
ageAgrSd<-sd(data.final$age[which(data.final$Agr==1)])
femAgr<-length(which(data.final$sex==2 & data.final$Agr==1))/numAgr
whiteAgr<-(length(which(data.final$race==1 & data.final$Agr==1)))/numAgr
meduAgrMean<-mean(data.final$meduCnbGo1[which(data.final$Agr==1)],na.rm=T)
meduAgrSd<-sd(data.final$meduCnbGo1[which(data.final$Agr==1)],na.rm=T)


#Anorexia (Ano)
data.final$Ano<-0
data.final$Ano[which(data.final$goassessSmryAno==4)]<-1  
#data.final$Ano[which(data.final$ACROSS.INCLUDE.12==0)]<-0
numAno<-sum(data.final$Ano)
ageAnoMean<-mean(data.final$age[which(data.final$Ano==1)])
ageAnoSd<-sd(data.final$age[which(data.final$Ano==1)])
femAno<-length(which(data.final$sex==2 & data.final$Ano==1))/numAno
whiteAno<-(length(which(data.final$race==1 & data.final$Ano==1)))/numAno
meduAnoMean<-mean(data.final$meduCnbGo1[which(data.final$Ano==1)],na.rm=T)
meduAnoSd<-sd(data.final$meduCnbGo1[which(data.final$Ano==1)],na.rm=T)


#Bulimia (Bul)
data.final$Bul<-0
data.final$Bul[which(data.final$goassessSmryBul==4)]<-1  
#data.final$Bul[which(data.final$ACROSS.INCLUDE.12==0)]<-0
numBul<-sum(data.final$Bul)
ageBulMean<-mean(data.final$age[which(data.final$Bul==1)])
ageBulSd<-sd(data.final$age[which(data.final$Bul==1)])
femBul<-length(which(data.final$sex==2 & data.final$Bul==1))/numBul
whiteBul<-(length(which(data.final$race==1 & data.final$Bul==1)))/numBul
meduBulMean<-mean(data.final$meduCnbGo1[which(data.final$Bul==1)],na.rm=T)
meduBulSd<-sd(data.final$meduCnbGo1[which(data.final$Bul==1)],na.rm=T)


#Conduct Disorder (Con)
data.final$Con<-0
data.final$Con[which(data.final$goassessSmryCon==4)]<-1  
#data.final$Con[which(data.final$ACROSS.INCLUDE.12==0)]<-0
numCon<-sum(data.final$Con)
ageConMean<-mean(data.final$age[which(data.final$Con==1)])
ageConSd<-sd(data.final$age[which(data.final$Con==1)])
femCon<-length(which(data.final$sex==2 & data.final$Con==1))/numCon
whiteCon<-(length(which(data.final$race==1 & data.final$Con==1)))/numCon
meduConMean<-mean(data.final$meduCnbGo1[which(data.final$Con==1)],na.rm=T)
meduConSd<-sd(data.final$meduCnbGo1[which(data.final$Con==1)],na.rm=T)


#Depression (Dep)
data.final$Dep<-0
data.final$Dep[which(data.final$goassessSmryDep==4)]<-1  
#data.final$Dep[which(data.final$ACROSS.INCLUDE.12==0)]<-0
numDep<-sum(data.final$Dep)
ageDepMean<-mean(data.final$age[which(data.final$Dep==1)])
ageDepSd<-sd(data.final$age[which(data.final$Dep==1)])
femDep<-length(which(data.final$sex==2 & data.final$Dep==1))/numDep
whiteDep<-(length(which(data.final$race==1 & data.final$Dep==1)))/numDep
meduDepMean<-mean(data.final$meduCnbGo1[which(data.final$Dep==1)],na.rm=T)
meduDepSd<-sd(data.final$meduCnbGo1[which(data.final$Dep==1)],na.rm=T)


#GAD
data.final$Gad<-0
data.final$Gad[which(data.final$goassessSmryGad==4)]<-1  
#data.final$Gad[which(data.final$ACROSS.INCLUDE.12==0)]<-0
numGad<-sum(data.final$Gad)
ageGadMean<-mean(data.final$age[which(data.final$Gad==1)])
ageGadSd<-sd(data.final$age[which(data.final$Gad==1)])
femGad<-length(which(data.final$sex==2 & data.final$Gad==1))/numGad
whiteGad<-(length(which(data.final$race==1 & data.final$Gad==1)))/numGad
meduGadMean<-mean(data.final$meduCnbGo1[which(data.final$Gad==1)],na.rm=T)
meduGadSd<-sd(data.final$meduCnbGo1[which(data.final$Gad==1)],na.rm=T)


#Mania (Man)
data.final$Man<-0
data.final$Man[which(data.final$goassessSmryMan==4)]<-1  
#data.final$Man[which(data.final$ACROSS.INCLUDE.12==0)]<-0
numMan<-sum(data.final$Man)
ageManMean<-mean(data.final$age[which(data.final$Man==1)])
ageManSd<-sd(data.final$age[which(data.final$Man==1)])
femMan<-length(which(data.final$sex==2 & data.final$Man==1))/numMan
whiteMan<-(length(which(data.final$race==1 & data.final$Man==1)))/numMan
meduManMean<-mean(data.final$meduCnbGo1[which(data.final$Man==1)],na.rm=T)
meduManSd<-sd(data.final$meduCnbGo1[which(data.final$Man==1)],na.rm=T)


#Obsessive-Compulsive Disorder (Ocd)
data.final$Ocd<-0
data.final$Ocd[which(data.final$goassessSmryOcd==4)]<-1  
#data.final$Ocd[which(data.final$ACROSS.INCLUDE.12==0)]<-0
numOcd<-sum(data.final$Ocd)
ageOcdMean<-mean(data.final$age[which(data.final$Ocd==1)])
ageOcdSd<-sd(data.final$age[which(data.final$Ocd==1)])
femOcd<-length(which(data.final$sex==2 & data.final$Ocd==1))/numOcd
whiteOcd<-(length(which(data.final$race==1 & data.final$Ocd==1)))/numOcd
meduOcdMean<-mean(data.final$meduCnbGo1[which(data.final$Ocd==1)],na.rm=T)
meduOcdSd<-sd(data.final$meduCnbGo1[which(data.final$Ocd==1)],na.rm=T)


#Oppositional Defiant Disorder (Odd)
data.final$Odd<-0
data.final$Odd[which(data.final$goassessSmryOdd==4)]<-1  
#data.final$Odd[which(data.final$ACROSS.INCLUDE.12==0)]<-0
numOdd<-sum(data.final$Odd)
ageOddMean<-mean(data.final$age[which(data.final$Odd==1)])
ageOddSd<-sd(data.final$age[which(data.final$Odd==1)])
femOdd<-length(which(data.final$sex==2 & data.final$Odd==1))/numOdd
whiteOdd<-(length(which(data.final$race==1 & data.final$Odd==1)))/numOdd
meduOddMean<-mean(data.final$meduCnbGo1[which(data.final$Odd==1)],na.rm=T)
meduOddSd<-sd(data.final$meduCnbGo1[which(data.final$Odd==1)],na.rm=T)


#Panic Disorder (Pan)
data.final$Pan<-0
data.final$Pan[which(data.final$goassessSmryPan==4)]<-1  
#data.final$Pan[which(data.final$ACROSS.INCLUDE.12==0)]<-0
numPan<-sum(data.final$Pan)
agePanMean<-mean(data.final$age[which(data.final$Pan==1)])
agePanSd<-sd(data.final$age[which(data.final$Pan==1)])
femPan<-length(which(data.final$sex==2 & data.final$Pan==1))/numPan
whitePan<-(length(which(data.final$race==1 & data.final$Pan==1)))/numPan
meduPanMean<-mean(data.final$meduCnbGo1[which(data.final$Pan==1)],na.rm=T)
meduPanSd<-sd(data.final$meduCnbGo1[which(data.final$Pan==1)],na.rm=T)


#Specific Phobia (Phb)
data.final$Phb<-0
data.final$Phb[which(data.final$goassessSmryPhb==4)]<-1  
#data.final$Phb[which(data.final$ACROSS.INCLUDE.12==0)]<-0
numPhb<-sum(data.final$Phb)
agePhbMean<-mean(data.final$age[which(data.final$Phb==1)])
agePhbSd<-sd(data.final$age[which(data.final$Phb==1)])
femPhb<-length(which(data.final$sex==2 & data.final$Phb==1))/numPhb
whitePhb<-(length(which(data.final$race==1 & data.final$Phb==1)))/numPhb
meduPhbMean<-mean(data.final$meduCnbGo1[which(data.final$Phb==1)],na.rm=T)
meduPhbSd<-sd(data.final$meduCnbGo1[which(data.final$Phb==1)],na.rm=T)


#Posttraumatic Stress Disorder (Ptd)
data.final$Ptd<-0
data.final$Ptd[which(data.final$goassessSmryPtd==4)]<-1  
#data.final$Ptd[which(data.final$ACROSS.INCLUDE.12==0)]<-0
numPtd<-sum(data.final$Ptd)
agePtdMean<-mean(data.final$age[which(data.final$Ptd==1)])
agePtdSd<-sd(data.final$age[which(data.final$Ptd==1)])
femPtd<-length(which(data.final$sex==2 & data.final$Ptd==1))/numPtd
whitePtd<-(length(which(data.final$race==1 & data.final$Ptd==1)))/numPtd
meduPtdMean<-mean(data.final$meduCnbGo1[which(data.final$Ptd==1)],na.rm=T)
meduPtdSd<-sd(data.final$meduCnbGo1[which(data.final$Ptd==1)],na.rm=T)


#Separation Anxiety Disorder (Sep)
data.final$Sep<-0
data.final$Sep[which(data.final$goassessSmrySep==4)]<-1  
#data.final$Sep[which(data.final$ACROSS.INCLUDE.12==0)]<-0
numSep<-sum(data.final$Sep)
ageSepMean<-mean(data.final$age[which(data.final$Sep==1)])
ageSepSd<-sd(data.final$age[which(data.final$Sep==1)])
femSep<-length(which(data.final$sex==2 & data.final$Sep==1))/numSep
whiteSep<-(length(which(data.final$race==1 & data.final$Sep==1)))/numSep
meduSepMean<-mean(data.final$meduCnbGo1[which(data.final$Sep==1)],na.rm=T)
meduSepSd<-sd(data.final$meduCnbGo1[which(data.final$Sep==1)],na.rm=T)


#Social Anxiety Disorder (Soc)
data.final$Soc<-0
data.final$Soc[which(data.final$goassessSmrySoc==4)]<-1  
#data.final$Soc[which(data.final$ACROSS.INCLUDE.12==0)]<-0
numSoc<-sum(data.final$Soc)
ageSocMean<-mean(data.final$age[which(data.final$Soc==1)])
ageSocSd<-sd(data.final$age[which(data.final$Soc==1)])
femSoc<-length(which(data.final$sex==2 & data.final$Soc==1))/numSoc
whiteSoc<-(length(which(data.final$race==1 & data.final$Soc==1)))/numSoc
meduSocMean<-mean(data.final$meduCnbGo1[which(data.final$Soc==1)],na.rm=T)
meduSocSd<-sd(data.final$meduCnbGo1[which(data.final$Soc==1)],na.rm=T)


###MERGE ACROSS DISORDERS

#get total number of diagnoses
dxNames<-c("bblid","ACROSS.INCLUDE.12","Add","Agr","Ano","Bul","Con","Dep","Gad","Man","Ocd","Odd","Pan","Phb","Ps","Ptd","Sep","Soc")
dxDf<-data.final[,dxNames]
dxDf<-dxDf[which(dxDf$ACROSS.INCLUDE.12==1),]
data.final$totDx<-rowSums(dxDf[,3:18]) #This is how many people have how many diagnoses: sum(totDx==0):236, sum(totDx==1):180, sum(totDx>=2):459 

#Typically Developing (Td)
data.final$Td<-0
data.final$Td[which(data.final$totDx==0)]<-1
#data.final$Td[which(data.final$ACROSS.INCLUDE.12==0)]<-0
numTd<-sum(data.final$Td)
ageTdMean<-mean(data.final$age[which(data.final$Td==1)])
ageTdSd<-sd(data.final$age[which(data.final$Td==1)])
femTd<-(length(which(data.final$sex==2 & data.final$Td==1)))/numTd
whiteTd<-(length(which(data.final$race==1 & data.final$Td==1)))/numTd
meduTdMean<-mean(data.final$meduCnbGo1[which(data.final$Td==1)],na.rm=T)
meduTdSd<-sd(data.final$meduCnbGo1[which(data.final$Td==1)],na.rm=T)

numComb<-c(numTd,numAdd,numAgr,numAno,numBul,numCon,numDep,numGad,numMan,numOcd,numOdd,numPan,numPhb,numPs,numPtd,numSep,numSoc)
ageMeanComb<-round(c(ageTdMean,ageAddMean,ageAgrMean,ageAnoMean,ageBulMean,ageConMean,ageDepMean,ageGadMean,ageManMean,ageOcdMean,ageOddMean,agePanMean,agePhbMean,agePsMean,agePtdMean,ageSepMean,ageSocMean),2)

ageSdComb<-round(c(ageTdSd,ageAddSd,ageAgrSd,ageAnoSd,ageBulSd,ageConSd,ageDepSd,ageGadSd,ageManSd,ageOcdSd,ageOddSd,agePanSd,agePhbSd,agePsSd,agePtdSd,ageSepSd,ageSocSd),2)

femComb<-round(c(femTd,femAdd,femAgr,femAno,femBul,femCon,femDep,femGad,femMan,femOcd,femOdd,femPan,femPhb,femPs,femPtd,femSep,femSoc),3)*100

whiteComb<-round(c(whiteTd,whiteAdd,whiteAgr,whiteAno,whiteBul,whiteCon,whiteDep,whiteGad,whiteMan,whiteOcd,whiteOdd,whitePan,whitePhb,whitePs,whitePtd,whiteSep,whiteSoc),3)*100

meduMeanComb<-round(c(meduTdMean,meduAddMean,meduAgrMean,meduAnoMean,meduBulMean,meduConMean,meduDepMean,meduGadMean,meduManMean,meduOcdMean,meduOddMean,meduPanMean,meduPhbMean,meduPsMean,meduPtdMean,meduSepMean,meduSocMean),2)

meduSdComb<-round(c(meduTdSd,meduAddSd,meduAgrSd,meduAnoSd,meduBulSd,meduConSd,meduDepSd,meduGadSd,meduManSd,meduOcdSd,meduOddSd,meduPanSd,meduPhbSd,meduPsSd,meduPtdSd,meduSepSd,meduSocSd),2)


#MAKE TABLE
dxNamesFull<-c("Typically Developing","ADHD","Agoraphobia","Anorexia","Bulimia","Conduct Disorder","Major Depression","GAD","Mania","OCD","ODD","Panic","Specific Phobias","Psychosis-spectrum","PTSD","Separation Anxiety","Social Phobia")
table1<-as.data.frame(matrix(nrow=17,ncol=5))
row.names(table1)<-dxNamesFull
colnames(table1)[1]<-"n"
colnames(table1)[2]<-"Mean (S.D.) Age"
colnames(table1)[3]<-"Female (%)"
colnames(table1)[4]<-"Caucasian (%)"
colnames(table1)[5]<-"Mean (S.D.) Years Maternal Education"
table1[,1]<-numComb
table1[,3]<-femComb
table1[,4]<-whiteComb

#combine age mean/sd
for (i in 1:17){
	table1[i,2]<-paste(ageMeanComb[i]," (",ageSdComb[i],") ", sep='')
        table1[i,5]<-paste(meduMeanComb[i]," (",meduSdComb[i],") ", sep='')
}
	
write.csv(table1,"/import/monstrum2/Users/antoniak/PNC_asl/tables_figures/n875_demographics_table1.csv",row.names=TRUE,quote=FALSE)
#To see table after running the script, type "table1" on the command line



######FIGURE S1: Trait and State Anxiety scores across categorical screening diagnoses
facTbl<-as.data.frame(matrix(nrow=17,ncol=4))
colnames(facTbl)[1]<-"TraitMean"
colnames(facTbl)[2]<-"TraitSem"
colnames(facTbl)[3]<-"StateMean"
colnames(facTbl)[4]<-"StateSem"

row.names(facTbl)<-dxNamesFull

dxs<-c("Td","Add","Agr","Ano","Bul","Con","Dep","Gad","Man","Ocd","Odd","Pan","Phb","Ps","Ptd","Sep","Soc")

for (i in 1:17){
	dx<-dxs[i]
	print(dx)
	y<-data.final[,dx]

        facTbl[i,1]<-mean(data.final$Trait_General_12[which(y==1)],na.rm=TRUE)
        facTbl[i,2]<-sd(data.final$Trait_General_12[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

	facTbl[i,3]<-mean(data.final$State_General_12[which(y==1)],na.rm=TRUE)
        facTbl[i,4]<-sd(data.final$State_General_12[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

}


#reshape to long format
library(reshape2)
facTbl$group<-as.factor(dxs)
facTblSem<-facTbl[,c(2,4,5)]
facTblMean<-facTbl[,c(1,3,5)]
facTblMeanLong<-melt(facTblMean,id.vars="group",variable.name="factor",value.name="meanScore")
facTblSemLong<-melt(facTblSem,id.vars="group",variable.name="factor",value.name="semScore")
facTblLong<-facTblMeanLong
facTblLong$semScore<-facTblSemLong$semScore

#remove those with less than 20 subjects (Anorexia, Bulimia, GAD, mania, Panic) 
facTblLong<-facTblLong[-c(grep("Bul",facTblLong$group),grep("Ano",facTblLong$group),grep("Gad",facTblLong$group),grep("Man",facTblLong$group),grep("Pan",facTblLong$group)),]
facTblLong$group<-factor(facTblLong$group) #remove empty factors

#plot w/ ggplot 
library(ggplot2)
library(grid)
#need to copy and paste this on the command line to call figure:
ggplot(facTblLong, aes(x=group, y=meanScore,fill=factor)) + ylab("Factor Score (z)") + xlab("") + ggtitle("State and Trait Anxiety Score by Screening Diagnosis") +
	geom_bar(stat="identity",position=position_dodge(), color="black",size=0.3) + 
	scale_fill_manual(values=c("#0000FF","#006400"), breaks=c("TraitMean", "StateMean"), labels=c("Trait Anxiety", "State Anxiety")) + 
	theme(axis.title.y = element_text(size = rel(1.5), angle = 90)) + theme(legend.justification=c(1,1), legend.position=c(1,1)) + 
	theme(plot.title = element_text(size = rel(1.5), vjust = 2)) + theme(axis.text.x = element_text(size = rel(1.25), colour="black", angle = 315, hjust = .05)) +
	theme(axis.text.y = element_text(size = rel(1.25), colour="black")) + guides(fill=guide_legend(title=NULL)) +
	scale_x_discrete(breaks=c("Add","Agr","Con","Dep","Ocd","Odd","Phb","Ps","Ptd","Sep","Soc","Td"), labels=c("ADHD","Agoraphobia","Conduct Disorder","MDD","OCD","ODD",
	"Phobia","Psychosis","PTSD","Separation Anxiety","Social Anxiety","Typically Developing")) + theme(plot.margin = unit(c(1,2.5,1,0.5), "cm")) +
	geom_errorbar(aes(ymin=meanScore-semScore, ymax=meanScore+semScore),  width=0.2, position=position_dodge(.9)) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

#save plot
ggsave(file="/import/monstrum2/Users/antoniak/PNC_asl/tables_figures/Figure1_TraitStateByDiagnosis.png", dpi = 300)




##helpful ggplot guides:
#ylab- y label title
#xlab- x label title
#ggtitle- plot title
#geom_bar - make bar plot
#geom_errorbar - create error-bars on plot
#scale_fill_manual- manually change the colors and names of the legend labels
#Within theme(axis.title.y = element_text()) or theme(axis.title.x = element_text()) or theme(plot.title = element_text()) you can change:
	#size = size of axis title
	#angle = orientation of text
	#vjust = vertical adjustment of text from axes
	#hjust = horizontal adjustment of text from axes
	#color = change color of text
#Above also works for axis tick labels using theme(axis.text.y=element_text()) or theme(axis.text.x=element_text())
#guides(fill=guide_legend(title=NULL)) - remove legend title
#scale_x_discrete(breaks=c("",""), labels=c("","")) - take the pre-existing axis labels and give them new labels
#theme(plot.margin = unit(c(1,2.5,1,0.5), "cm")) - change the margins of the plot to add a little extra room around the entire figure. Note: this requires the function "unit" from the package "grid" to work. unit(c(top,right,bottom,left))