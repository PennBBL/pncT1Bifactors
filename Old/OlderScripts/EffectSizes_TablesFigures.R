######################
#### READ IN DATA ####
######################

subjData<-readRDS("/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1360_JLF_volCtGmd_subjData.rds")

#########################################
#### MAKE DIAGNOSIS FACTOR VARIABLES ####
#########################################

##Calculate means, sds, percentage of females, percentage of White, and Ns for all diagnoses. 

#ADHD
numAdd<-sum(subjData$Add, na.rm=TRUE)
ageAddMean<-mean(subjData$age[which(subjData$Add==1)],na.rm=T)
ageAddSd<-sd(subjData$age[which(subjData$Add==1)],na.rm=T)
femAdd<-length(which(subjData$sex=="female" & subjData$Add==1))/numAdd
whiteAdd<-(length(which(subjData$race==1 & subjData$Add==1)))/numAdd
meduAddMean<-mean(subjData$meduCnbGo1[which(subjData$Add==1)],na.rm=T)
meduAddSd<-sd(subjData$meduCnbGo1[which(subjData$Add==1)],na.rm=T)

#Agoraphobia
numAgr<-sum(subjData$Agr, na.rm=TRUE)
ageAgrMean<-mean(subjData$age[which(subjData$Agr==1)],na.rm=T)
ageAgrSd<-sd(subjData$age[which(subjData$Agr==1)],na.rm=T)
femAgr<-length(which(subjData$sex=="female" & subjData$Agr==1))/numAgr
whiteAgr<-(length(which(subjData$race==1 & subjData$Agr==1)))/numAgr
meduAgrMean<-mean(subjData$meduCnbGo1[which(subjData$Agr==1)],na.rm=T)
meduAgrSd<-sd(subjData$meduCnbGo1[which(subjData$Agr==1)],na.rm=T)

#Anorexia
numAno<-sum(subjData$Ano, na.rm=TRUE)
ageAnoMean<-mean(subjData$age[which(subjData$Ano==1)],na.rm=T)
ageAnoSd<-sd(subjData$age[which(subjData$Ano==1)],na.rm=T)
femAno<-length(which(subjData$sex=="female" & subjData$Ano==1))/numAno
whiteAno<-(length(which(subjData$race==1 & subjData$Ano==1)))/numAno
meduAnoMean<-mean(subjData$meduCnbGo1[which(subjData$Ano==1)],na.rm=T)
meduAnoSd<-sd(subjData$meduCnbGo1[which(subjData$Ano==1)],na.rm=T)

#Bulimia
numBul<-sum(subjData$Bul, na.rm=TRUE)
ageBulMean<-mean(subjData$age[which(subjData$Bul==1)],na.rm=T)
ageBulSd<-sd(subjData$age[which(subjData$Bul==1)],na.rm=T)
femBul<-length(which(subjData$sex=="female" & subjData$Bul==1))/numBul
whiteBul<-(length(which(subjData$race==1 & subjData$Bul==1)))/numBul
meduBulMean<-mean(subjData$meduCnbGo1[which(subjData$Bul==1)],na.rm=T)
meduBulSd<-sd(subjData$meduCnbGo1[which(subjData$Bul==1)],na.rm=T)

#Conduct Disorder
numCon<-sum(subjData$Con, na.rm=TRUE)
ageConMean<-mean(subjData$age[which(subjData$Con==1)],na.rm=T)
ageConSd<-sd(subjData$age[which(subjData$Con==1)],na.rm=T)
femCon<-length(which(subjData$sex=="female" & subjData$Con==1))/numCon
whiteCon<-(length(which(subjData$race==1 & subjData$Con==1)))/numCon
meduConMean<-mean(subjData$meduCnbGo1[which(subjData$Con==1)],na.rm=T)
meduConSd<-sd(subjData$meduCnbGo1[which(subjData$Con==1)],na.rm=T)

#Generalized Anxiety Disorder
numGad<-sum(subjData$Gad, na.rm=TRUE)
ageGadMean<-mean(subjData$age[which(subjData$Gad==1)],na.rm=T)
ageGadSd<-sd(subjData$age[which(subjData$Gad==1)],na.rm=T)
femGad<-length(which(subjData$sex=="female" & subjData$Gad==1))/numGad
whiteGad<-(length(which(subjData$race==1 & subjData$Gad==1)))/numGad
meduGadMean<-mean(subjData$meduCnbGo1[which(subjData$Gad==1)],na.rm=T)
meduGadSd<-sd(subjData$meduCnbGo1[which(subjData$Gad==1)],na.rm=T)

#Major Depressive Disorder
numMdd<-sum(subjData$Mdd, na.rm=TRUE)
ageMddMean<-mean(subjData$age[which(subjData$Mdd==1)],na.rm=T)
ageMddSd<-sd(subjData$age[which(subjData$Mdd==1)],na.rm=T)
femMdd<-length(which(subjData$sex=="female" & subjData$Mdd==1))/numMdd
whiteMdd<-(length(which(subjData$race==1 & subjData$Mdd==1)))/numMdd
meduMddMean<-mean(subjData$meduCnbGo1[which(subjData$Mdd==1)],na.rm=T)
meduMddSd<-sd(subjData$meduCnbGo1[which(subjData$Mdd==1)],na.rm=T)

#Mania
numMan<-sum(subjData$Man, na.rm=TRUE)
ageManMean<-mean(subjData$age[which(subjData$Man==1)],na.rm=T)
ageManSd<-sd(subjData$age[which(subjData$Man==1)],na.rm=T)
femMan<-length(which(subjData$sex=="female" & subjData$Man==1))/numMan
whiteMan<-(length(which(subjData$race==1 & subjData$Man==1)))/numMan
meduManMean<-mean(subjData$meduCnbGo1[which(subjData$Man==1)],na.rm=T)
meduManSd<-sd(subjData$meduCnbGo1[which(subjData$Man==1)],na.rm=T)

#OCD
numOcd<-sum(subjData$Ocd, na.rm=TRUE)
ageOcdMean<-mean(subjData$age[which(subjData$Ocd==1)],na.rm=T)
ageOcdSd<-sd(subjData$age[which(subjData$Ocd==1)],na.rm=T)
femOcd<-length(which(subjData$sex=="female" & subjData$Ocd==1))/numOcd
whiteOcd<-(length(which(subjData$race==1 & subjData$Ocd==1)))/numOcd
meduOcdMean<-mean(subjData$meduCnbGo1[which(subjData$Ocd==1)],na.rm=T)
meduOcdSd<-sd(subjData$meduCnbGo1[which(subjData$Ocd==1)],na.rm=T)

#Oppositional Defiant Disorder
numOdd<-sum(subjData$Odd, na.rm=TRUE)
ageOddMean<-mean(subjData$age[which(subjData$Odd==1)],na.rm=T)
ageOddSd<-sd(subjData$age[which(subjData$Odd==1)],na.rm=T)
femOdd<-length(which(subjData$sex=="female" & subjData$Odd==1))/numOdd
whiteOdd<-(length(which(subjData$race==1 & subjData$Odd==1)))/numOdd
meduOddMean<-mean(subjData$meduCnbGo1[which(subjData$Odd==1)],na.rm=T)
meduOddSd<-sd(subjData$meduCnbGo1[which(subjData$Odd==1)],na.rm=T)

#Panic Disorder
numPan<-sum(subjData$Pan, na.rm=TRUE)
agePanMean<-mean(subjData$age[which(subjData$Pan==1)],na.rm=T)
agePanSd<-sd(subjData$age[which(subjData$Pan==1)],na.rm=T)
femPan<-length(which(subjData$sex=="female" & subjData$Pan==1))/numPan
whitePan<-(length(which(subjData$race==1 & subjData$Pan==1)))/numPan
meduPanMean<-mean(subjData$meduCnbGo1[which(subjData$Pan==1)],na.rm=T)
meduPanSd<-sd(subjData$meduCnbGo1[which(subjData$Pan==1)],na.rm=T)

#Psychosis
numPs<-sum(subjData$Ps, na.rm=TRUE)
agePsMean<-mean(subjData$age[which(subjData$Ps==1)],na.rm=T)
agePsSd<-sd(subjData$age[which(subjData$Ps==1)],na.rm=T)
femPs<-length(which(subjData$sex=="female" & subjData$Ps==1))/numPs
whitePs<-(length(which(subjData$race==1 & subjData$Ps==1)))/numPs
meduPsMean<-mean(subjData$meduCnbGo1[which(subjData$Ps==1)],na.rm=T)
meduPsSd<-sd(subjData$meduCnbGo1[which(subjData$Ps==1)],na.rm=T)

#Posttraumatic Stress Disorder
numPtd<-sum(subjData$Ptd, na.rm=TRUE)
agePtdMean<-mean(subjData$age[which(subjData$Ptd==1)],na.rm=T)
agePtdSd<-sd(subjData$age[which(subjData$Ptd==1)],na.rm=T)
femPtd<-length(which(subjData$sex=="female" & subjData$Ptd==1))/numPtd
whitePtd<-(length(which(subjData$race==1 & subjData$Ptd==1)))/numPtd
meduPtdMean<-mean(subjData$meduCnbGo1[which(subjData$Ptd==1)],na.rm=T)
meduPtdSd<-sd(subjData$meduCnbGo1[which(subjData$Ptd==1)],na.rm=T)

#Separation Anxiety Disorder
numSep<-sum(subjData$Sep, na.rm=TRUE)
ageSepMean<-mean(subjData$age[which(subjData$Sep==1)],na.rm=T)
ageSepSd<-sd(subjData$age[which(subjData$Sep==1)],na.rm=T)
femSep<-length(which(subjData$sex=="female" & subjData$Sep==1))/numSep
whiteSep<-(length(which(subjData$race==1 & subjData$Sep==1)))/numSep
meduSepMean<-mean(subjData$meduCnbGo1[which(subjData$Sep==1)],na.rm=T)
meduSepSd<-sd(subjData$meduCnbGo1[which(subjData$Sep==1)],na.rm=T)

#Social Anxiety Disorder
numSoc<-sum(subjData$Soc, na.rm=TRUE)
ageSocMean<-mean(subjData$age[which(subjData$Soc==1)],na.rm=T)
ageSocSd<-sd(subjData$age[which(subjData$Soc==1)],na.rm=T)
femSoc<-length(which(subjData$sex=="female" & subjData$Soc==1))/numSoc
whiteSoc<-(length(which(subjData$race==1 & subjData$Soc==1)))/numSoc
meduSocMean<-mean(subjData$meduCnbGo1[which(subjData$Soc==1)],na.rm=T)
meduSocSd<-sd(subjData$meduCnbGo1[which(subjData$Soc==1)],na.rm=T)

#Specific Phobia
numSph<-sum(subjData$Sph, na.rm=TRUE)
ageSphMean<-mean(subjData$age[which(subjData$Sph==1)],na.rm=T)
ageSphSd<-sd(subjData$age[which(subjData$Sph==1)],na.rm=T)
femSph<-length(which(subjData$sex=="female" & subjData$Sph==1))/numSph
whiteSph<-(length(which(subjData$race==1 & subjData$Sph==1)))/numSph
meduSphMean<-mean(subjData$meduCnbGo1[which(subjData$Sph==1)],na.rm=T)
meduSphSd<-sd(subjData$meduCnbGo1[which(subjData$Sph==1)],na.rm=T)

#Typically Developing
numTd<-sum(subjData$Td, na.rm=TRUE)
ageTdMean<-mean(subjData$age[which(subjData$Td==1)],na.rm=T)
ageTdSd<-sd(subjData$age[which(subjData$Td==1)],na.rm=T)
femTd<-length(which(subjData$sex=="female" & subjData$Td==1))/numTd
whiteTd<-(length(which(subjData$race==1 & subjData$Td==1)))/numTd
meduTdMean<-mean(subjData$meduCnbGo1[which(subjData$Td==1)],na.rm=T)
meduTdSd<-sd(subjData$meduCnbGo1[which(subjData$Td==1)],na.rm=T)



###############################
#### TABLE 1: DEMOGRAPHICS ####
###############################

#Combine variables
dxNames<-c("bblid","Td","Add","Agr","Ano","Bul","Con","Gad","Man","Mdd","Ocd","Odd","Pan","Ps","Ptd","Sep","Soc","Sph")
dxDf<-subjData[,dxNames]

numComb<-c(numTd,numAdd,numAgr,numAno,numBul,numCon,numGad,numMdd,numMan,numOcd,numOdd,numPan,numPs,numPtd,numSep,numSoc,numSph)

ageMeanComb<-round(c(ageTdMean,ageAddMean,ageAgrMean,ageAnoMean,ageBulMean,ageConMean,ageGadMean,ageMddMean,ageManMean,ageOcdMean,ageOddMean,agePanMean,agePsMean,agePtdMean,ageSepMean,ageSocMean,ageSphMean),2)

ageSdComb<-round(c(ageTdSd,ageAddSd,ageAgrSd,ageAnoSd,ageBulSd,ageConSd,ageGadSd,ageMddSd,ageManSd,ageOcdSd,ageOddSd,agePanSd,agePsSd,agePtdSd,ageSepSd,ageSocSd,ageSphSd),2)

femComb<-round(c(femTd,femAdd,femAgr,femAno,femBul,femCon,femGad,femMdd,femMan,femOcd,femOdd,femPan,femPs,femPtd,femSep,femSoc,femSph),3)*100

whiteComb<-round(c(whiteTd,whiteAdd,whiteAgr,whiteAno,whiteBul,whiteCon,whiteGad,whiteMdd,whiteMan,whiteOcd,whiteOdd,whitePan,whitePs,whitePtd,whiteSep,whiteSoc,whiteSph),3)*100

meduMeanComb<-round(c(meduTdMean,meduAddMean,meduAgrMean,meduAnoMean,meduBulMean,meduConMean,meduGadMean,meduMddMean,meduManMean,meduOcdMean,meduOddMean,meduPanMean,meduPsMean,meduPtdMean,meduSepMean,meduSocMean,meduSphMean),2)

meduSdComb<-round(c(meduTdSd,meduAddSd,meduAgrSd,meduAnoSd,meduBulSd,meduConSd,meduGadSd,meduMddSd,meduManSd,meduOcdSd,meduOddSd,meduPanSd,meduPsSd,meduPtdSd,meduSepSd,meduSocSd,meduSphSd),2)

#MAKE TABLE
dxNamesFull<-c("Typically Developing","ADHD","Agoraphobia","Anorexia","Bulimia","Conduct Disorder","Generalized Anxiety Disorder","Major Depression","Mania","Obsessive-Compulsive Disorder","Oppositional Defiant Disorder","Panic","Psychosis-spectrum","PTSD","Separation Anxiety","Social Phobia","Specific Phobia")
table1<-as.data.frame(matrix(nrow=17,ncol=7))
row.names(table1)<-dxNamesFull
colnames(table1)[1]<-"N"
colnames(table1)[2]<-"Female (%)"
colnames(table1)[3]<-"Caucasian (%)"
colnames(table1)[4]<-"Mean Age"
colnames(table1)[5]<-"SD Age"
colnames(table1)[6]<-"Mean Maternal Education (Years)"
colnames(table1)[7]<-"SD Maternal Education"
table1[,1]<-numComb
table1[,2]<-femComb
table1[,3]<-whiteComb
table1[,4]<-ageMeanComb
table1[,5]<-ageSdComb
table1[,6]<-meduMeanComb
table1[,7]<-meduSdComb

#SAVE TABLE
write.csv(table1,"/data/joy/BBL/projects/pncT1AcrossDisorder/TablesFigures/Table1.csv",row.names=TRUE,quote=FALSE)


###################################################
#### FIGURE 1: BIFACTORS BY SCREENING CATEGORY ####
###################################################

#Create table with bifactor means and sds only for diagnoses with >20 subjects.
facTbl<-as.data.frame(matrix(nrow=13,ncol=10))
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

#Name the rows
dxNamesShort<-c("ADHD","Agoraphobia","Conduct","GAD","MDD","OCD","ODD","Psychosis","PTSD","Separation Anxiety","Social Phobia","Specific Phobia","TD")

row.names(facTbl)<-dxNamesShort

dxs<-c("Add","Agr","Con","Gad","Mdd","Ocd","Odd","Ps","Ptd","Sep","Soc","Sph","Td")

#Calculate means and standard deviations
for (i in 1:13){
        dx<-dxs[i]
        print(dx)
        y<-subjData[,dx]

        facTbl[i,1]<-mean(subjData$goassessItemBifactor4FactorOverallPsy[which(y==1)],na.rm=TRUE)
        facTbl[i,2]<-sd(subjData$goassessItemBifactor4FactorOverallPsy[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

        facTbl[i,3]<-mean(subjData$goassessItemBifactor4FactorMood[which(y==1)],na.rm=TRUE)
        facTbl[i,4]<-sd(subjData$goassessItemBifactor4FactorMood[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

        facTbl[i,5]<-mean(subjData$goassessItemBifactor4FactorPsych[which(y==1)],na.rm=TRUE)
        facTbl[i,6]<-sd(subjData$goassessItemBifactor4FactorPsych[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

        facTbl[i,7]<-mean(subjData$goassessItemBifactor4FactorExt[which(y==1)],na.rm=TRUE)
        facTbl[i,8]<-sd(subjData$goassessItemBifactor4FactorExt[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

        facTbl[i,9]<-mean(subjData$goassessItemBifactor4FactorPhb[which(y==1)],na.rm=TRUE)
        facTbl[i,10]<-sd(subjData$goassessItemBifactor4FactorPhb[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

}


#Reshape to long format
library(reshape2)
facTbl$group<-as.factor(dxs)
facTblSem<-facTbl[,c(2,4,6,8,10,11)]
facTblMean<-facTbl[,c(1,3,5,7,9,11)]
facTblMeanLong<-melt(facTblMean,id.vars="group",variable.name="factor",value.name="meanScore")
facTblSemLong<-melt(facTblSem,id.vars="group",variable.name="factor",value.name="semScore")
facTblLong<-facTblMeanLong
facTblLong$semScore<-facTblSemLong$semScore

#Remove empty factors
facTblLong$group<-factor(facTblLong$group)


#Plot w/ ggplot
library(ggplot2)
library(grid)

#Colors used: #329444 = green (OverallPsych), #325194 = blue (Anxious-Misery), #943282 = purple (Psychosis), #B3141C = red (Behavioral), #F58311 = orange (Fear)

Fig1<-ggplot(facTblLong, aes(x=group, y=meanScore,fill=factor)) +
        ylab("Factor Score (z)") + xlab("") + ggtitle("Orthogonal Dimensions of Psychopathology by Screening Diagnosis") +
	geom_bar(stat="identity",position=position_dodge()) +
        scale_fill_manual(values=c("#329444","#325194","#943282","#B3141C","#F58311"), breaks=c("OverallMean","MoodMean","PsychosisMean","ExternalizingMean","PhobiasMean"),
	labels=c("Overall Psychopathology", "Anxious-Misery", "Psychosis", "Behavioral", "Fear")) +
	theme(axis.title.y = element_text(size = rel(1.5), angle = 90)) + theme(legend.text = element_text(size = 10), legend.justification=c(0.5,0.5), legend.position=c(.9,.9)) +
	theme(plot.title = element_text(size = rel(1.5), vjust = 2)) + theme(axis.text.x = element_text(size = rel(1.25), colour="black")) +
	theme(axis.text.y = element_text(size = rel(1.25), colour="black")) + guides(fill=guide_legend(title=NULL)) +
	scale_x_discrete(breaks=c("Add","Agr","Con","Gad","Mdd","Ocd","Odd","Ps","Ptd","Sep","Soc","Sph","Td"), labels=c("ADHD","Agoraphobia","Conduct","GAD","MDD","OCD","ODD",
	"Psychosis","PTSD","Separation Anx","Social Anx","Specific Phobia","TD")) + theme(plot.margin = unit(c(1,2.5,1,0.5), "cm")) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), 
	axis.line.y = element_line(colour = "black"))

#see colors used in plot (leave "data" specified as is)
Plot1<-ggplot_build(Fig1)$data

ggsave(file="/data/joy/BBL/projects/pncT1AcrossDisorder/TablesFigures/Figure1_BifactorsByDiag.png", width = 15, height = 5, units = "in", dpi = 300)


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
#geom_bar(size=0.3) = changes the thickness of the black line around each bar (1 = thicker, .03 = thinner)
#geom_bar(color="black") = changes the color of the black line around each bar (remove this command to remove the black outlines).
#legend.justification = defines which side of the legend that the legend.position coordinates refer to (can use left, right, centre or numeric value (x = between 0 and 1))
#legend.position = move the legend (accepts left, right, top, bottom or define relative coordinates on plot c(x, y) between 0 and 1) 



###########################################
#### CREATE SUBSETS FOR EACH DIAGNOSIS ####
###########################################

#subset the data for each diagnosis with N>20
AddData<-subjData[!is.na(subjData$Add), ]
AgrData<-subjData[!is.na(subjData$Agr), ]
ConData<-subjData[!is.na(subjData$Con), ]
GadData<-subjData[!is.na(subjData$Gad), ]
MddData<-subjData[!is.na(subjData$Mdd), ]
OcdData<-subjData[!is.na(subjData$Ocd), ]
OddData<-subjData[!is.na(subjData$Odd), ]
PsData<-subjData[!is.na(subjData$Ps), ]
PtdData<-subjData[!is.na(subjData$Ptd), ]
SepData<-subjData[!is.na(subjData$Sep), ]
SocData<-subjData[!is.na(subjData$Soc), ]
SphData<-subjData[!is.na(subjData$Sph), ]


#########################
#### LS MEANS VOLUME ####
#########################

#Load library; If not installed, then use this command: install.packages("lsmeans", repos="http://R-Forge.R-project.org")
library(lsmeans)

#Use lsmeans to save the adjusted mean for each disorder

#First, define lists
Vol_GmWmCsfList <- names(subjData)[2342:2344]
Vol_Lobe_List <- names(subjData)[2836:2840]

dataVol<-subjData[,grep("mprage_jlf_vol",names(subjData))]
dataVolGm<-dataVol[,-(unique(c( grep("Vent",names(dataVol)), grep("White",names(dataVol)), grep("DC",names(dataVol)),
        grep("Brain_Stem",names(dataVol)), grep("CSF",names(dataVol)), grep("Vessel",names(dataVol)), grep("OpticChiasm",names(dataVol)) )))]
Vol_ROI_List <- colnames(dataVolGm)


###ADD###

#Run models for GM/WM/CSF
require(lsmeans)
Add_GmWmModels_vol <- lapply(Vol_GmWmCsfList, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Add, list(i = as.name(x))), data = AddData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Add_GmWmLSMeans_vol<-sapply(Add_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Add))$lsmean } )
Add_GmWmSEs_vol<-sapply(Add_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Add))$SE } )

#Run models for Lobes
require(lsmeans)
Add_LobeModels_vol <- lapply(Vol_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Add, list(i = as.name(x))), data = AddData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Add_LobeLSMeans_vol<-sapply(Add_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Add))$lsmean } )
Add_LobeSEs_vol<-sapply(Add_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Add))$SE } )

#Run models for ROIs
require(lsmeans)
Add_ROIModels_vol <- lapply(Vol_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Add, list(i = as.name(x))), data = AddData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Add_ROILSMeans_vol<-sapply(Add_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Add))$lsmean } )
Add_ROISEs_vol<-sapply(Add_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Add))$SE } )


###Agr###

#Run models for GM/WM/CSF
require(lsmeans)
Agr_GmWmModels_vol <- lapply(Vol_GmWmCsfList, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Agr, list(i = as.name(x))), data = AgrData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Agr_GmWmLSMeans_vol<-sapply(Agr_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Agr))$lsmean } )
Agr_GmWmSEs_vol<-sapply(Agr_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Agr))$SE } )

#Run models for Lobes
require(lsmeans)
Agr_LobeModels_vol <- lapply(Vol_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Agr, list(i = as.name(x))), data = AgrData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Agr_LobeLSMeans_vol<-sapply(Agr_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Agr))$lsmean } )
Agr_LobeSEs_vol<-sapply(Agr_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Agr))$SE } )

#Run models for ROIs
require(lsmeans)
Agr_ROIModels_vol <- lapply(Vol_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Agr, list(i = as.name(x))), data = AgrData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Agr_ROILSMeans_vol<-sapply(Agr_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Agr))$lsmean } )
Agr_ROISEs_vol<-sapply(Agr_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Agr))$SE } )


###CON###

#Run models for GM/WM/CSF
require(lsmeans)
Con_GmWmModels_vol <- lapply(Vol_GmWmCsfList, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Con, list(i = as.name(x))), data = ConData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Con_GmWmLSMeans_vol<-sapply(Con_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Con))$lsmean } )
Con_GmWmSEs_vol<-sapply(Con_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Con))$SE } )

#Run models for Lobes
require(lsmeans)
Con_LobeModels_vol <- lapply(Vol_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Con, list(i = as.name(x))), data = ConData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Con_LobeLSMeans_vol<-sapply(Con_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Con))$lsmean } )
Con_LobeSEs_vol<-sapply(Con_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Con))$SE } )

#Run models for ROIs
require(lsmeans)
Con_ROIModels_vol <- lapply(Vol_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Con, list(i = as.name(x))), data = ConData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Con_ROILSMeans_vol<-sapply(Con_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Con))$lsmean } )
Con_ROISEs_vol<-sapply(Con_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Con))$SE } )


###GAD###

#Run models for GM/WM/CSF
require(lsmeans)
Gad_GmWmModels_vol <- lapply(Vol_GmWmCsfList, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Gad, list(i = as.name(x))), data = GadData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Gad_GmWmLSMeans_vol<-sapply(Gad_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Gad))$lsmean } )
Gad_GmWmSEs_vol<-sapply(Gad_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Gad))$SE } )

#Run models for Lobes
require(lsmeans)
Gad_LobeModels_vol <- lapply(Vol_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Gad, list(i = as.name(x))), data = GadData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Gad_LobeLSMeans_vol<-sapply(Gad_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Gad))$lsmean } )
Gad_LobeSEs_vol<-sapply(Gad_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Gad))$SE } )

#Run models for ROIs
require(lsmeans)
Gad_ROIModels_vol <- lapply(Vol_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Gad, list(i = as.name(x))), data = GadData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Gad_ROILSMeans_vol<-sapply(Gad_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Gad))$lsmean } )
Gad_ROISEs_vol<-sapply(Gad_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Gad))$SE } )


###MDD###

#Run models for GM/WM/CSF
require(lsmeans)
Mdd_GmWmModels_vol <- lapply(Vol_GmWmCsfList, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Mdd, list(i = as.name(x))), data = MddData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Mdd_GmWmLSMeans_vol<-sapply(Mdd_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Mdd))$lsmean } )
Mdd_GmWmSEs_vol<-sapply(Mdd_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Mdd))$SE } )

#Run models for Lobes
require(lsmeans)
Mdd_LobeModels_vol <- lapply(Vol_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Mdd, list(i = as.name(x))), data = MddData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Mdd_LobeLSMeans_vol<-sapply(Mdd_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Mdd))$lsmean } )
Mdd_LobeSEs_vol<-sapply(Mdd_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Mdd))$SE } )

#Run models for ROIs
require(lsmeans)
Mdd_ROIModels_vol <- lapply(Vol_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Mdd, list(i = as.name(x))), data = MddData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Mdd_ROILSMeans_vol<-sapply(Mdd_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Mdd))$lsmean } )
Mdd_ROISEs_vol<-sapply(Mdd_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Mdd))$SE } )


###OCD###

#Run models for GM/WM/CSF
require(lsmeans)
Ocd_GmWmModels_vol <- lapply(Vol_GmWmCsfList, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Ocd, list(i = as.name(x))), data = OcdData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Ocd_GmWmLSMeans_vol<-sapply(Ocd_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Ocd))$lsmean } )
Ocd_GmWmSEs_vol<-sapply(Ocd_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Ocd))$SE } )

#Run models for Lobes
require(lsmeans)
Ocd_LobeModels_vol <- lapply(Vol_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Ocd, list(i = as.name(x))), data = OcdData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Ocd_LobeLSMeans_vol<-sapply(Ocd_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Ocd))$lsmean } )
Ocd_LobeSEs_vol<-sapply(Ocd_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Ocd))$SE } )

#Run models for ROIs
require(lsmeans)
Ocd_ROIModels_vol <- lapply(Vol_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Ocd, list(i = as.name(x))), data = OcdData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Ocd_ROILSMeans_vol<-sapply(Ocd_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Ocd))$lsmean } )
Ocd_ROISEs_vol<-sapply(Ocd_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Ocd))$SE } )


###ODD###

#Run models for GM/WM/CSF
require(lsmeans)
Odd_GmWmModels_vol <- lapply(Vol_GmWmCsfList, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Odd, list(i = as.name(x))), data = OddData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Odd_GmWmLSMeans_vol<-sapply(Odd_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Odd))$lsmean } )
Odd_GmWmSEs_vol<-sapply(Odd_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Odd))$SE } )

#Run models for Lobes
require(lsmeans)
Odd_LobeModels_vol <- lapply(Vol_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Odd, list(i = as.name(x))), data = OddData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Odd_LobeLSMeans_vol<-sapply(Odd_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Odd))$lsmean } )
Odd_LobeSEs_vol<-sapply(Odd_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Odd))$SE } )

#Run models for ROIs
require(lsmeans)
Odd_ROIModels_vol <- lapply(Vol_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Odd, list(i = as.name(x))), data = OddData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Odd_ROILSMeans_vol<-sapply(Odd_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Odd))$lsmean } )
Odd_ROISEs_vol<-sapply(Odd_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Odd))$SE } )


###PS###

#Run models for GM/WM/CSF
require(lsmeans)
Ps_GmWmModels_vol <- lapply(Vol_GmWmCsfList, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Ps, list(i = as.name(x))), data = PsData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Ps_GmWmLSMeans_vol<-sapply(Ps_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Ps))$lsmean } )
Ps_GmWmSEs_vol<-sapply(Ps_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Ps))$SE } )

#Run models for Lobes
require(lsmeans)
Ps_LobeModels_vol <- lapply(Vol_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Ps, list(i = as.name(x))), data = PsData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Ps_LobeLSMeans_vol<-sapply(Ps_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Ps))$lsmean } )
Ps_LobeSEs_vol<-sapply(Ps_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Ps))$SE } )

#Run models for ROIs
require(lsmeans)
Ps_ROIModels_vol <- lapply(Vol_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Ps, list(i = as.name(x))), data = PsData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Ps_ROILSMeans_vol<-sapply(Ps_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Ps))$lsmean } )
Ps_ROISEs_vol<-sapply(Ps_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Ps))$SE } )


###PTD###

#Run models for GM/WM/CSF
require(lsmeans)
Ptd_GmWmModels_vol <- lapply(Vol_GmWmCsfList, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Ptd, list(i = as.name(x))), data = PtdData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Ptd_GmWmLSMeans_vol<-sapply(Ptd_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Ptd))$lsmean } )
Ptd_GmWmSEs_vol<-sapply(Ptd_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Ptd))$SE } )

#Run models for Lobes
require(lsmeans)
Ptd_LobeModels_vol <- lapply(Vol_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Ptd, list(i = as.name(x))), data = PtdData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Ptd_LobeLSMeans_vol<-sapply(Ptd_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Ptd))$lsmean } )
Ptd_LobeSEs_vol<-sapply(Ptd_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Ptd))$SE } )

#Run models for ROIs
require(lsmeans)
Ptd_ROIModels_vol <- lapply(Vol_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Ptd, list(i = as.name(x))), data = PtdData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Ptd_ROILSMeans_vol<-sapply(Ptd_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Ptd))$lsmean } )
Ptd_ROISEs_vol<-sapply(Ptd_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Ptd))$SE } )


###Sep###

#Run models for GM/WM/CSF
require(lsmeans)
Sep_GmWmModels_vol <- lapply(Vol_GmWmCsfList, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Sep, list(i = as.name(x))), data = SepData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Sep_GmWmLSMeans_vol<-sapply(Sep_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Sep))$lsmean } )
Sep_GmWmSEs_vol<-sapply(Sep_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Sep))$SE } )

#Run models for Lobes
require(lsmeans)
Sep_LobeModels_vol <- lapply(Vol_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Sep, list(i = as.name(x))), data = SepData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Sep_LobeLSMeans_vol<-sapply(Sep_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Sep))$lsmean } )
Sep_LobeSEs_vol<-sapply(Sep_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Sep))$SE } )

#Run models for ROIs
require(lsmeans)
Sep_ROIModels_vol <- lapply(Vol_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Sep, list(i = as.name(x))), data = SepData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Sep_ROILSMeans_vol<-sapply(Sep_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Sep))$lsmean } )
Sep_ROISEs_vol<-sapply(Sep_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Sep))$SE } )


###SOC###

#Run models for GM/WM/CSF
require(lsmeans)
Soc_GmWmModels_vol <- lapply(Vol_GmWmCsfList, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Soc, list(i = as.name(x))), data = SocData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Soc_GmWmLSMeans_vol<-sapply(Soc_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Soc))$lsmean } )
Soc_GmWmSEs_vol<-sapply(Soc_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Soc))$SE } )

#Run models for Lobes
require(lsmeans)
Soc_LobeModels_vol <- lapply(Vol_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Soc, list(i = as.name(x))), data = SocData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Soc_LobeLSMeans_vol<-sapply(Soc_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Soc))$lsmean } )
Soc_LobeSEs_vol<-sapply(Soc_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Soc))$SE } )

#Run models for ROIs
require(lsmeans)
Soc_ROIModels_vol <- lapply(Vol_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Soc, list(i = as.name(x))), data = SocData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Soc_ROILSMeans_vol<-sapply(Soc_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Soc))$lsmean } )
Soc_ROISEs_vol<-sapply(Soc_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Soc))$SE } )


###SPH###

#Run models for GM/WM/CSF
require(lsmeans)
Sph_GmWmModels_vol <- lapply(Vol_GmWmCsfList, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Sph, list(i = as.name(x))), data = SphData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Sph_GmWmLSMeans_vol<-sapply(Sph_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Sph))$lsmean } )
Sph_GmWmSEs_vol<-sapply(Sph_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Sph))$SE } )

#Run models for Lobes
require(lsmeans)
Sph_LobeModels_vol <- lapply(Vol_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Sph, list(i = as.name(x))), data = SphData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Sph_LobeLSMeans_vol<-sapply(Sph_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Sph))$lsmean } )
Sph_LobeSEs_vol<-sapply(Sph_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Sph))$SE } )

#Run models for ROIs
require(lsmeans)
Sph_ROIModels_vol <- lapply(Vol_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Sph, list(i = as.name(x))), data = SphData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Sph_ROILSMeans_vol<-sapply(Sph_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Sph))$lsmean } )
Sph_ROISEs_vol<-sapply(Sph_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Sph))$SE } )
Sph_LobeLSMeans_vol


###TD###

#Get lsmeans and SEs for TD vs all diagnoses (for plotting)

#Run models for GM/WM/CSF
require(lsmeans)
Td_GmWmModels_vol <- lapply(Vol_GmWmCsfList, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Td, list(i = as.name(x))), data = subjData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Td_GmWmLSMeans_vol<-sapply(Td_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Td))$lsmean } )
Td_GmWmSEs_vol<-sapply(Td_GmWmModels_vol, function(x) { f <- summary(lsmeans(x, ~ Td))$SE } )

#Run models for Lobes
require(lsmeans)
Td_LobeModels_vol <- lapply(Vol_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Td, list(i = as.name(x))), data = subjData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Td_LobeLSMeans_vol<-sapply(Td_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Td))$lsmean } )
Td_LobeSEs_vol<-sapply(Td_LobeModels_vol, function(x) { f <- summary(lsmeans(x, ~ Td))$SE } )

#Run models for ROIs
require(lsmeans)
Td_ROIModels_vol <- lapply(Vol_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Td, list(i = as.name(x))), data = subjData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Td_ROILSMeans_vol<-sapply(Td_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Td))$lsmean } )
Td_ROISEs_vol<-sapply(Td_ROIModels_vol, function(x) { f <- summary(lsmeans(x, ~ Td))$SE } )



######################
#### LS MEANS CT #####
######################

#Use lsmeans to save the adjusted mean for each disorder

#First, define lists
CT_Gm_List <- names(subjData)[2833]
CT_Lobe_List <- names(subjData)[2841:2844]
dataCTGm<-subjData[,grep("mprage_jlf_ct",names(subjData))]
CT_ROI_List <- colnames(dataCTGm)


###ADD###

#Run models for GM
require(lsmeans)
Add_GmModels_ct <- lapply(CT_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Add, list(i = as.name(x))), data = AddData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Add_GmLSMeans_ct<-sapply(Add_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Add))$lsmean } )
Add_GmSEs_ct<-sapply(Add_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Add))$SE } )

#Run models for Lobes
require(lsmeans)
Add_LobeModels_ct <- lapply(CT_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Add, list(i = as.name(x))), data = AddData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Add_LobeLSMeans_ct<-sapply(Add_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Add))$lsmean } )
Add_LobeSEs_ct<-sapply(Add_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Add))$SE } )

#Run models for ROIs
require(lsmeans)
Add_ROIModels_ct <- lapply(CT_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Add, list(i = as.name(x))), data = AddData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Add_ROILSMeans_ct<-sapply(Add_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Add))$lsmean } )
Add_ROISEs_ct<-sapply(Add_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Add))$SE } )


###Agr###

#Run models for GM
require(lsmeans)
Agr_GmModels_ct <- lapply(CT_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Agr, list(i = as.name(x))), data = AgrData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Agr_GmLSMeans_ct<-sapply(Agr_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Agr))$lsmean } )
Agr_GmSEs_ct<-sapply(Agr_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Agr))$SE } )

#Run models for Lobes
require(lsmeans)
Agr_LobeModels_ct <- lapply(CT_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Agr, list(i = as.name(x))), data = AgrData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Agr_LobeLSMeans_ct<-sapply(Agr_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Agr))$lsmean } )
Agr_LobeSEs_ct<-sapply(Agr_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Agr))$SE } )

#Run models for ROIs
require(lsmeans)
Agr_ROIModels_ct <- lapply(CT_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Agr, list(i = as.name(x))), data = AgrData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Agr_ROILSMeans_ct<-sapply(Agr_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Agr))$lsmean } )
Agr_ROISEs_ct<-sapply(Agr_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Agr))$SE } )


###CON###

#Run models for GM
require(lsmeans)
Con_GmModels_ct <- lapply(CT_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Con, list(i = as.name(x))), data = ConData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Con_GmLSMeans_ct<-sapply(Con_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Con))$lsmean } )
Con_GmSEs_ct<-sapply(Con_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Con))$SE } )

#Run models for Lobes
require(lsmeans)
Con_LobeModels_ct <- lapply(CT_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Con, list(i = as.name(x))), data = ConData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Con_LobeLSMeans_ct<-sapply(Con_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Con))$lsmean } )
Con_LobeSEs_ct<-sapply(Con_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Con))$SE } )

#Run models for ROIs
require(lsmeans)
Con_ROIModels_ct <- lapply(CT_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Con, list(i = as.name(x))), data = ConData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Con_ROILSMeans_ct<-sapply(Con_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Con))$lsmean } )
Con_ROISEs_ct<-sapply(Con_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Con))$SE } )


###GAD###

#Run models for GM
require(lsmeans)
Gad_GmModels_ct <- lapply(CT_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Gad, list(i = as.name(x))), data = GadData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Gad_GmLSMeans_ct<-sapply(Gad_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Gad))$lsmean } )
Gad_GmSEs_ct<-sapply(Gad_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Gad))$SE } )

#Run models for Lobes
require(lsmeans)
Gad_LobeModels_ct <- lapply(CT_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Gad, list(i = as.name(x))), data = GadData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Gad_LobeLSMeans_ct<-sapply(Gad_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Gad))$lsmean } )
Gad_LobeSEs_ct<-sapply(Gad_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Gad))$SE } )

#Run models for ROIs
require(lsmeans)
Gad_ROIModels_ct <- lapply(CT_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Gad, list(i = as.name(x))), data = GadData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Gad_ROILSMeans_ct<-sapply(Gad_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Gad))$lsmean } )
Gad_ROISEs_ct<-sapply(Gad_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Gad))$SE } )


###MDD###

#Run models for GM
require(lsmeans)
Mdd_GmModels_ct <- lapply(CT_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Mdd, list(i = as.name(x))), data = MddData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Mdd_GmLSMeans_ct<-sapply(Mdd_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Mdd))$lsmean } )
Mdd_GmSEs_ct<-sapply(Mdd_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Mdd))$SE } )

#Run models for Lobes
require(lsmeans)
Mdd_LobeModels_ct <- lapply(CT_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Mdd, list(i = as.name(x))), data = MddData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Mdd_LobeLSMeans_ct<-sapply(Mdd_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Mdd))$lsmean } )
Mdd_LobeSEs_ct<-sapply(Mdd_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Mdd))$SE } )

#Run models for ROIs
require(lsmeans)
Mdd_ROIModels_ct <- lapply(CT_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Mdd, list(i = as.name(x))), data = MddData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Mdd_ROILSMeans_ct<-sapply(Mdd_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Mdd))$lsmean } )
Mdd_ROISEs_ct<-sapply(Mdd_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Mdd))$SE } )


###OCD###

#Run models for GM
require(lsmeans)
Ocd_GmModels_ct <- lapply(CT_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Ocd, list(i = as.name(x))), data = OcdData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Ocd_GmLSMeans_ct<-sapply(Ocd_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Ocd))$lsmean } )
Ocd_GmSEs_ct<-sapply(Ocd_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Ocd))$SE } )

#Run models for Lobes
require(lsmeans)
Ocd_LobeModels_ct <- lapply(CT_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Ocd, list(i = as.name(x))), data = OcdData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Ocd_LobeLSMeans_ct<-sapply(Ocd_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Ocd))$lsmean } )
Ocd_LobeSEs_ct<-sapply(Ocd_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Ocd))$SE } )

#Run models for ROIs
require(lsmeans)
Ocd_ROIModels_ct <- lapply(CT_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Ocd, list(i = as.name(x))), data = OcdData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Ocd_ROILSMeans_ct<-sapply(Ocd_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Ocd))$lsmean } )
Ocd_ROISEs_ct<-sapply(Ocd_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Ocd))$SE } )


###ODD###

#Run models for GM
require(lsmeans)
Odd_GmModels_ct <- lapply(CT_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Odd, list(i = as.name(x))), data = OddData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Odd_GmLSMeans_ct<-sapply(Odd_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Odd))$lsmean } )
Odd_GmSEs_ct<-sapply(Odd_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Odd))$SE } )

#Run models for Lobes
require(lsmeans)
Odd_LobeModels_ct <- lapply(CT_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Odd, list(i = as.name(x))), data = OddData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Odd_LobeLSMeans_ct<-sapply(Odd_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Odd))$lsmean } )
Odd_LobeSEs_ct<-sapply(Odd_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Odd))$SE } )

#Run models for ROIs
require(lsmeans)
Odd_ROIModels_ct <- lapply(CT_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Odd, list(i = as.name(x))), data = OddData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Odd_ROILSMeans_ct<-sapply(Odd_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Odd))$lsmean } )
Odd_ROISEs_ct<-sapply(Odd_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Odd))$SE } )


###PS###

#Run models for GM
require(lsmeans)
Ps_GmModels_ct <- lapply(CT_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Ps, list(i = as.name(x))), data = PsData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Ps_GmLSMeans_ct<-sapply(Ps_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Ps))$lsmean } )
Ps_GmSEs_ct<-sapply(Ps_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Ps))$SE } )

#Run models for Lobes
require(lsmeans)
Ps_LobeModels_ct <- lapply(CT_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Ps, list(i = as.name(x))), data = PsData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Ps_LobeLSMeans_ct<-sapply(Ps_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Ps))$lsmean } )
Ps_LobeSEs_ct<-sapply(Ps_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Ps))$SE } )

#Run models for ROIs
require(lsmeans)
Ps_ROIModels_ct <- lapply(CT_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Ps, list(i = as.name(x))), data = PsData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Ps_ROILSMeans_ct<-sapply(Ps_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Ps))$lsmean } )
Ps_ROISEs_ct<-sapply(Ps_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Ps))$SE } )


###PTD###

#Run models for GM
require(lsmeans)
Ptd_GmModels_ct <- lapply(CT_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Ptd, list(i = as.name(x))), data = PtdData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Ptd_GmLSMeans_ct<-sapply(Ptd_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Ptd))$lsmean } )
Ptd_GmSEs_ct<-sapply(Ptd_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Ptd))$SE } )

#Run models for Lobes
require(lsmeans)
Ptd_LobeModels_ct <- lapply(CT_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Ptd, list(i = as.name(x))), data = PtdData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Ptd_LobeLSMeans_ct<-sapply(Ptd_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Ptd))$lsmean } )
Ptd_LobeSEs_ct<-sapply(Ptd_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Ptd))$SE } )

#Run models for ROIs
require(lsmeans)
Ptd_ROIModels_ct <- lapply(CT_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Ptd, list(i = as.name(x))), data = PtdData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Ptd_ROILSMeans_ct<-sapply(Ptd_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Ptd))$lsmean } )
Ptd_ROISEs_ct<-sapply(Ptd_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Ptd))$SE } )


###Sep###

#Run models for GM
require(lsmeans)
Sep_GmModels_ct <- lapply(CT_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Sep, list(i = as.name(x))), data = SepData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Sep_GmLSMeans_ct<-sapply(Sep_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Sep))$lsmean } )
Sep_GmSEs_ct<-sapply(Sep_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Sep))$SE } )

#Run models for Lobes
require(lsmeans)
Sep_LobeModels_ct <- lapply(CT_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Sep, list(i = as.name(x))), data = SepData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Sep_LobeLSMeans_ct<-sapply(Sep_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Sep))$lsmean } )
Sep_LobeSEs_ct<-sapply(Sep_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Sep))$SE } )

#Run models for ROIs
require(lsmeans)
Sep_ROIModels_ct <- lapply(CT_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Sep, list(i = as.name(x))), data = SepData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Sep_ROILSMeans_ct<-sapply(Sep_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Sep))$lsmean } )
Sep_ROISEs_ct<-sapply(Sep_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Sep))$SE } )


###SOC###

#Run models for GM
require(lsmeans)
Soc_GmModels_ct <- lapply(CT_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Soc, list(i = as.name(x))), data = SocData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Soc_GmLSMeans_ct<-sapply(Soc_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Soc))$lsmean } )
Soc_GmSEs_ct<-sapply(Soc_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Soc))$SE } )

#Run models for Lobes
require(lsmeans)
Soc_LobeModels_ct <- lapply(CT_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Soc, list(i = as.name(x))), data = SocData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Soc_LobeLSMeans_ct<-sapply(Soc_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Soc))$lsmean } )
Soc_LobeSEs_ct<-sapply(Soc_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Soc))$SE } )

#Run models for ROIs
require(lsmeans)
Soc_ROIModels_ct <- lapply(CT_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Soc, list(i = as.name(x))), data = SocData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Soc_ROILSMeans_ct<-sapply(Soc_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Soc))$lsmean } )
Soc_ROISEs_ct<-sapply(Soc_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Soc))$SE } )


###SPH###

#Run models for GM
require(lsmeans)
Sph_GmModels_ct <- lapply(CT_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Sph, list(i = as.name(x))), data = SphData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Sph_GmLSMeans_ct<-sapply(Sph_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Sph))$lsmean } )
Sph_GmSEs_ct<-sapply(Sph_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Sph))$SE } )

#Run models for Lobes
require(lsmeans)
Sph_LobeModels_ct <- lapply(CT_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Sph, list(i = as.name(x))), data = SphData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Sph_LobeLSMeans_ct<-sapply(Sph_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Sph))$lsmean } )
Sph_LobeSEs_ct<-sapply(Sph_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Sph))$SE } )

#Run models for ROIs
require(lsmeans)
Sph_ROIModels_ct <- lapply(CT_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Sph, list(i = as.name(x))), data = SphData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Sph_ROILSMeans_ct<-sapply(Sph_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Sph))$lsmean } )
Sph_ROISEs_ct<-sapply(Sph_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Sph))$SE } )


###TD###

#Get lsmeans and SEs for TD vs all diagnoses (for plotting)

#Run models for GM
require(lsmeans)
Td_GmModels_ct <- lapply(CT_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Td, list(i = as.name(x))), data = subjData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Td_GmLSMeans_ct<-sapply(Td_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Td))$lsmean } )
Td_GmSEs_ct<-sapply(Td_GmModels_ct, function(x) { f <- summary(lsmeans(x, ~ Td))$SE } )

#Run models for Lobes
require(lsmeans)
Td_LobeModels_ct <- lapply(CT_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Td, list(i = as.name(x))), data = subjData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Td_LobeLSMeans_ct<-sapply(Td_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Td))$lsmean } )
Td_LobeSEs_ct<-sapply(Td_LobeModels_ct, function(x) { f <- summary(lsmeans(x, ~ Td))$SE } )

#Run models for ROIs
require(lsmeans)
Td_ROIModels_ct <- lapply(CT_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + ageSq + mprage_antsCT_vol_TBV + Td, list(i = as.name(x))), data = subjData)
})

#Calculate and pull the lsmeans and SEs from each lm summary
Td_ROILSMeans_ct<-sapply(Td_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Td))$lsmean } )
Td_ROISEs_ct<-sapply(Td_ROIModels_ct, function(x) { f <- summary(lsmeans(x, ~ Td))$SE } )



#######################
### CT EFFECT SIZES ###
#######################

#Compute covariate-adjusted effect sizes (Cohens d)

#Define the effect size function (which controls for covariates)
###NOTE: the coefficient is hard coded at t$coefficients[6,1], if you change the number of variables in your model, this number will also have to be changed.
dCov<-function(x){
  t<-summary(x)
  effsize <- t$coefficients[6,1]/t$sigma
  return(abs(effsize))
}

#Double check that the effect size (estimate divided by residual standard error) is correctly calculated.
#test<-lm(CT_gmFrontalTotal~ age + sex + ageSq + mprage_antsCT_vol_TBV + Add, data = AddData)

#Calculate effect sizes:

#Add vs Td
Add_GmModels_ct_d<-lapply(Add_GmModels_ct, dCov)
Add_LobeModels_ct_d<-lapply(Add_LobeModels_ct, dCov)
Add_ROIModels_ct_d<-lapply(Add_ROIModels_ct, dCov)

#Agr vs Td
Agr_GmModels_ct_d<-lapply(Agr_GmModels_ct, dCov)
Agr_LobeModels_ct_d<-lapply(Agr_LobeModels_ct, dCov)
Agr_ROIModels_ct_d<-lapply(Agr_ROIModels_ct, dCov)

#Con vs Td
Con_GmModels_ct_d<-lapply(Con_GmModels_ct, dCov)
Con_LobeModels_ct_d<-lapply(Con_LobeModels_ct, dCov)
Con_ROIModels_ct_d<-lapply(Con_ROIModels_ct, dCov)

#Gad vs Td
Gad_GmModels_ct_d<-lapply(Gad_GmModels_ct, dCov)
Gad_LobeModels_ct_d<-lapply(Gad_LobeModels_ct, dCov)
Gad_ROIModels_ct_d<-lapply(Gad_ROIModels_ct, dCov)

#Mdd vs Td
Mdd_GmModels_ct_d<-lapply(Mdd_GmModels_ct, dCov)
Mdd_LobeModels_ct_d<-lapply(Mdd_LobeModels_ct, dCov)
Mdd_ROIModels_ct_d<-lapply(Mdd_ROIModels_ct, dCov)

#Ocd vs Td
Ocd_GmModels_ct_d<-lapply(Ocd_GmModels_ct, dCov)
Ocd_LobeModels_ct_d<-lapply(Ocd_LobeModels_ct, dCov)
Ocd_ROIModels_ct_d<-lapply(Ocd_ROIModels_ct, dCov)

#Odd vs Td
Odd_GmModels_ct_d<-lapply(Odd_GmModels_ct, dCov)
Odd_LobeModels_ct_d<-lapply(Odd_LobeModels_ct, dCov)
Odd_ROIModels_ct_d<-lapply(Odd_ROIModels_ct, dCov)

#Ps vs Td
Ps_GmModels_ct_d<-lapply(Ps_GmModels_ct, dCov)
Ps_LobeModels_ct_d<-lapply(Ps_LobeModels_ct, dCov)
Ps_ROIModels_ct_d<-lapply(Ps_ROIModels_ct, dCov)

#Ptd vs Td
Ptd_GmModels_ct_d<-lapply(Ptd_GmModels_ct, dCov)
Ptd_LobeModels_ct_d<-lapply(Ptd_LobeModels_ct, dCov)
Ptd_ROIModels_ct_d<-lapply(Ptd_ROIModels_ct, dCov)

#Sep vs Td
Sep_GmModels_ct_d<-lapply(Sep_GmModels_ct, dCov)
Sep_LobeModels_ct_d<-lapply(Sep_LobeModels_ct, dCov)
Sep_ROIModels_ct_d<-lapply(Sep_ROIModels_ct, dCov)

#Soc vs Td
Soc_GmModels_ct_d<-lapply(Soc_GmModels_ct, dCov)
Soc_LobeModels_ct_d<-lapply(Soc_LobeModels_ct, dCov)
Soc_ROIModels_ct_d<-lapply(Soc_ROIModels_ct, dCov)

#Sph vs Td
Sph_GmModels_ct_d<-lapply(Sph_GmModels_ct, dCov)
Sph_LobeModels_ct_d<-lapply(Sph_LobeModels_ct, dCov)
Sph_ROIModels_ct_d<-lapply(Sph_ROIModels_ct, dCov)


###########################
### VOLUME EFFECT SIZES ###
###########################

#Compute covariate-adjusted effect sizes (Cohens d)

#Define the effect size function (which controls for covariates)
###NOTE: the coefficient is hard coded at t$coefficients[6,1], if you change the number of variables in your model, this number will also have to be changed.
dCov<-function(x){
  t<-summary(x)
  effsize <- t$coefficients[6,1]/t$sigma
  return(abs(effsize))
}

#Double check that the effect size (estimate divided by residual standard error) is correctly calculated.
#test<-lm(Vol_gmSubcortTotal~ age + sex + ageSq + mprage_antsCT_vol_TBV + Add, data = AddData)

#Calculate effect sizes:

#Add vs Td
Add_GmWmModels_vol_d<-lapply(Add_GmWmModels_vol, dCov)
Add_LobeModels_vol_d<-lapply(Add_LobeModels_vol, dCov)
Add_ROIModels_vol_d<-lapply(Add_ROIModels_vol, dCov)

#Agr vs Td
Agr_GmWmModels_vol_d<-lapply(Agr_GmWmModels_vol, dCov)
Agr_LobeModels_vol_d<-lapply(Agr_LobeModels_vol, dCov)
Agr_ROIModels_vol_d<-lapply(Agr_ROIModels_vol, dCov)

#Con vs Td
Con_GmWmModels_vol_d<-lapply(Con_GmWmModels_vol, dCov)
Con_LobeModels_vol_d<-lapply(Con_LobeModels_vol, dCov)
Con_ROIModels_vol_d<-lapply(Con_ROIModels_vol, dCov)

#Gad vs Td
Gad_GmWmModels_vol_d<-lapply(Gad_GmWmModels_vol, dCov)
Gad_LobeModels_vol_d<-lapply(Gad_LobeModels_vol, dCov)
Gad_ROIModels_vol_d<-lapply(Gad_ROIModels_vol, dCov)

#Mdd vs Td
Mdd_GmWmModels_vol_d<-lapply(Mdd_GmWmModels_vol, dCov)
Mdd_LobeModels_vol_d<-lapply(Mdd_LobeModels_vol, dCov)
Mdd_ROIModels_vol_d<-lapply(Mdd_ROIModels_vol, dCov)

#Ocd vs Td
Ocd_GmWmModels_vol_d<-lapply(Ocd_GmWmModels_vol, dCov)
Ocd_LobeModels_vol_d<-lapply(Ocd_LobeModels_vol, dCov)
Ocd_ROIModels_vol_d<-lapply(Ocd_ROIModels_vol, dCov)

#Odd vs Td
Odd_GmWmModels_vol_d<-lapply(Odd_GmWmModels_vol, dCov)
Odd_LobeModels_vol_d<-lapply(Odd_LobeModels_vol, dCov)
Odd_ROIModels_vol_d<-lapply(Odd_ROIModels_vol, dCov)

#Ps vs Td
Ps_GmWmModels_vol_d<-lapply(Ps_GmWmModels_vol, dCov)
Ps_LobeModels_vol_d<-lapply(Ps_LobeModels_vol, dCov)
Ps_ROIModels_vol_d<-lapply(Ps_ROIModels_vol, dCov)

#Ptd vs Td
Ptd_GmWmModels_vol_d<-lapply(Ptd_GmWmModels_vol, dCov)
Ptd_LobeModels_vol_d<-lapply(Ptd_LobeModels_vol, dCov)
Ptd_ROIModels_vol_d<-lapply(Ptd_ROIModels_vol, dCov)

#Sep vs Td
Sep_GmWmModels_vol_d<-lapply(Sep_GmWmModels_vol, dCov)
Sep_LobeModels_vol_d<-lapply(Sep_LobeModels_vol, dCov)
Sep_ROIModels_vol_d<-lapply(Sep_ROIModels_vol, dCov)

#Soc vs Td
Soc_GmWmModels_vol_d<-lapply(Soc_GmWmModels_vol, dCov)
Soc_LobeModels_vol_d<-lapply(Soc_LobeModels_vol, dCov)
Soc_ROIModels_vol_d<-lapply(Soc_ROIModels_vol, dCov)

#Sph vs Td
Sph_GmWmModels_vol_d<-lapply(Sph_GmWmModels_vol, dCov)
Sph_LobeModels_vol_d<-lapply(Sph_LobeModels_vol, dCov)
Sph_ROIModels_vol_d<-lapply(Sph_ROIModels_vol, dCov)



###################################################################
#### FIGURE 2a: LOBE VOlUME EFFECT SIZES BY SCREENING CATEGORY ####
###################################################################

##Create table with lobe effect sizes only for diagnoses with >20 subjects.
facTbl2<-as.data.frame(matrix(nrow=12,ncol=5))
colnames(facTbl2)[1]<-"Subcortical"
colnames(facTbl2)[2]<-"Frontal"
colnames(facTbl2)[3]<-"Occipital"
colnames(facTbl2)[4]<-"Parietal"
colnames(facTbl2)[5]<-"Temporal"

##Name the rows
dxNamesShort2<-c("ADHD","Agoraphobia","Conduct","GAD","MDD","OCD","ODD","Psychosis","PTSD","Separation Anxiety","Social Phobia","Specific Phobia")

row.names(facTbl2)<-dxNamesShort2

dxs2<-c("Add","Agr","Con","Gad","Mdd","Ocd","Odd","Ps","Ptd","Sep","Soc","Sph")

##Add effect sizes to table
         facTbl2[1,1]<-Add_LobeModels_vol_d[1]
         facTbl2[1,2]<-Add_LobeModels_vol_d[2]
	 facTbl2[1,3]<-Add_LobeModels_vol_d[3]
         facTbl2[1,4]<-Add_LobeModels_vol_d[4]
	 facTbl2[1,5]<-Add_LobeModels_vol_d[5]

         facTbl2[2,1]<-Agr_LobeModels_vol_d[1]
         facTbl2[2,2]<-Agr_LobeModels_vol_d[2]
	 facTbl2[2,3]<-Agr_LobeModels_vol_d[3]
         facTbl2[2,4]<-Agr_LobeModels_vol_d[4]
         facTbl2[2,5]<-Agr_LobeModels_vol_d[5]

         facTbl2[3,1]<-Con_LobeModels_vol_d[1]
         facTbl2[3,2]<-Con_LobeModels_vol_d[2]
	 facTbl2[3,3]<-Con_LobeModels_vol_d[3]
         facTbl2[3,4]<-Con_LobeModels_vol_d[4]
         facTbl2[3,5]<-Con_LobeModels_vol_d[5]

         facTbl2[4,1]<-Gad_LobeModels_vol_d[1]
         facTbl2[4,2]<-Gad_LobeModels_vol_d[2]
         facTbl2[4,3]<-Gad_LobeModels_vol_d[3]
         facTbl2[4,4]<-Gad_LobeModels_vol_d[4]
         facTbl2[4,5]<-Gad_LobeModels_vol_d[5]

         facTbl2[5,1]<-Mdd_LobeModels_vol_d[1]
         facTbl2[5,2]<-Mdd_LobeModels_vol_d[2]
	 facTbl2[5,3]<-Mdd_LobeModels_vol_d[3]
         facTbl2[5,4]<-Mdd_LobeModels_vol_d[4]
         facTbl2[5,5]<-Mdd_LobeModels_vol_d[5]

         facTbl2[6,1]<-Ocd_LobeModels_vol_d[1]
         facTbl2[6,2]<-Ocd_LobeModels_vol_d[2]
	 facTbl2[6,3]<-Ocd_LobeModels_vol_d[3]
         facTbl2[6,4]<-Ocd_LobeModels_vol_d[4]
         facTbl2[6,5]<-Ocd_LobeModels_vol_d[5]

         facTbl2[7,1]<-Odd_LobeModels_vol_d[1]
         facTbl2[7,2]<-Odd_LobeModels_vol_d[2]
	 facTbl2[7,3]<-Odd_LobeModels_vol_d[3]
         facTbl2[7,4]<-Odd_LobeModels_vol_d[4]
         facTbl2[7,5]<-Odd_LobeModels_vol_d[5]

         facTbl2[8,1]<-Ps_LobeModels_vol_d[1]
         facTbl2[8,2]<-Ps_LobeModels_vol_d[2]
	 facTbl2[8,3]<-Ps_LobeModels_vol_d[3]
         facTbl2[8,4]<-Ps_LobeModels_vol_d[4]
         facTbl2[8,5]<-Ps_LobeModels_vol_d[5]

         facTbl2[9,1]<-Ptd_LobeModels_vol_d[1]
         facTbl2[9,2]<-Ptd_LobeModels_vol_d[2]
	 facTbl2[9,3]<-Ptd_LobeModels_vol_d[3]
         facTbl2[9,4]<-Ptd_LobeModels_vol_d[4]
         facTbl2[9,5]<-Ptd_LobeModels_vol_d[5]

         facTbl2[10,1]<-Sep_LobeModels_vol_d[1]
         facTbl2[10,2]<-Sep_LobeModels_vol_d[2]
	 facTbl2[10,3]<-Sep_LobeModels_vol_d[3]
         facTbl2[10,4]<-Sep_LobeModels_vol_d[4]
         facTbl2[10,5]<-Sep_LobeModels_vol_d[5]

         facTbl2[11,1]<-Soc_LobeModels_vol_d[1]
         facTbl2[11,2]<-Soc_LobeModels_vol_d[2]
	 facTbl2[11,3]<-Soc_LobeModels_vol_d[3]
         facTbl2[11,4]<-Soc_LobeModels_vol_d[4]
         facTbl2[11,5]<-Soc_LobeModels_vol_d[5]

         facTbl2[12,1]<-Sph_LobeModels_vol_d[1]
         facTbl2[12,2]<-Sph_LobeModels_vol_d[2]
         facTbl2[12,3]<-Sph_LobeModels_vol_d[3]
         facTbl2[12,4]<-Sph_LobeModels_vol_d[4]
         facTbl2[12,5]<-Sph_LobeModels_vol_d[5]


##Reshape to long format
library(reshape2)
facTbl2$group<-as.factor(dxs2)
facTblEffectSize<-facTbl2[,c(1,2,3,4,5,6)]
facTblEFLong<-melt(facTblEffectSize,id.vars="group",variable.name="lobe",value.name="EffectSize")

##Plot w/ ggplot
library(ggplot2)
library(grid)

Fig2a<-ggplot(facTblEFLong, aes(x=group, y=EffectSize,fill=lobe)) +
     ylab("Effect Size") + xlab("") + ggtitle("Lobular Volume Loss: Screening Diagnosis vs. Typically Developing") +
     geom_bar(stat="identity",position=position_dodge()) +
     scale_fill_manual(values=c("#329444","#325194","#943282","#B3141C","#F58311"), breaks=c("Subcortical","Frontal","Occipital","Parietal","Temporal"),
     labels=c("Subcortical", "Frontal Lobe", "Occipital Lobe", "Parietal Lobe", "Temporal Lobe")) +
     theme(axis.title.y = element_text(size = rel(1.5), angle = 90)) + theme(legend.text = element_text(size = 10), legend.justification=c(0.5,0.5), 
     legend.position=c(.95,.9)) +
     theme(plot.title = element_text(size = rel(1.5), vjust = 2)) + theme(axis.text.x = element_text(size = rel(1.25), colour="black")) +
     theme(axis.text.y = element_text(size = rel(1.25), colour="black")) + guides(fill=guide_legend(title=NULL)) +
     scale_x_discrete(breaks=c("Add","Agr","Con","Gad","Mdd","Ocd","Odd","Ps","Ptd","Sep","Soc","Sph","Td"), labels=c("ADHD","Agoraphobia","Conduct","GAD","MDD","OCD","ODD",
     "Psychosis","PTSD","Separation Anx","Social Anx","Specific Phobia","TD")) + theme(plot.margin = unit(c(1,2.5,1,0.5), "cm")) +
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
     axis.line.y = element_line(colour = "black"))

##see colors used in plot (leave "data" specified as is)
#Plot1<-ggplot_build(Fig1)$data

ggsave(file="/data/joy/BBL/projects/pncT1AcrossDisorder/TablesFigures/Figure2a_LobeVolByDiag.png", width = 15, height = 5, units = "in", dpi = 300)



###############################################################
#### FIGURE 2b: LOBE CT EFFECT SIZES BY SCREENING CATEGORY ####
###############################################################

##Create table with lobe effect sizes only for diagnoses with >20 subjects.
facTbl3<-as.data.frame(matrix(nrow=12,ncol=4))
colnames(facTbl3)[1]<-"Frontal"
colnames(facTbl3)[2]<-"Occipital"
colnames(facTbl3)[3]<-"Parietal"
colnames(facTbl3)[4]<-"Temporal"

##Name the rows
dxNamesShort3<-c("ADHD","Agoraphobia","Conduct","GAD","MDD","OCD","ODD","Psychosis","PTSD","Separation Anxiety","Social Phobia","Specific Phobia")

row.names(facTbl3)<-dxNamesShort3

dxs3<-c("Add","Agr","Con","Gad","Mdd","Ocd","Odd","Ps","Ptd","Sep","Soc","Sph")

##Add effect sizes to table
         facTbl3[1,1]<-Add_LobeModels_ct_d[1]
         facTbl3[1,2]<-Add_LobeModels_ct_d[2]
         facTbl3[1,3]<-Add_LobeModels_ct_d[3]
         facTbl3[1,4]<-Add_LobeModels_ct_d[4]

         facTbl3[2,1]<-Agr_LobeModels_ct_d[1]
         facTbl3[2,2]<-Agr_LobeModels_ct_d[2]
         facTbl3[2,3]<-Agr_LobeModels_ct_d[3]
         facTbl3[2,4]<-Agr_LobeModels_ct_d[4]

         facTbl3[3,1]<-Con_LobeModels_ct_d[1]
         facTbl3[3,2]<-Con_LobeModels_ct_d[2]
         facTbl3[3,3]<-Con_LobeModels_ct_d[3]
         facTbl3[3,4]<-Con_LobeModels_ct_d[4]

         facTbl3[4,1]<-Gad_LobeModels_ct_d[1]
         facTbl3[4,2]<-Gad_LobeModels_ct_d[2]
         facTbl3[4,3]<-Gad_LobeModels_ct_d[3]
         facTbl3[4,4]<-Gad_LobeModels_ct_d[4]

         facTbl3[5,1]<-Mdd_LobeModels_ct_d[1]
         facTbl3[5,2]<-Mdd_LobeModels_ct_d[2]
         facTbl3[5,3]<-Mdd_LobeModels_ct_d[3]
         facTbl3[5,4]<-Mdd_LobeModels_ct_d[4]

         facTbl3[6,1]<-Ocd_LobeModels_ct_d[1]
         facTbl3[6,2]<-Ocd_LobeModels_ct_d[2]
         facTbl3[6,3]<-Ocd_LobeModels_ct_d[3]
         facTbl3[6,4]<-Ocd_LobeModels_ct_d[4]

         facTbl3[7,1]<-Odd_LobeModels_ct_d[1]
         facTbl3[7,2]<-Odd_LobeModels_ct_d[2]
         facTbl3[7,3]<-Odd_LobeModels_ct_d[3]
         facTbl3[7,4]<-Odd_LobeModels_ct_d[4]

         facTbl3[8,1]<-Ps_LobeModels_ct_d[1]
         facTbl3[8,2]<-Ps_LobeModels_ct_d[2]
         facTbl3[8,3]<-Ps_LobeModels_ct_d[3]
         facTbl3[8,4]<-Ps_LobeModels_ct_d[4]

         facTbl3[9,1]<-Ptd_LobeModels_ct_d[1]
         facTbl3[9,2]<-Ptd_LobeModels_ct_d[2]
         facTbl3[9,3]<-Ptd_LobeModels_ct_d[3]
         facTbl3[9,4]<-Ptd_LobeModels_ct_d[4]

         facTbl3[10,1]<-Sep_LobeModels_ct_d[1]
         facTbl3[10,2]<-Sep_LobeModels_ct_d[2]
         facTbl3[10,3]<-Sep_LobeModels_ct_d[3]
         facTbl3[10,4]<-Sep_LobeModels_ct_d[4]

         facTbl3[11,1]<-Soc_LobeModels_ct_d[1]
         facTbl3[11,2]<-Soc_LobeModels_ct_d[2]
         facTbl3[11,3]<-Soc_LobeModels_ct_d[3]
         facTbl3[11,4]<-Soc_LobeModels_ct_d[4]

         facTbl3[12,1]<-Sph_LobeModels_ct_d[1]
         facTbl3[12,2]<-Sph_LobeModels_ct_d[2]
         facTbl3[12,3]<-Sph_LobeModels_ct_d[3]
         facTbl3[12,4]<-Sph_LobeModels_ct_d[4]


##Reshape to long format
library(reshape2)
facTbl3$group<-as.factor(dxs3)
facTbl3EffectSize<-facTbl3[,c(1,2,3,4,5)]
facTbl3EFLong<-melt(facTbl3EffectSize,id.vars="group",variable.name="lobe",value.name="EffectSize")

##Plot w/ ggplot
library(ggplot2)
library(grid)


Fig2b<-ggplot(facTbl3EFLong, aes(x=group, y=EffectSize,fill=lobe)) +
     ylab("Effect Size") + xlab("") + ggtitle("Lobular Cortical Thickness Loss: Screening Diagnosis vs. Typically Developing") +
     geom_bar(stat="identity",position=position_dodge()) +
     scale_fill_manual(values=c("#325194","#943282","#B3141C","#F58311"), breaks=c("Frontal","Occipital","Parietal","Temporal"),
     labels=c("Frontal Lobe", "Occipital Lobe", "Parietal Lobe", "Temporal Lobe")) +
     theme(axis.title.y = element_text(size = rel(1.5), angle = 90)) + theme(legend.text = element_text(size = 10), legend.justification=c(0.5,0.5),
     legend.position=c(.9,.9)) +
     theme(plot.title = element_text(size = rel(1.5), vjust = 2)) + theme(axis.text.x = element_text(size = rel(1.25), colour="black")) +
     theme(axis.text.y = element_text(size = rel(1.25), colour="black")) + guides(fill=guide_legend(title=NULL)) +
     scale_x_discrete(breaks=c("Add","Agr","Con","Gad","Mdd","Ocd","Odd","Ps","Ptd","Sep","Soc","Sph","Td"), labels=c("ADHD","Agoraphobia","Conduct","GAD","MDD","OCD","ODD",
     "Psychosis","PTSD","Separation Anx","Social Anx","Specific Phobia","TD")) + theme(plot.margin = unit(c(1,2.5,1,0.5), "cm")) +
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
     axis.line.y = element_line(colour = "black"))

##see colors used in plot (leave "data" specified as is)
#Plot1<-ggplot_build(Fig1)$data

ggsave(file="/data/joy/BBL/projects/pncT1AcrossDisorder/TablesFigures/Figure2b_LobeCTByDiag.png", width = 15, height = 5, units = "in", dpi = 300)



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
#geom_bar(size=0.3) = changes the thickness of the black line around each bar (1 = thicker, .03 = thinner)
#geom_bar(color="black") = changes the color of the black line around each bar (remove this command to remove the black outlines).
#legend.justification = defines which side of the legend that the legend.position coordinates refer to (can use left, right, centre or numeric value (x = between 0 and 1))
#legend.position = move the legend (accepts left, right, top, bottom or define relative coordinates on plot c(x, y) between 0 and 1)
