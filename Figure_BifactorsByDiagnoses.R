######################
#### READ IN DATA ####
######################

subjData<-readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1396_T1_subjData_NewRavens.rds")

 
####################################################
#### FIGURE 1: BIFACTORS BY SCREENING CATEGORY ####
####################################################

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

        facTbl[i,1]<-mean(subjData$overall_psychopathology_4factorv2[which(y==1)],na.rm=TRUE)
        facTbl[i,2]<-sd(subjData$overall_psychopathology_4factorv2[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

        facTbl[i,3]<-mean(subjData$mood_4factorv2[which(y==1)],na.rm=TRUE)
        facTbl[i,4]<-sd(subjData$mood_4factorv2[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

        facTbl[i,5]<-mean(subjData$psychosis_4factorv2[which(y==1)],na.rm=TRUE)
        facTbl[i,6]<-sd(subjData$psychosis_4factorv2[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

        facTbl[i,7]<-mean(subjData$externalizing_4factorv2[which(y==1)],na.rm=TRUE)
        facTbl[i,8]<-sd(subjData$externalizing_4factorv2[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

        facTbl[i,9]<-mean(subjData$phobias_4factorv2[which(y==1)],na.rm=TRUE)
        facTbl[i,10]<-sd(subjData$phobias_4factorv2[which(y==1)],na.rm=TRUE)/sqrt(length(which(y==1)))

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
        theme(plot.title = element_text(size = rel(1.5), vjust = 0, hjust = .5)) + theme(axis.text.x = element_text(size = rel(1.25), colour="black")) +
        theme(axis.text.y = element_text(size = rel(1.25), colour="black")) + guides(fill=guide_legend(title=NULL)) +
        scale_x_discrete(breaks=c("Add","Agr","Con","Gad","Mdd","Ocd","Odd","Ps","Ptd","Sep","Soc","Sph","Td"), labels=c("ADHD","Agoraphobia","Conduct","GAD","MDD","OCD","ODD",
        "Psychosis","PTSD","Separation Anx","Social Anx","Specific Phobia","TD")) + theme(plot.margin = unit(c(1,2.5,1,0.5), "cm")) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"))

#see colors used in plot (leave "data" specified as is)
Plot1<-ggplot_build(Fig1)$data

ggsave(file="/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Figure1_BifactorsByDiag.png", width = 15, height = 5, units = "in", dpi = 300)


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



