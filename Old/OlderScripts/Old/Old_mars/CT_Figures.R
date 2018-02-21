#To graph CT results

#Read in data
subjData<-readRDS("/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/n1385_MARS_datarel_020716.rds")

#Load library for nolinear analyses
library(mgcv)
library(visreg)

#Make sex an ordered variable (only need ordered if doing an interaction with a spline age term- spline interactions don't work for more than two levels)
subjData$sex<-ordered(subjData$sex)

#Add labels to sex
subjData$sex <- factor(subjData$sex,labels=c("Male","Female"))

Model1<-gam(CT_gmTotal ~ s(age) + sex + mprageMassICV + averageRating + goassessItemBifactor4FactorMood + goassessItemBifactor4FactorPsych + goassessItemBifactor4FactorExt + goassessItemBifactor4FactorPhb + goassessItemBifactor4FactorOverallPsy, method='REML', data = subjData)

Model2<-gam(CT_gmFrontalTotal ~ s(age) + sex + mprageMassICV + averageRating + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, method='REML', data = subjData)


####COLORS####
colorB<-rgb(0,0,1,alpha=0.3) #"00FF004D"
colorR<-rgb(1,0,0,alpha=0.3) # "#FF00004D"
colorFillSex<-c("#0000FF4D","#FF00004D")
colorSex<-c("blue","red")

#Total grey matter cortical thickness by age
jpeg(file = "/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/TablesFigures/CTGmByAge.jpg")
par(mar=c(6,6,1,1))
visreg(Model1, "age", ylab = "Cortical Thickness: Total Grey Matter", xlab = "Age (years)", cex.lab=1.5, cex.axis=1.25, mgp=c(4,1,0))
dev.off()

#Frontal lobe cortical thickness by age
jpeg(file = "/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/TablesFigures/CTFrontalByAge.jpg")
par(mar=c(6,6,1,1))
visreg(Model2, "age", ylab = "Cortical Thickness: Frontal Lobe", xlab = "Age (years)", cex.lab=1.5, cex.axis=1.25, mgp=c(4,1,0))
dev.off()


##Helpful hints
#ylab,xlab,main - y and x axis labels and main title label
#cex - font size (1.25 and 1.5 are reasonable values)
        #cex.lab - label font size (both x and y)
        #cex.main - main title font size
        #cex.axis - font size of x and y axis numbers
#font.main - font of main title (1=plain, 2=bold, 3=italic, 4=bold italic, 5=symbol)
#mpg - location of x and y axis labels to the graph: default is c(3,1,0) where changing 3 moves the labels further or closer to the graph.
#legend() - set various parameters to create legend text, line width (lwd), colors (col), font size (cex), and location ("bottomright", "bottom", "bottomleft", "left", \
"topleft", "top", "topright", "right" and "center")
#xlim,ylim - limits of the x and y axes
#mar - margins around plot; use par(mar=()) before visreg to change the dimensions of the plot canvas (Bottom, Left, Top, Right).
