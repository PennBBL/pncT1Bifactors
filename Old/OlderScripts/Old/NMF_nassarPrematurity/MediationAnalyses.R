############################
#### MEDIATION ANALYSES ####
############################

#Load data
subjData<-read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/MergedNMF_20170306.csv", header=TRUE, na.strings="NA")

#standardize the variables of interest (compute z scores)
VarNames <- c("Nmf18C1", "Nmf18C3", "Nmf18C4", "Nmf18C5", "Nmf18C8", "Nmf18C9", "Nmf18C11", "Nmf18C12", "Nmf18C13", "Nmf18C14", "Nmf18C15", "Nmf18C16", "Nmf18C17", "Nmf18C18", 
              "ga", "Overall_Accuracy", "Overall_Efficiency", "F1_Exec_Comp_Res_Accuracy", "F3_Memory_Accuracy", "F2_Social_Cog_Accuracy")
VarData <- subjData[,VarNames]
VarData[] <- lapply(VarData, scale) 

#Create vector of NMF component names that you want to test
NmfNames<- names(VarData)[grep("Nmf18",names(VarData))]

#Create vector of cognitive measure names that you want to test
CogNames<- names(VarData)[c(grep("Accuracy",names(VarData)), grep("Efficiency",names(VarData)))]

#Run mediation analyses to see if c-prime become less or non-significant

psych::mediate(x="ga", m="Nmf18C1", y="Overall_Accuracy", data=VarData)

MediationModels <- lapply(NmfNames, function(x) {
  psych::mediate(substitute(x="ga", m=i, y="Overall_Accuracy", list(i = as.name(x))), data = VarData)
})

med.fit <- lm(Nmf18C1 ~ ageAtScan1 + sex + medu1 + ga, data = subjData)
out.fit <- lm(Overall_Accuracy ~ ageAtScan1 + sex + medu1 + ga, data = subjData)

med.out <- mediate(med.fit, out.fit, treat = "Overall_Accuracy", mediator = "Nmf18C1", robustSE = TRUE, sims = 100)
summary(med.out)

#To plot the 

#If the mediation analyses above suggest mediation may be present, follow-up with bootstrapped confidence intervals from the lavaan package:
#Load packages
library(lavaan)
library(semPlot)
library(caret)

#z-score X, M, and Y
X <- scale(subjData$ga)

M <- scale(subjData$Nmf18C1)

Y <- scale(subjData$Overall_Accuracy)

age <- scale(subjData$ageAtScan1)

#Add variables of interest into a dataframe
Data <- data.frame(X = X, Y = Y, M = M, age = age)

model <- ' # direct effect "c path" (X->Y)

          Y ~ c*X + age

          # "a path" (X->M)

          M ~ a*X + age

          # "b path" (M->Y)

          Y ~ b*M + age

          Y ~ C*X + B*M + age

          # indirect effect (a*b) ("c-c prime"; amount of mediation)

          ab := a*b

          # total effect (direct effect + indirect effect)

          total := c + (a*b)

          '


model <- ' # direct effect "c path" (X->Y)

             Y ~ c*X

           # "a path" (X->M)

             M ~ a*X

          # "b path" (M->Y)

             Y ~ b*M

         Y ~ C*X + B*M

          # indirect effect (a*b) ("c-c prime"; amount of mediation)

             ab := a*b

          # total effect (direct effect + indirect effect)

             total := c + (a*b)

         '

example1 <- ' ## regressions
m1 ~ a1*x1 + x2 + x3
m2 ~ a2*x1 + x2 + x3
y1 ~ b1*m1 + b2*m2 + x2
## define indirect effects
ind1 := a1 * b1
ind2 := a2 * b2
totalind := ind1 + ind2
## correlated residual variances
m1 ~~ m2 '

fit <- sem(model, data = Data, se="bootstrap", bootstrap=10000)

summary(fit, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)

boot.fit <- parameterEstimates(fit, boot.ci.type="perc",level=0.95, ci=TRUE,standardized = TRUE)

boot.fit

pdf('testMediationTriangle.pdf')
semPaths(fit, what='std',title = FALSE, edge.label.cex=1.5, residuals = F)
dev.off()


