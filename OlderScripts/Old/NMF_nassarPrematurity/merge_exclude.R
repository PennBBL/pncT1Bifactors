###LOAD DATA### 
t1Qa <-read.csv( "Documents/Magic Briefcase/PROJECTS/nassarPrematurity/dataFreeze20161215/n1601_t1QaData_v2.csv" ) 
neo <-read.csv( "Documents/Magic Briefcase/PROJECTS/nassarPrematurity/dataFreeze20161215/gaData_final.csv" ) 
health <-read.csv( "Documents/Magic Briefcase/PROJECTS/nassarPrematurity/dataFreeze20161215/n1601_health_20161214.csv" ) 
antsVol <-read.csv( "Documents/Magic Briefcase/PROJECTS/nassarPrematurity/dataFreeze20161215/n1601_antsCtVol.csv" )
jlfVol <-read.csv( "Documents/Magic Briefcase/PROJECTS/nassarPrematurity/dataFreeze20161215/n1601_jlfAntsCTIntersectionVol.csv" ) 
demo <-read.csv( "Documents/Magic Briefcase/PROJECTS/nassarPrematurity/dataFreeze20161215/n1601_demographics_go1_20161212.csv" ) 
cnb <-read.csv( "Documents/Magic Briefcase/PROJECTS/nassarPrematurity/dataFreeze20161215/n1601_cnb_factor_scores_tymoore_20151006.csv" ) 


###GA DATA OPERATIONS## 
#filter neo data by non-missing GA 
neo2 <- neo [!is.na( neo $ ga ),] #345 


#establish <37 week preterm category 
neo2 $ preterm <- 0 
neo2 $ preterm [which( neo2 $ ga < 37 )]<- 1 
table( neo2 $ preterm ) 


###MERGE DATA### 
dataComb1 <-merge( neo2 , t1Qa ) 
dataComb2 <-merge( dataComb1 , health ) 
dataComb3 <-merge( dataComb2 , antsVol ) 
dataComb4 <-merge( dataComb3 , jlfVol ) 
dataComb5 <-merge( dataComb4 , demo ) 
dataComb6 <-merge( dataComb5 , cnb ) #still 345 


###APPLY EXCLUSIONS### 
data <- dataComb6 [which( dataComb6 $ healthExcludev2 == 0 & dataComb6 $ t1Exclude == 0 ),] #n=279 