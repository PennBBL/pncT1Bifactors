
subjData<-readRDS("/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/n1385_MARS_datarel_020716.rds")
install.packages("lsmeans", repos="http://R-Forge.R-project.org")
library(lsmeans)
require(lsmeans)
Test <- lm(Vol_gmTotal ~ age + sex, data = subjData)
lsmeans(Test, ~ sex)

#Error: could not find function "lsmeans"