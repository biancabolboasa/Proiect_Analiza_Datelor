getwd()
setwd("F:/facultate/an 3 sem 1/AD/Seminare/Indicatori ad")
proiect <- read.table("Date_AD.txt",header=TRUE,sep="\t",dec=".",row.names = 1)
attach(proiect)
View(proiect)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
windows()
chart.Correlation(proiect, histogram=TRUE, method="pearson")
library(corrplot)
windows()
corrplot(cor(proiect),method = "number",type="upper")

  #Aplic Analiza factor
library(psych)
KMO(cor(proiect)) #0.74

cortest.bartlett(cor(proiect),n=157) #2.273616e-196 (p-value)

windows()
fa.parallel(proiect) #4 factori

#Componente Principale fara rotatie
windows()
af <- fa(cor(proiect), nfactors = 4,n.obs=157, rotate="none",fm="pa",covar=FALSE)
fa.diagram(af)

#Componente principale cu rotatie
windows()
af1 <- fa(cor(proiect), nfactors = 4,n.obs=157, rotate="varimax",fm="pa",covar=FALSE)
fa.diagram(af1)

#Verosimilitate maxima fara rotatie
windows()
af <- fa(cor(proiect), nfactors = 4,n.obs=157, rotate="none",fm="ml",covar=FALSE)
fa.diagram(af)

#Verosimilitate maxima cu rotatie
windows()
af<- fa(cor(proiect), nfactors = 4,n.obs=157, rotate="varimax",fm="ml",covar=FALSE)
fa.diagram(af)

#Verosimilitate maxima cu rotatie- 3 factori
windows()
af <- fa(cor(proiect), nfactors = 3,n.obs=157, rotate="varimax",fm="ml",covar=FALSE)
fa.diagram(af)

af1
