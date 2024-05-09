#Interested in
#o MIC_TET (tetracycline)
#o MIC_SSS (sulfamethoxazole)
#o MIC_STR (streptomycin)

library(caret)
library(ranger)
library(e1071)
library(randomForest)
library(mlbench)


Chick_Sal_ARA_mgKg <- read.csv("Chick_Sal_ARA_mgKg.csv", row.names=1)

#Cleaning Region Names
for(i in 1:nrow(Chick_Sal_ARA_mgKg)){

  if(Chick_Sal_ARA_mgKg[i,3] == 'ALBERTA'){Chick_Sal_ARA_mgKg[i,3] <- 'Alberta'}
  if(Chick_Sal_ARA_mgKg[i,3] == 'BRITISH COLUMBIA'){Chick_Sal_ARA_mgKg[i,3] <- 'British Columbia'}
  if(Chick_Sal_ARA_mgKg[i,3] == 'ONTARIO'){Chick_Sal_ARA_mgKg[i,3] <- 'Ontario'}
  if(Chick_Sal_ARA_mgKg[i,3] == 'QUEBEC'){Chick_Sal_ARA_mgKg[i,3] <- 'Quebec'}
  if(Chick_Sal_ARA_mgKg[i,3] == 'SASKATCHEWAN'){Chick_Sal_ARA_mgKg[i,3] <- 'Saskatchewan'}

}

#Finding which variables have too many NA's
nas<-rep(NA,ncol(Chick_Sal_ARA_mgKg))
for(i in 1:ncol(Chick_Sal_ARA_mgKg)){
  nas[i] <- length(which(is.na(Chick_Sal_ARA_mgKg[,i])))
}

View(as.matrix(nas))
colnames(Chick_Sal_ARA_mgKg)[c(14,17,20,66,39,65,47)]
Chick_Sal_ARA_mgKg <- Chick_Sal_ARA_mgKg[,-c(14,17,20,66,39,65,47)]

#Removing Unnecessary 'C' Variables
View(Chick_Sal_ARA_mgKg)
Chick_Sal_ARA_mgKg <- Chick_Sal_ARA_mgKg[,-(29:45)]

#Removing Estab and Region Num variables
Chick_Sal_ARA_mgKg <- Chick_Sal_ARA_mgKg[,-c(5,8)]


#########
#MIC_TET#
#########
#Fix Repeated Factor Levels of MIC_TET
Chick_Sal_ARA_mgKg[,25] <- as.factor(Chick_Sal_ARA_mgKg[,25])
levels(Chick_Sal_ARA_mgKg[,25])
Chick_Sal_ARA_mgKg[,25] <- as.character(Chick_Sal_ARA_mgKg[,25])

for(i in 1:nrow(Chick_Sal_ARA_mgKg)){

  if(Chick_Sal_ARA_mgKg[i,25] == '<= 4'){Chick_Sal_ARA_mgKg[i,25] <- '<=4'}
  if(Chick_Sal_ARA_mgKg[i,25] == '> 32'){Chick_Sal_ARA_mgKg[i,25] <- '32'}

}
Chick_Sal_ARA_mgKg[,25] <- as.factor(Chick_Sal_ARA_mgKg[,25])
levels(Chick_Sal_ARA_mgKg[,25])



#########
#MIC_SSS#
#########

#Fix Repeated Factor Levels of MIC_SSS
Chick_Sal_ARA_mgKg[,22] <- as.factor(Chick_Sal_ARA_mgKg[,22])
levels(Chick_Sal_ARA_mgKg[,22])
Chick_Sal_ARA_mgKg[,22] <- as.character(Chick_Sal_ARA_mgKg[,22])

for(i in 1:nrow(Chick_Sal_ARA_mgKg)){

  if(Chick_Sal_ARA_mgKg[i,22] == '<= 16'){Chick_Sal_ARA_mgKg[i,22] <- '<=16'}
  if(Chick_Sal_ARA_mgKg[i,22] == '> 256'){Chick_Sal_ARA_mgKg[i,22] <- '>256'}

}
Chick_Sal_ARA_mgKg[,22] <- as.factor(Chick_Sal_ARA_mgKg[,22])
levels(Chick_Sal_ARA_mgKg[,22])


#########
#MIC_STR#
#########
Chick_Sal_ARA_mgKg[,28] <- as.factor(Chick_Sal_ARA_mgKg[,28])
levels(Chick_Sal_ARA_mgKg[,28])
Chick_Sal_ARA_mgKg[,23] <- as.character(Chick_Sal_ARA_mgKg[,23])

which(Chick_Sal_ARA_mgKg[,23] == '')
Chick_Sal_ARA_mgKg_STR <- Chick_Sal_ARA_mgKg[-which(Chick_Sal_ARA_mgKg[,23] == ''),]
nrow(Chick_Sal_ARA_mgKg_STR)
Chick_Sal_ARA_mgKg_STR[,23] <- as.factor(Chick_Sal_ARA_mgKg_STR[,23])
levels(Chick_Sal_ARA_mgKg_STR[,23])


#Store a cleaned version of the data for TET and SSS, STR has its own thing going on
Chick_Sal_ARA_mgKg_TET <- Chick_Sal_ARA_mgKg
Chick_Sal_ARA_mgKg_SSS <- Chick_Sal_ARA_mgKg

###############################################################################
