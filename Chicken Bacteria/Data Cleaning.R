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


for(i in 1:nrow(Chick_Sal_ARA_mgKg)){

  if(Chick_Sal_ARA_mgKg[i,3] == 'ALBERTA'){Chick_Sal_ARA_mgKg[i,3] <- 'Alberta'}
  if(Chick_Sal_ARA_mgKg[i,3] == 'BRITISH COLUMBIA'){Chick_Sal_ARA_mgKg[i,3] <- 'British Columbia'}
  if(Chick_Sal_ARA_mgKg[i,3] == 'ONTARIO'){Chick_Sal_ARA_mgKg[i,3] <- 'Ontario'}
  if(Chick_Sal_ARA_mgKg[i,3] == 'QUEBEC'){Chick_Sal_ARA_mgKg[i,3] <- 'Quebec'}
  if(Chick_Sal_ARA_mgKg[i,3] == 'SASKATCHEWAN'){Chick_Sal_ARA_mgKg[i,3] <- 'Saskatchewan'}

}

colnames(Chick_Sal_ARA_mgKg)[c(14,17,20,66,39,65,47,7,10,51)]
Chick_Sal_ARA_mgKg <- Chick_Sal_ARA_mgKg[,-c(14,17,20,66,39,65,47,7,10,51)]


#Changing the units for MIC_TET, let's start by narrowing down to 2 classes. <=8 vs >=16
for(i in 1:nrow(Chick_Sal_ARA_mgKg)){

  if(Chick_Sal_ARA_mgKg[i,25] == '<= 4'||Chick_Sal_ARA_mgKg[i,25] =='<=4'||Chick_Sal_ARA_mgKg[i,25] =='8'){
    Chick_Sal_ARA_mgKg[i,25] <- '<=8'
    }

  else Chick_Sal_ARA_mgKg[i,25] <- '>=16'

}

Chick_Sal_ARA_mgKg[,25] <- as.factor(Chick_Sal_ARA_mgKg[,25])
levels(Chick_Sal_ARA_mgKg[,25])


#########
#MIC_SSS#
#########

#Fix Factor Levels of MIC_SSS

for(i in 1:nrow(Chick_Sal_ARA_mgKg)){

  if(Chick_Sal_ARA_mgKg[i,22] == '<= 16' || Chick_Sal_ARA_mgKg[i,22] == '<=16'|| Chick_Sal_ARA_mgKg[i,22] == '32'
     || Chick_Sal_ARA_mgKg[i,22] == '64'){Chick_Sal_ARA_mgKg[i,22] <- '<=64'}
  else Chick_Sal_ARA_mgKg[i,22] <- '>=128'

}

Chick_Sal_ARA_mgKg[,22] <- as.factor(Chick_Sal_ARA_mgKg[,22])
levels(Chick_Sal_ARA_mgKg[,22])


#########
#MIC_STR#
#########
Chick_Sal_ARA_mgKg[,23] <- as.factor(Chick_Sal_ARA_mgKg[,23])
levels(Chick_Sal_ARA_mgKg[,23])
Chick_Sal_ARA_mgKg[,23] <- as.character(Chick_Sal_ARA_mgKg[,23])

which(Chick_Sal_ARA_mgKg[,23] == '')
Chick_Sal_ARA_mgKg_STR <- Chick_Sal_ARA_mgKg[-which(Chick_Sal_ARA_mgKg[,23] == ''),]
Chick_Sal_ARA_mgKg_STR[,23] <- as.factor(Chick_Sal_ARA_mgKg_STR[,23])
levels(Chick_Sal_ARA_mgKg_STR[,23])
Chick_Sal_ARA_mgKg_STR[,23] <- as.character(Chick_Sal_ARA_mgKg_STR[,23])
for(i in 1:nrow(Chick_Sal_ARA_mgKg_STR)){
  if(Chick_Sal_ARA_mgKg_STR[i,23]=='32'){Chick_Sal_ARA_mgKg_STR[i,23]<-'<=32'}
}
Chick_Sal_ARA_mgKg_STR[,23] <- as.factor(Chick_Sal_ARA_mgKg_STR[,23])
levels(Chick_Sal_ARA_mgKg_STR[,23])

#To investigate: keep pushing with Trees, meeting about SWAG

###############################################################################
