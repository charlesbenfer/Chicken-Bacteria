#Interested in
#o MIC_TET (tetracycline)
#o MIC_SSS (sulfamethoxazole)
#o MIC_STR (streptomycin)

library(glmnet)
library(caret)
library(ranger)
library(tidyverse)
library(e1071)


for(i in 1:nrow(Chick_Sal_ARA_mgKg.csv)){

  if(Chick_Sal_ARA_mgKg.csv[i,3] == 'ALBERTA'){Chick_Sal_ARA_mgKg.csv[i,3] <- 'Alberta'}
  if(Chick_Sal_ARA_mgKg.csv[i,3] == 'BRITISH COLUMBIA'){Chick_Sal_ARA_mgKg.csv[i,3] <- 'British Columbia'}
  if(Chick_Sal_ARA_mgKg.csv[i,3] == 'ONTARIO'){Chick_Sal_ARA_mgKg.csv[i,3] <- 'Ontario'}
  if(Chick_Sal_ARA_mgKg.csv[i,3] == 'QUEBEC'){Chick_Sal_ARA_mgKg.csv[i,3] <- 'Quebec'}
  if(Chick_Sal_ARA_mgKg.csv[i,3] == 'SASKATCHEWAN'){Chick_Sal_ARA_mgKg.csv[i,3] <- 'Saskatchewan'}

}

for(i in 1:nrow(Chick_Sal_ARA_mgKg)){

  if(Chick_Sal_ARA_mgKg[i,25] == '<= 4'){Chick_Sal_ARA_mgKg[i,25] <- '<=4'}
  if(Chick_Sal_ARA_mgKg[i,25] == '> 32'){Chick_Sal_ARA_mgKg[i,25] <- '>32'}

}

TET_probs <- rep(NA, nrow(Chick_Sal_ARA_mgKg.csv))
for(i in 1:nrow(Chick_Sal_ARA_mgKg.csv)){
  if(any(Chick_Sal_ARA_mgKg.csv[i,25]!='<=4'||Chick_Sal_ARA_mgKg.csv[i,25]!='>32'||
         Chick_Sal_ARA_mgKg.csv[i,25]!='16'||Chick_Sal_ARA_mgKg.csv[i,25]!='32'||Chick_Sal_ARA_mgKg.csv[i,25]!='8')){
    TET_probs[i] <- 0
  }
  else TET_probs[i] <- 1
}



nas <- rep(NA, ncol(Chick_Sal_ARA_mgKg.csv))
for(i in 1:ncol(Chick_Sal_ARA_mgKg.csv)){

  nas[i] <- length(which(is.na(Chick_Sal_ARA_mgKg.csv[,i])))

}

View(as.matrix(nas))

colnames(Chick_Sal_ARA_mgKg.csv)[c(14,17,20,66,39,65,47,7,10,51)]
Chick_Sal_ARA_mgKg <- Chick_Sal_ARA_mgKg.csv[,-c(14,17,20,66,39,65,47,7,10,51)]

Chick_Sal_ARA_mgKg[,25] <- as.factor(Chick_Sal_ARA_mgKg[,25])
levels(Chick_Sal_ARA_mgKg[,25])

Chick_Sal_ARA_mgKg[,25]<-droplevels(Chick_Sal_ARA_mgKg[,25])

#toDo, research Shappley values, add importance metrics to training code




#####################
#Caret Random Forest#
#####################

#Split data into training and testing data

ind <- sample(2, nrow(Chick_Sal_ARA_mgKg), replace = T,prob = c(.7,.3))
train <- Chick_Sal_ARA_mgKg[ind==1,]
test <- Chick_Sal_ARA_mgKg[ind==2,]

#########
#MIC_TET#
#########

#Train a RF for MIC_TET on the train data based on columns 22-56 (Excluding other MIC's)

#Importance = T when training

rf_TET_part <- train(as.formula(paste(colnames(train)[25], "~",
                             paste(colnames(train)[46:58], collapse = "+"),
                       sep = "")), data = train, method = 'ranger')

importance(rf_TET_part)

#Test the RF on the test set

TET_pred_part <- predict(rf_TET_part, test)
confusionMatrix(TET_pred_part, as.factor(test$MIC_TET))


#Train a RF for MIC_TET on the train data based on columns 22-56 (Excluding other MIC's)

rf_TET <- train(as.formula(paste(colnames(Chick_Sal_ARA_mgKg)[25], "~",
                                 paste(colnames(Chick_Sal_ARA_mgKg)[22:56], collapse = "+"),
                                 sep = "")), data = train, method = 'ranger')

#Test the RF on the test set

TET_pred_whole <- predict(rf_TET, test)
confusionMatrix(TET_pred_whole, as.factor(test$MIC_TET))

#100% accuracy?

#########
#MIC_SSS#
#########

#Fix Factor Levels of MIC_SSS
levels(Chick_Sal_ARA_mgKg$MIC_SSS)

for(i in 1:nrow(Chick_Sal_ARA_mgKg)){

  if(Chick_Sal_ARA_mgKg[i,17] == '<= 16'){Chick_Sal_ARA_mgKg[i,17] <- '<=16'}
  if(Chick_Sal_ARA_mgKg[i,17] == '> 256'){Chick_Sal_ARA_mgKg[i,17] <- '>256'}

}

Chick_Sal_ARA_mgKg[,17]<-droplevels(Chick_Sal_ARA_mgKg[,17])

#Train a RF for MIC_SSS on the train data based on columns 40-56

rf_SSS_part <- train(as.formula(paste(colnames(Chick_Sal_ARA_mgKg)[17], "~",
                             paste(colnames(Chick_Sal_ARA_mgKg)[40:56], collapse = "+"),
                             sep = "")), data = train, method = 'ranger')

#Test the RF on the test set

SSS_pred_part <- predict(rf_SSS_part, test)
confusionMatrix(SSS_pred_part, as.factor(test$MIC_SSS))

#Poorer performance


#Train a RF for MIC_SSS on the train data based on columns 22-56

rf_SSS <- train(as.formula(paste(colnames(Chick_Sal_ARA_mgKg)[17], "~",
                                 paste(colnames(Chick_Sal_ARA_mgKg)[22:56], collapse = "+"),
                                 sep = "")), data = train, method = 'ranger')

#Test the RF on the test set

SSS_pred <- predict(rf_SSS, test)
confusionMatrix(SSS_pred, as.factor(test$MIC_SSS))

#100% accuracy again, must be something in those extra variables


#To investigate: keep pushing with Trees, meeting about SWAG

#########
#MIC_STR#
#########

#The levels for this over lap too much, we should see if we can clear this up


###############################################################################
