#########
#MIC_SSS#
#########

ind <- sample(2, nrow(Chick_Sal_ARA_mgKg), replace = T,prob = c(.7,.3))
train <- Chick_Sal_ARA_mgKg[ind==1,]
test <- Chick_Sal_ARA_mgKg[ind==2,]

#Train a RF for MIC_SSS on the train data based on columns 40-56
rf_SSS_2 <- randomForest(as.formula(paste(colnames(Chick_Sal_ARA_mgKg)[22],
                                        "~",paste(colnames(Chick_Sal_ARA_mgKg)[c(1:21,23:39,41:56)],
                                                  collapse = "+"),sep = "")),
                       data = train, ntree=1000,keep.forest=T,importance=T)

importance(rf_SSS_2,type=1)
varImpPlot(rf_SSS_2)
rf_SSS_2$confusion
# Final ID Serotype has massive comparative accuracy decrease, what is this variable?

#Prediction with generated random forest
SSS_preds_2 <- predict(rf_SSS_2, newdata = test)
confusionMatrix(test[,22],SSS_preds_2)

################################################################################

#Let's try to more accurately label the classes.

Chick_Sal_ARA_mgKg_SSS <- read.csv("Chick_Sal_ARA_mgKg.csv", row.names=1)
for(i in 1:nrow(Chick_Sal_ARA_mgKg)){

  if(Chick_Sal_ARA_mgKg[i,3] == 'ALBERTA'){Chick_Sal_ARA_mgKg[i,3] <- 'Alberta'}
  if(Chick_Sal_ARA_mgKg[i,3] == 'BRITISH COLUMBIA'){Chick_Sal_ARA_mgKg[i,3] <- 'British Columbia'}
  if(Chick_Sal_ARA_mgKg[i,3] == 'ONTARIO'){Chick_Sal_ARA_mgKg[i,3] <- 'Ontario'}
  if(Chick_Sal_ARA_mgKg[i,3] == 'QUEBEC'){Chick_Sal_ARA_mgKg[i,3] <- 'Quebec'}
  if(Chick_Sal_ARA_mgKg[i,3] == 'SASKATCHEWAN'){Chick_Sal_ARA_mgKg[i,3] <- 'Saskatchewan'}

}
colnames(Chick_Sal_ARA_mgKg_SSS)[c(14,17,20,66,39,65,47,7,10,51)]
Chick_Sal_ARA_mgKg_SSS <- Chick_Sal_ARA_mgKg_SSS[,-c(14,17,20,66,39,65,47,7,10,51)]

for(i in 1:nrow(Chick_Sal_ARA_mgKg_SSS)){
  if(Chick_Sal_ARA_mgKg_SSS[i,22] == '<= 16' || Chick_Sal_ARA_mgKg_SSS[i,22] == '<=16'){
    Chick_Sal_ARA_mgKg_SSS[i,22] <- '<=16'
  }
  if(Chick_Sal_ARA_mgKg_SSS[i,22] == '> 256' ||Chick_Sal_ARA_mgKg_SSS[i,22] == '>256'
     ||Chick_Sal_ARA_mgKg_SSS[i,22] == '256'){Chick_Sal_ARA_mgKg_SSS[i,22] <- '>=256'}
}
Chick_Sal_ARA_mgKg_SSS[,22]<-as.factor(Chick_Sal_ARA_mgKg_SSS[,22])
levels(Chick_Sal_ARA_mgKg_SSS[,22])

#Run another RF with the more diverse labels

ind <- sample(2, nrow(Chick_Sal_ARA_mgKg_SSS), replace = T,prob = c(.7,.3))
train_SSS <- Chick_Sal_ARA_mgKg_SSS[ind==1,]
test_SSS <- Chick_Sal_ARA_mgKg_SSS[ind==2,]

#Train a RF for MIC_SSS on the train data based on columns 40-56
rf_SSS_5 <- randomForest(as.formula(paste(colnames(Chick_Sal_ARA_mgKg_SSS)[22],
                                        "~",paste(colnames(Chick_Sal_ARA_mgKg_SSS)[c(1:21,23:39,41:56)],
                                                  collapse = "+"),sep = "")),
                       data = train_SSS, ntree=1000,keep.forest=T,importance=T)

importance(rf_SSS_5,type=1)
varImpPlot(rf_SSS_5)
rf_SSS_5$confusion
#There are not enough of each class for this to be super meaningful it seems,
#guidance on how to split more effectively may be necessary

#Prediction with generated random forest
SSS_preds_5 <- predict(rf_SSS_5, newdata = test_SSS)
confusionMatrix(test_SSS[,22],SSS_preds_5)
#Similar to above in 66
