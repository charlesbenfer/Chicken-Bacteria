###############
#Random Forest#
###############

#Split data into training and testing data

ind <- sample(2, nrow(Chick_Sal_ARA_mgKg), replace = T,prob = c(.7,.3))
train_TET <- Chick_Sal_ARA_mgKg[ind==1,]
test_TET <- Chick_Sal_ARA_mgKg[ind==2,]

#########
#MIC_TET#
#########

#Train a RF for MIC_TET on the train data based on all variables (Excluding itself and "CTET" Variable)

rf_TET_2 <- randomForest(as.formula(paste(colnames(Chick_Sal_ARA_mgKg)[25],
                                        "~",paste(colnames(Chick_Sal_ARA_mgKg)[c(1:24,26:41,43:56)],
                                                  collapse = "+"),sep = "")),
                       data = train_TET, ntree=1000,keep.forest=T,importance=T)

importance(rf_TET_2,type=1)
varImpPlot(rf_TET_2)
rf_TET_2$confusion
# Final ID Serotype has massive comparative accuracy decrease, what is this variable?

#Prediction with generated random forest
TET_2_preds <- predict(rf_TET_2, newdata = test_TET)
confusionMatrix(test_TET[,25],TET_2_preds)


################################################################################

#Let's try and increase the number of classes: Begin with all of the original

Chick_Sal_ARA_mgKg_TET <- read.csv("Chick_Sal_ARA_mgKg.csv", row.names=1)

#Do all of the beginning cleaning as before

for(i in 1:nrow(Chick_Sal_ARA_mgKg_TET)){

  if(Chick_Sal_ARA_mgKg_TET[i,3] == 'ALBERTA'){Chick_Sal_ARA_mgKg_TET[i,3] <- 'Alberta'}
  if(Chick_Sal_ARA_mgKg_TET[i,3] == 'BRITISH COLUMBIA'){Chick_Sal_ARA_mgKg_TET[i,3] <- 'British Columbia'}
  if(Chick_Sal_ARA_mgKg_TET[i,3] == 'ONTARIO'){Chick_Sal_ARA_mgKg_TET[i,3] <- 'Ontario'}
  if(Chick_Sal_ARA_mgKg_TET[i,3] == 'QUEBEC'){Chick_Sal_ARA_mgKg_TET[i,3] <- 'Quebec'}
  if(Chick_Sal_ARA_mgKg_TET[i,3] == 'SASKATCHEWAN'){Chick_Sal_ARA_mgKg_TET[i,3] <- 'Saskatchewan'}

}

colnames(Chick_Sal_ARA_mgKg_TET)[c(14,17,20,66,39,65,47,7,10,51)]
Chick_Sal_ARA_mgKg_TET <- Chick_Sal_ARA_mgKg_TET[,-c(14,17,20,66,39,65,47,7,10,51)]

#Keep almost all of the original labels for TET, run the RF
Chick_Sal_ARA_mgKg_TET[,25]<-as.factor(Chick_Sal_ARA_mgKg_TET[,25])
levels(Chick_Sal_ARA_mgKg_TET[,25])
Chick_Sal_ARA_mgKg_TET[,25]<-as.character(Chick_Sal_ARA_mgKg_TET[,25])
for(i in 1:nrow(Chick_Sal_ARA_mgKg_TET)){
  if(Chick_Sal_ARA_mgKg_TET[i,25]=='<= 4'){Chick_Sal_ARA_mgKg_TET[i,25]<-'<=4'}
  if(Chick_Sal_ARA_mgKg_TET[i,25]=='> 32'||Chick_Sal_ARA_mgKg_TET[i,25]=='>32'
     ||Chick_Sal_ARA_mgKg_TET[i,25]=='32'){Chick_Sal_ARA_mgKg_TET[i,25]<-'>=32'}
}
Chick_Sal_ARA_mgKg_TET[,25]<-as.factor(Chick_Sal_ARA_mgKg_TET[,25])
levels(Chick_Sal_ARA_mgKg_TET[,25])

ind <- sample(2, nrow(Chick_Sal_ARA_mgKg_TET), replace = T,prob = c(.7,.3))
train_TET_4 <- Chick_Sal_ARA_mgKg_TET[ind==1,]
test_TET_4 <- Chick_Sal_ARA_mgKg_TET[ind==2,]

rf_TET_4 <- randomForest(as.formula(paste(colnames(Chick_Sal_ARA_mgKg_TET)[25],
                                          "~",paste(colnames(Chick_Sal_ARA_mgKg_TET)[c(1:24,26:41,43:56)],
                                                    collapse = "+"),sep = "")),
                         data = train_TET_4, ntree=1000,keep.forest=T,importance=T)

#Sometimes get an empty class error, not enough observations of a class

importance(rf_TET_4,type=1)
varImpPlot(rf_TET_4)
rf_TET_4$confusion
# Final ID Serotype has massive comparative accuracy decrease, what is this variable?

#Prediction with generated random forest
TET_4_preds <- predict(rf_TET_4, newdata = test_TET_4)
confusionMatrix(test_TET_4[,25],TET_4_preds)

#8 and 16 classes should get combined, but does this make sense for the experiment?


################################################################################
#From randomForest documentation

#Here are the definitions of the variable importance measures.
#The first measure is computed from permuting OOB data: For each tree,
#the prediction error on the out-of-bag portion of the data is recorded
#(error rate for classification, MSE for regression). Then the same is done
#after permuting each predictor variable. The difference between the two are
#then averaged over all trees, and normalized by the standard deviation of
#the differences. If the standard deviation of the differences is equal to 0
#for a variable, the division is not done (but the average is almost always
#equal to 0 in that case).

#The second measure is the total decrease in node impurities from splitting
#on the variable, averaged over all trees. For classification, the node impurity
#is measured by the Gini index.

#Gini Index, also known as Gini impurity, calculates the amount of probability
#of a specific feature that is classified incorrectly when selected randomly.
#If all the elements are linked with a single class then it can be called pure.
