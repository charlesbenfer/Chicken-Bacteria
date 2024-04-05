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

importance(rf_TET_2,type = 1)
importance(rf_TET_2, type = 2)
nrow(importance(rf_TET_2, type = 2))
varImpPlot(rf_TET_2)
rf_TET_2$confusion

# Final ID Serotype has massive comparative accuracy decrease, what is this variable?

#Prediction with generated random forest
TET_2_preds <- predict(rf_TET_2, newdata = test_TET)
confusionMatrix(test_TET[,25],TET_2_preds)$overall[1]


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
length(which(Chick_Sal_ARA_mgKg_TET[,25]=="8"))

ind <- sample(2, nrow(Chick_Sal_ARA_mgKg_TET), replace = T,prob = c(.7,.3))
train_TET_4 <- Chick_Sal_ARA_mgKg_TET[ind==1,]
length(which(train_TET_4[,25]=="8"))
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

################################################################################

#Run several random forests and find means for MDA, MDG, accuracy, sensitivity and specificity

#Binary
n <- 10
TET_Bi_MDAs <- matrix(NA, nrow = 54, ncol = n)
TET_Bi_MDGs <- matrix(NA, nrow = 54, ncol = n)
TET_Bi_Accs <- rep(NA, n)
TET_Bi_Sens <- rep(NA, n)
TET_Bi_Specs <- rep(NA, n)


for(i in 1:n){

  rf_TET_2 <- randomForest(as.formula(paste(colnames(Chick_Sal_ARA_mgKg)[25],
                                            "~",paste(colnames(Chick_Sal_ARA_mgKg)[c(1:24,26:41,43:56)],
                                                      collapse = "+"),sep = "")),
                           data = train_TET, ntree=1000,keep.forest=T,importance=T)

  TET_Bi_MDAs[,i] <- importance(rf_TET_2,type = 1)[,1]
  TET_Bi_MDGs[,i] <- importance(rf_TET_2, type = 2)[,1]

  TET_2_preds <- predict(rf_TET_2, newdata = test_TET)

  TET_Bi_Accs[i] <- confusionMatrix(test_TET[,25],TET_2_preds)$overall[1]
  TET_Bi_Sens[i] <- confusionMatrix(test_TET[,25],TET_2_preds)$overall[2]
  TET_Bi_Specs[i] <- confusionMatrix(test_TET[,25],TET_2_preds)$overall[3]


}

mean(TET_Bi_Accs)
mean(TET_Bi_Sens)
mean(TET_Bi_Specs)


TET_Bi_MDAs_avgs <- rep(NA, nrow(MDAs))
for(i in 1:nrow(TET_Bi_MDAs)){

  TET_Bi_MDAs_avgs[i] <- mean(TET_Bi_MDAs[i,])

}

TET_Bi_MDAs<-cbind(TET_Bi_MDAs,TET_Bi_MDAs_avgs)
View(TET_Bi_MDAs)
rownames(TET_Bi_MDAs) <- rownames(importance(rf_TET_2,type = 1))

TET_Bi_MDGs_avgs <- rep(NA, nrow(TET_Bi_MDGs))
for(i in 1:nrow(TET_Bi_MDGs)){

  TET_Bi_MDGs_avgs[i] <- mean(TET_Bi_MDGs[i,])

}
TET_Bi_MDGs<-cbind(TET_Bi_MDGs,TET_Bi_MDGs_avgs)
View(TET_Bi_MDGs)
rownames(TET_Bi_MDGs) <- rownames(importance(rf_TET_2,type = 2))

################################################################################
#Multi-class
n <- 10
TET_MC_MDAs <- matrix(NA, nrow = 54, ncol = n)
TET_MC_MDGs <- matrix(NA, nrow = 54, ncol = n)
TET_MC_Accs <- rep(NA, n)

for(i in 1:n){

  rf_TET_4 <- randomForest(as.formula(paste(colnames(Chick_Sal_ARA_mgKg_TET)[25],
                                            "~",paste(colnames(Chick_Sal_ARA_mgKg_TET)[c(1:24,26:41,43:56)],
                                                      collapse = "+"),sep = "")),
                           data = train_TET_4, ntree=1000,keep.forest=T,importance=T)

  TET_MC_MDAs[,i] <- importance(rf_TET_4,type = 1)[,1]
  TET_MC_MDGs[,i] <- importance(rf_TET_4, type = 2)[,1]

  TET_4_preds <- predict(rf_TET_4, newdata = test_TET_4)

  TET_MC_Accs[i] <- confusionMatrix(test_TET_4[,25],TET_4_preds)$overall[1]


}

mean(TET_MC_Accs)



TET_MC_MDAs_avgs <- rep(NA, nrow(TET_MC_MDAs))
for(i in 1:nrow(TET_MC_MDAs)){

  TET_MC_MDAs_avgs[i] <- mean(TET_MC_MDAs[i,])

}

TET_MC_MDAs<-cbind(TET_MC_MDAs,TET_MC_MDAs_avgs)
View(TET_MC_MDAs)
rownames(TET_MC_MDAs) <- rownames(importance(rf_TET_4,type = 1))

TET_MC_MDGs_avgs <- rep(NA, nrow(TET_MC_MDGs))
for(i in 1:nrow(TET_MC_MDGs)){

  TET_MC_MDGs_avgs[i] <- mean(TET_MC_MDGs[i,])

}
TET_MC_MDGs<-cbind(TET_MC_MDGs,TET_MC_MDGs_avgs)
View(TET_MC_MDGs)
rownames(TET_MC_MDGs) <- rownames(importance(rf_TET_4,type = 2))
