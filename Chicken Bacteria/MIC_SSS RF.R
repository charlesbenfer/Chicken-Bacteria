#########
#MIC_SSS#
#########

#Changing to binary
Chick_Sal_ARA_mgKg[,22]<-as.character(Chick_Sal_ARA_mgKg[,22])
for(i in 1:nrow(Chick_Sal_ARA_mgKg)){

  if(Chick_Sal_ARA_mgKg[i,22] == '<= 16' || Chick_Sal_ARA_mgKg[i,22] == '<=16'){Chick_Sal_ARA_mgKg[i,22] <- '<=16'}
  else Chick_Sal_ARA_mgKg[i,22] <- '>=32'

}

Chick_Sal_ARA_mgKg[,22] <- as.factor(Chick_Sal_ARA_mgKg[,22])
levels(Chick_Sal_ARA_mgKg[,22])

ind <- sample(2, nrow(Chick_Sal_ARA_mgKg), replace = T,prob = c(.7,.3))
train_SSS <- Chick_Sal_ARA_mgKg[ind==1,]
test_SSS <- Chick_Sal_ARA_mgKg[ind==2,]

#Train a RF for MIC_SSS on the train data based on columns 40-56
rf_SSS_2 <- randomForest(as.formula(paste(colnames(Chick_Sal_ARA_mgKg)[22],
                                        "~",paste(colnames(Chick_Sal_ARA_mgKg)[c(1:21,23:39,41:56)],
                                                  collapse = "+"),sep = "")),
                       data = train_SSS, ntree=1000,keep.forest=T,importance=T)

importance(rf_SSS_2,type=1)
varImpPlot(rf_SSS_2)
rf_SSS_2$confusion
# Final ID Serotype has massive comparative accuracy decrease, what is this variable?

#Prediction with generated random forest
SSS_preds_2 <- predict(rf_SSS_2, newdata = test_SSS)
confusionMatrix(test_SSS[,22],SSS_preds_2)

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

Chick_Sal_ARA_mgKg_SSS[,22] <- as.factor(Chick_Sal_ARA_mgKg_SSS[,22])
levels(Chick_Sal_ARA_mgKg_SSS[,22])
Chick_Sal_ARA_mgKg_SSS[,22] <- as.character(Chick_Sal_ARA_mgKg_SSS[,22])

for(i in 1:nrow(Chick_Sal_ARA_mgKg_SSS)){
  if(Chick_Sal_ARA_mgKg_SSS[i,22] == '<= 16' || Chick_Sal_ARA_mgKg_SSS[i,22] == '<=16'){
    Chick_Sal_ARA_mgKg_SSS[i,22] <- '<=16'
  }
  if(Chick_Sal_ARA_mgKg_SSS[i,22] == '> 256' ||Chick_Sal_ARA_mgKg_SSS[i,22] == '>256'
     ||Chick_Sal_ARA_mgKg_SSS[i,22] == '256'){Chick_Sal_ARA_mgKg_SSS[i,22] <- '>=256'}
}
Chick_Sal_ARA_mgKg_SSS[,22]<-as.factor(Chick_Sal_ARA_mgKg_SSS[,22])
levels(Chick_Sal_ARA_mgKg_SSS[,22])
length(which(Chick_Sal_ARA_mgKg_SSS[,22]=='>=256'))

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
#Similar to above

################################################################################

#Run several random forests and find means for MDA, MDG, accuracy, sensitivity and specificity

#Binary
n <- 10
SSS_Bi_MDAs <- matrix(NA, nrow = 54, ncol = n)
SSS_Bi_MDGs <- matrix(NA, nrow = 54, ncol = n)
SSS_Bi_Accs <- rep(NA, n)
SSS_Bi_Sens <- rep(NA, n)
SSS_Bi_Specs <- rep(NA, n)


for(i in 1:n){

  rf_SSS_2 <- randomForest(as.formula(paste(colnames(Chick_Sal_ARA_mgKg)[22],
                                            "~",paste(colnames(Chick_Sal_ARA_mgKg)[c(1:21,23:39,41:56)],
                                                      collapse = "+"),sep = "")),
                           data = train_SSS, ntree=1000,keep.forest=T,importance=T)

  SSS_Bi_MDAs[,i] <- importance(rf_SSS_2,type = 1)[,1]
  SSS_Bi_MDGs[,i] <- importance(rf_SSS_2, type = 2)[,1]

  SSS_2_preds <- predict(rf_SSS_2, newdata = test_SSS)

  SSS_Bi_Accs[i] <- confusionMatrix(test_SSS[,22],SSS_2_preds)$overall[1]
  SSS_Bi_Sens[i] <- confusionMatrix(test_SSS[,22],SSS_2_preds)$overall[2]
  SSS_Bi_Specs[i] <- confusionMatrix(test_SSS[,22],SSS_2_preds)$overall[3]


}

mean(SSS_Bi_Accs)
mean(SSS_Bi_Sens)
mean(SSS_Bi_Specs)


SSS_Bi_MDAs_avgs <- rep(NA, nrow(SSS_Bi_MDAs))
for(i in 1:nrow(SSS_Bi_MDAs)){

  SSS_Bi_MDAs_avgs[i] <- mean(SSS_Bi_MDAs[i,])

}

SSS_Bi_MDAs<-cbind(SSS_Bi_MDAs,SSS_Bi_MDAs_avgs)
View(SSS_Bi_MDAs)
rownames(SSS_Bi_MDAs) <- rownames(importance(rf_SSS_2,type = 1))

SSS_Bi_MDGs_avgs <- rep(NA, nrow(SSS_Bi_MDGs))
for(i in 1:nrow(SSS_Bi_MDGs)){

  SSS_Bi_MDGs_avgs[i] <- mean(SSS_Bi_MDGs[i,])

}
SSS_Bi_MDGs<-cbind(SSS_Bi_MDGs,SSS_Bi_MDGs_avgs)
View(SSS_Bi_MDGs)
rownames(SSS_Bi_MDGs) <- rownames(importance(rf_SSS_2,type = 2))

################################################################################

#MultiClass

Chick_Sal_ARA_mgKg_SSS[,22] <- as.factor(Chick_Sal_ARA_mgKg_SSS[,22])
levels(Chick_Sal_ARA_mgKg_SSS[,22])
Chick_Sal_ARA_mgKg_SSS[,22] <- as.character(Chick_Sal_ARA_mgKg_SSS[,22])

for(i in 1:nrow(Chick_Sal_ARA_mgKg_SSS)){
  if(Chick_Sal_ARA_mgKg_SSS[i,22] == '> 256' ||Chick_Sal_ARA_mgKg_SSS[i,22] == '>256'
     ||Chick_Sal_ARA_mgKg_SSS[i,22] == '256'){Chick_Sal_ARA_mgKg_SSS[i,22] <- '>=256'}
}
Chick_Sal_ARA_mgKg_SSS[,22]<-as.factor(Chick_Sal_ARA_mgKg_SSS[,22])
levels(Chick_Sal_ARA_mgKg_SSS[,22])

ind <- sample(2, nrow(Chick_Sal_ARA_mgKg_SSS), replace = T,prob = c(.7,.3))
train_SSS <- Chick_Sal_ARA_mgKg_SSS[ind==1,]
length(which(train_SSS[,22] == '128'))
test_SSS <- Chick_Sal_ARA_mgKg_SSS[ind==2,]

n <- 10
SSS_MC_MDAs <- matrix(NA, nrow = 54, ncol = n)
SSS_MC_MDGs <- matrix(NA, nrow = 54, ncol = n)
SSS_MC_Accs <- rep(NA, n)
SSS_MC_Sens <- rep(NA, n)
SSS_MC_Specs <- rep(NA, n)

for (i in 1:n) {

  rf_SSS_5 <- randomForest(as.formula(paste(colnames(Chick_Sal_ARA_mgKg_SSS)[22],
                                            "~",paste(colnames(Chick_Sal_ARA_mgKg_SSS)[c(1:21,23:39,41:56)],
                                                      collapse = "+"),sep = "")),
                           data = train_SSS, ntree=1000,keep.forest=T,importance=T)

  SSS_MC_MDAs[,i] <- importance(rf_SSS_5,type = 1)[,1]
  SSS_MC_MDGs[,i] <- importance(rf_SSS_5, type = 2)[,1]

  SSS_5_preds <- predict(rf_SSS_5, newdata = test_SSS)

  SSS_MC_Accs[i] <- confusionMatrix(test_SSS[,22],SSS_5_preds)$overall[1]

}

mean(SSS_MC_Accs)


SSS_MC_MDAs_avgs <- rep(NA, nrow(SSS_MC_MDAs))
for(i in 1:nrow(SSS_MC_MDAs)){

  SSS_MC_MDAs_avgs[i] <- mean(SSS_MC_MDAs[i,])

}

SSS_MC_MDAs<-cbind(SSS_MC_MDAs,SSS_MC_MDAs_avgs)
View(SSS_MC_MDAs)
rownames(SSS_MC_MDAs) <- rownames(importance(rf_SSS_5,type = 1))

SSS_MC_MDGs_avgs <- rep(NA, nrow(SSS_MC_MDGs))
for(i in 1:nrow(SSS_MC_MDGs)){

  SSS_MC_MDGs_avgs[i] <- mean(SSS_MC_MDGs[i,])

}
SSS_MC_MDGs<-cbind(SSS_MC_MDGs,SSS_MC_MDGs_avgs)
View(SSS_MC_MDGs)
rownames(SSS_MC_MDGs) <- rownames(importance(rf_SSS_5,type = 2))

