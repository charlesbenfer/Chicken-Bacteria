#########
#MIC_STR#
#########

#Changing the STR variable to binary
Chick_Sal_ARA_mgKg_STR[,23] <- as.character(Chick_Sal_ARA_mgKg_STR[,23])
for(i in 1:nrow(Chick_Sal_ARA_mgKg_STR)){
  if(Chick_Sal_ARA_mgKg_STR[i,23]=='32'){Chick_Sal_ARA_mgKg_STR[i,23]<-'<=32'}
}

for(i in 1:nrow(Chick_Sal_ARA_mgKg_STR)){
  if(Chick_Sal_ARA_mgKg_STR[i,23]=='>64'||Chick_Sal_ARA_mgKg_STR[i,23]=='64'){Chick_Sal_ARA_mgKg_STR[i,23]<-'>=64'}
  else Chick_Sal_ARA_mgKg_STR[i,23]<-'<=32'
}

Chick_Sal_ARA_mgKg_STR[,23] <- as.factor(Chick_Sal_ARA_mgKg_STR[,23])
levels(Chick_Sal_ARA_mgKg_STR[,23])
length(which(Chick_Sal_ARA_mgKg_STR[,23] == '>=64'))


ind <- sample(2, nrow(Chick_Sal_ARA_mgKg_STR), replace = T,prob = c(.7,.3))
train_STR <- Chick_Sal_ARA_mgKg_STR[ind==1,]
test_STR <- Chick_Sal_ARA_mgKg_STR[ind==2,]

#Train a RF for MIC_SSS on the train data based on columns 40-56
rf_STR_2 <- randomForest(as.formula(paste(colnames(Chick_Sal_ARA_mgKg_STR)[23],
                                        "~",paste(colnames(Chick_Sal_ARA_mgKg)[c(1:22,24:65)],
                                                  collapse = "+"),sep = "")),
                       data = train, ntree=1000,keep.forest=T,importance=T)

nrow(importance(rf_STR,type=1))
varImpPlot(rf_STR)
rf_STR$confusion
# Final ID Serotype has massive comparative accuracy decrease, what is this variable?

#Prediction with generated random forest
STR_preds <- predict(rf_STR, newdata = test)
confusionMatrix(test[,23],STR_preds)


rf_STR_8 <- randomForest(as.formula(paste(colnames(Chick_Sal_ARA_mgKg_STR)[23],
                                          "~",paste(colnames(Chick_Sal_ARA_mgKg)[c(1:22,24:56)],
                                                    collapse = "+"),sep = "")),
                         data = train_STR, ntree=1000,keep.forest=T,importance=T)

nrow(importance(rf_STR_8,type=1))
varImpPlot(rf_STR)
rf_STR$confusion
# Final ID Serotype has massive comparative accuracy decrease, what is this variable?

#Prediction with generated random forest
STR_preds <- predict(rf_STR, newdata = test)
confusionMatrix(test[,23],STR_preds)

#Pretty low success when predicting on these 7 overlapping classes, may want some
#guidance on how to relabel these.

################################################################################

#Run several random forests and find means for MDA, MDG, accuracy, sensitivity and specificity

#Binary
n <- 10
STR_Bi_MDAs <- matrix(NA, nrow = 55, ncol = n)
STR_Bi_MDGs <- matrix(NA, nrow = 55, ncol = n)
STR_Bi_Accs <- rep(NA, n)
STR_Bi_Sens <- rep(NA, n)
STR_Bi_Specs <- rep(NA, n)


for(i in 1:n){

  rf_STR_2 <- randomForest(as.formula(paste(colnames(Chick_Sal_ARA_mgKg_STR)[23],
                                            "~",paste(colnames(Chick_Sal_ARA_mgKg)[c(1:22,24:65)],
                                                      collapse = "+"),sep = "")),
                           data = train_STR, ntree=1000,keep.forest=T,importance=T)

  STR_Bi_MDAs[,i] <- importance(rf_STR_2,type = 1)[,1]
  STR_Bi_MDGs[,i] <- importance(rf_STR_2, type = 2)[,1]

  STR_2_preds <- predict(rf_STR_2, newdata = test_STR)

  STR_Bi_Accs[i] <- confusionMatrix(test_STR[,23],STR_2_preds)$overall[1]
  STR_Bi_Sens[i] <- confusionMatrix(test_STR[,23],STR_2_preds)$overall[2]
  STR_Bi_Specs[i] <- confusionMatrix(test_STR[,23],STR_2_preds)$overall[3]


}

mean(STR_Bi_Accs)
mean(STR_Bi_Sens)
mean(STR_Bi_Specs)


STR_Bi_MDAs_avgs <- rep(NA, nrow(STR_Bi_MDAs))
for(i in 1:nrow(STR_Bi_MDAs)){

  STR_Bi_MDAs_avgs[i] <- mean(STR_Bi_MDAs[i,])

}

STR_Bi_MDAs<-cbind(STR_Bi_MDAs,STR_Bi_MDAs_avgs)
View(STR_Bi_MDAs)
rownames(STR_Bi_MDAs) <- rownames(importance(rf_STR_2,type = 1))

STR_Bi_MDGs_avgs <- rep(NA, nrow(STR_Bi_MDGs))
for(i in 1:nrow(STR_Bi_MDGs)){

  STR_Bi_MDGs_avgs[i] <- mean(STR_Bi_MDGs[i,])

}
STR_Bi_MDGs<-cbind(STR_Bi_MDGs,STR_Bi_MDGs_avgs)
View(STR_Bi_MDGs)
rownames(STR_Bi_MDGs) <- rownames(importance(rf_STR_2,type = 2))


#MultiClass
n <- 10
STR_MC_MDAs <- matrix(NA, nrow = 64, ncol = n)
STR_MC_MDGs <- matrix(NA, nrow = 64, ncol = n)
STR_MC_Accs <- rep(NA, n)
STR_MC_Sens <- rep(NA, n)
STR_MC_Specs <- rep(NA, n)


for(i in 1:n){

  rf_STR_8 <- randomForest(as.formula(paste(colnames(Chick_Sal_ARA_mgKg_STR)[23],
                                            "~",paste(colnames(Chick_Sal_ARA_mgKg)[c(1:22,24:65)],
                                                      collapse = "+"),sep = "")),
                           data = train_STR, ntree=1000,keep.forest=T,importance=T)

  STR_MC_MDAs[,i] <- importance(rf_STR_8,type = 1)[,1]
  STR_MC_MDGs[,i] <- importance(rf_STR_8, type = 2)[,1]

  STR_8_preds <- predict(rf_STR_8, newdata = test_STR)

  STR_MC_Accs[i] <- confusionMatrix(test_STR[,23],STR_8_preds)$overall[1]



}

mean(STR_MC_Accs)


STR_MC_MDAs_avgs <- rep(NA, nrow(STR_MC_MDAs))
for(i in 1:nrow(STR_MC_MDAs)){

  STR_MC_MDAs_avgs[i] <- mean(STR_MC_MDAs[i,])

}

STR_MC_MDAs<-cbind(STR_MC_MDAs,STR_MC_MDAs_avgs)
View(STR_MC_MDAs)
rownames(STR_MC_MDAs) <- rownames(importance(rf_STR_8,type = 1))

STR_MC_MDGs_avgs <- rep(NA, nrow(STR_MC_MDGs))
for(i in 1:nrow(STR_MC_MDGs)){

  STR_MC_MDGs_avgs[i] <- mean(STR_MC_MDGs[i,])

}
STR_MC_MDGs<-cbind(STR_MC_MDGs,STR_MC_MDGs_avgs)
View(STR_MC_MDGs)
rownames(STR_MC_MDGs) <- rownames(importance(rf_STR_8,type = 2))
