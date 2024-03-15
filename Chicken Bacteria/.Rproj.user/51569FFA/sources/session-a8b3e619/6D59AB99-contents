#########
#MIC_STR#
#########

ind <- sample(2, nrow(Chick_Sal_ARA_mgKg_STR), replace = T,prob = c(.7,.3))
train <- Chick_Sal_ARA_mgKg_STR[ind==1,]
test <- Chick_Sal_ARA_mgKg_STR[ind==2,]

#Train a RF for MIC_SSS on the train data based on columns 40-56
rf_STR <- randomForest(as.formula(paste(colnames(Chick_Sal_ARA_mgKg_STR)[23],
                                        "~",paste(colnames(Chick_Sal_ARA_mgKg)[c(1:22,24:56)],
                                                  collapse = "+"),sep = "")),
                       data = train, ntree=1000,keep.forest=T,importance=T)

importance(rf_STR,type=1)
varImpPlot(rf_STR)
rf_STR$confusion
# Final ID Serotype has massive comparative accuracy decrease, what is this variable?

#Prediction with generated random forest
STR_preds <- predict(rf_STR, newdata = test)
confusionMatrix(test[,23],STR_preds)

#Pretty low success when predicting on these 7 overlapping classes, may want some
#guidance on how to relabel these.
