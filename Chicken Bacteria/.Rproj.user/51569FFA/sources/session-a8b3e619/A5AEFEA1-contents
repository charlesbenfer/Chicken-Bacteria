###############
#Random Forest#
###############

#Split data into training and testing data

ind <- sample(2, nrow(Chick_Sal_ARA_mgKg), replace = T,prob = c(.7,.3))
train <- Chick_Sal_ARA_mgKg[ind==1,]
test <- Chick_Sal_ARA_mgKg[ind==2,]

#########
#MIC_TET#
#########

#Train a RF for MIC_TET on the train data based on all variables (Excluding itself and "CTET" Variable)

rf_TET <- randomForest(as.formula(paste(colnames(Chick_Sal_ARA_mgKg)[25],
                                        "~",paste(colnames(Chick_Sal_ARA_mgKg)[c(1:24,26:41,43:56)],
                                                  collapse = "+"),sep = "")),
                       data = train, ntree=1000,keep.forest=T,importance=T)

importance(rf_TET,type=1)
varImpPlot(rf_TET)
rf_TET$confusion
# Final ID Serotype has massive comparative accuracy decrease, what is this variable?

#Prediction with generated random forest
TET_preds <- predict(rf_TET, newdata = test)
confusionMatrix(test[,25],TET_preds)

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
