#Load CSV with reduced features
Chick_Sal_Reduced <- read.csv("Chick_Sal_Reduced.csv", row.names=1)


#Fixing high NA values

nas<-rep(NA,ncol(Chick_Sal_Reduced))
for(i in 1:ncol(Chick_Sal_Reduced)){
  nas[i] <- length(which(is.na(Chick_Sal_Reduced[,i])))
}
View(as.matrix(nas))

#Removing variables with high NA values

Chick_Sal_Reduced <- Chick_Sal_Reduced[,-c(13,16,19,45,44)]


#Transform


###############################################################################

#Try an RF on the Reduced Data

Chick_Sal_Reduced$MIC_TET <- as.character(Chick_Sal_Reduced$MIC_TET)
for(i in 1:nrow(Chick_Sal_Reduced)){

  if(Chick_Sal_Reduced$MIC_TET[i] == '<= 4'||Chick_Sal_Reduced$MIC_TET[i] =='<=4'||Chick_Sal_Reduced$MIC_TET[i] =='8'){
    Chick_Sal_Reduced$MIC_TET[i] <- '<=8'
  }

  else Chick_Sal_Reduced$MIC_TET[i] <- '>=16'

}

Chick_Sal_Reduced$MIC_TET <- as.factor(Chick_Sal_Reduced$MIC_TET)
levels(Chick_Sal_Reduced$MIC_TET)


#Split data into training and testing data

ind <- sample(2, nrow(Chick_Sal_Reduced), replace = T,prob = c(.7,.3))
train_TET <- Chick_Sal_Reduced[ind==1,]
test_TET <- Chick_Sal_Reduced[ind==2,]

#########
#MIC_TET#
#########

#Train a RF for MIC_TET on the train data based on all variables (Excluding itself and "CTET" Variable)

rf_TET_2 <- randomForest(as.formula(paste(colnames(Chick_Sal_Reduced)[26],
                                          "~",paste(colnames(Chick_Sal_Reduced)[c(1:25,27:49)],
                                                    collapse = "+"),sep = "")),
                         data = train_TET, ntree=1000,keep.forest=T,importance=T,
                         na.action = na.omit)

importance(rf_TET_2,type = 1)
importance(rf_TET_2, type = 2)
nrow(importance(rf_TET_2, type = 2))
varImpPlot(rf_TET_2)
rf_TET_2$confusion

# Final ID Serotype has massive comparative accuracy decrease, what is this variable?

#Prediction with generated random forest
TET_2_preds <- predict(rf_TET_2, newdata = test_TET)
confusionMatrix(test_TET[,26],TET_2_preds)

