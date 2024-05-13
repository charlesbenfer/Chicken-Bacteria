#STR Regression
library(glmnet)
jittered_data[,29] <- as.Date(jittered_data[,29])


jittered_data[,50] <- rep(NA, nrow(jittered_data))
for(i in 1:nrow(jittered_data)){
  if(jittered_data[i,29] < '2014-01-01'){jittered_data[i,50] <- 0}
  if(jittered_data[i,29] >= '2014-01-01' & jittered_data[i,29] < '2018-01-01'){
    jittered_data[i,50] <- 1
  }
  if(jittered_data[i,29] >= '2018-01-01'){jittered_data[i,50] <- 2}
}

colnames(jittered_data)[50] <- 'Ban Level'

x <- data.matrix(jittered_data[,c(30:34, 37:50)])
y <- jittered_data[,23]

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
simple_lambda <- cv_model$lambda.1se
best_lambda


#produce plot of test MSE by lambda value
plot(cv_model)


best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
simple_model <- glmnet(x, y, alpha = 1, lambda = simple_lambda)
coef(best_model)
coef(simple_model)


#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

#save and plot residuals
resids <- y_predicted - y
plot(resids, main = 'Residual Plot', ylab = 'Residual Value')

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

pairs(jittered_data[,c(23, 33:34, 37:40)], pch = 19)
