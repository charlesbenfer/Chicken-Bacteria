#Multinomial Regression for MIC_STR
library(glmnet)
#Create a new column vector that represents the STR variable as Binary
binary_str <- Chick_Sal_ARA_mgKg[,23]
binary_str <- as.character(binary_str)
for(i in 1:length(binary_str)){
  if(binary_str[i]=='32'){binary_str[i]<-'<=32'}
}

for(i in 1:length(binary_str)){
  if(binary_str[i]=='>64'||binary_str[i]=='64'){binary_str[i]<-'>=64'}
  else binary_str[i]<-'<=32'
}

binary_str <- as.numeric(binary_str)

# Find the best lambda using cross-validation
set.seed(123)
cv.lasso <- cv.glmnet(as.matrix(Chick_Sal_ARA_mgKg[,c(30:34,37:49)]), Chick_Sal_ARA_mgKg[,23], alpha = 1, family = "multinomial")
plot(cv.lasso)

# Fit the final model on the training data
model <- glmnet(as.matrix(Chick_Sal_ARA_mgKg[,c(30:34,37:49)]), Chick_Sal_ARA_mgKg[,23], alpha = 1, family = "multinomial",
                lambda = cv.lasso$lambda.min)
model_simp <- glmnet(as.matrix(Chick_Sal_ARA_mgKg[,c(30:34,37:49)]), binary_str, alpha = 1, family = "binomial",
                     lambda = cv.lasso$lambda.1se)
# Display regression coefficients

coef(model)
#We see than farm_capacity_n and ara_LINC_AMCL_mgKg went to 0 on the optimal lambda

coef(model_simp)
#for the simple model, we see that only Mortality_percent_barn, ara_3GCS_mgKg,
# and ara_AMIN_mgKg, ara_FLAV_mgKg are kept.

ind <- sample(2, nrow(Chick_Sal_ARA_mgKg), replace = T,prob = c(.7,.3))
train_STR <- Chick_Sal_ARA_mgKg[ind==1,]
test_STR <- Chick_Sal_ARA_mgKg[ind==2,]

multi_simple <- multinom(MIC_STR ~ Mortality_percent_barn + ara_3GCS_mgKg
                         + ara_AMIN_mgKg + ara_FLAV_mgKg, data = train_STR)
summary(multi_simple)

z <- summary(multi_simple)$coefficients/summary(multi_simple)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(multi_simple))

head(pp <- fitted(multi_simple))

predict(model_simp, newx = test_STR[])
