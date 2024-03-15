#Shapley Values

library(fastshap)
library(tibble)
library(shapviz)

pfun <- function(object, newdata) {  # prediction wrapper
  predict(object, newdata, type='prob', predict.all=F)
}

rand_row <- sample(1:nrow(Chick_Sal_ARA_mgKg),size=1)
rand_pred <- Chick_Sal_ARA_mgKg[rand_row,]

pred <- pfun(rf_TET_2,rand_pred)[1,1]

X <- subset(Chick_Sal_ARA_mgKg, select = -MIC_TET)

shaps <- explain(rf_TET_2, X = X, pred_wrapper = pfun, nsim = 100,
                 adjust = TRUE, shap_only = FALSE)

tibble::as_tibble(shaps$shapley_values)


