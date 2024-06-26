#Shapley Values

library(fastshap)
library(tibble)
library(shapviz)

#Build ranger RF for this

pfun <- function(object, newdata) {  # prediction wrapper
  unname(predict(object, data = newdata)$predictions[, "<=8"])
}





#####################
#Binary TET Shapleys#
#####################
pfun_TET <- function(object, newdata) {  # prediction wrapper
  unname(predict(object, data = newdata)$predictions[, "<=8"])
}

X_TET <- subset(Chick_Sal_ARA_mgKg, select = -MIC_TET)
shaps_TET <- explain(ranger_TET, X = X_TET, pred_wrapper = pfun_TET, nsim = 10, adjust = T,
                 shap_only = F)

tibble::as_tibble(shaps_TET$shapley_values)

#Global Look
shv_TET <- shapviz(shaps_TET)
sv_waterfall(shv_TET,fill_colors = c('green','red'))
sv_importance(shv_TET)


#Individual Point
rand_row <- sample(1:nrow(X_TET),size=1)
rand_pred <- X_TET[rand_row,]
shaps_TET_ind <- explain(ranger_TET, X = X_TET, pred_wrapper = pfun_TET
                         , newdata = X_TET[1617,])

probs <- rep(NA,nrow(Chick_Sal_ARA_mgKg))
for(i in 1:nrow(Chick_Sal_ARA_mgKg)){probs[i] <- pfun(ranger_TET,Chick_Sal_ARA_mgKg[i,])[1]}
baseline <- mean(probs)

pred <- pfun(ranger_TET,rand_pred)[1]
pred

shv_TET_ind <- shapviz(shaps_TET_ind, X=X_TET[1617,],baseline = baseline)
sv_waterfall(shv_TET_ind,fill_colors = c('green','red'))
sv_importance(shv_TET)


#####################
#Binary SSS Shapleys#
#####################

pfun_SSS <- function(object, newdata) {  # prediction wrapper
  unname(predict(object, data = newdata)$predictions[, "<=16"])
}

ranger_SSS <- ranger((as.formula(paste(colnames(Chick_Sal_ARA_mgKg)[22],
                                       "~",paste(colnames(Chick_Sal_ARA_mgKg)[c(1:21,23:39,41:56)],
                                                 collapse = "+"),sep = ""))),
                     data = Chick_Sal_ARA_mgKg, probability = TRUE)

X_SSS <- subset(Chick_Sal_ARA_mgKg, select = -MIC_SSS)

shaps_SSS <- explain(ranger_SSS, X = X_SSS, pred_wrapper = pfun_SSS, nsim = 10,
                        adjust = T, shap_only = F)

tibble::as_tibble(shaps_SSS$shapley_values)

shv_SSS <- shapviz(shaps_SSS)
sv_waterfall(shv_SSS,fill_colors = c('green','red'))

rand_row <- sample(1:nrow(X_SSS),size=1)
rand_pred <- X_SSS[rand_row,]
shaps_SSS_ind <- explain(ranger_SSS, X = X_SSS, pred_wrapper = pfun_SSS
                         , newdata = X_SSS[1810,])

probs <- rep(NA,nrow(Chick_Sal_ARA_mgKg))
for(i in 1:nrow(Chick_Sal_ARA_mgKg)){probs[i] <- pfun_SSS(ranger_SSS,Chick_Sal_ARA_mgKg[i,])[1]}
baseline <- mean(probs)

pred <- pfun_SSS(ranger_SSS,rand_pred)[1]
pred

shv_SSS_ind <- shapviz(shaps_SSS_ind, X=X_SSS[1810,],baseline = baseline)
sv_waterfall(shv_SSS_ind,fill_colors = c('green','red'))
sv_importance(shv_TET)
sv_importance(shv_SSS)

#####################
#Binary STR Shapleys#
#####################

pfun_STR <- function(object, newdata) {  # prediction wrapper
  unname(predict(object, data = newdata)$predictions[, "<=32"])
}

ranger_STR <- ranger(as.formula(paste(colnames(Chick_Sal_ARA_mgKg_STR)[23],
                                       "~",paste(colnames(Chick_Sal_ARA_mgKg)[c(1:22,24:65)],
                                                 collapse = "+"),sep = "")),
                     data = train_STR, probability = TRUE)

X_STR <- subset(Chick_Sal_ARA_mgKg_STR, select = -MIC_STR)

shaps_STR <- explain(ranger_STR, X = X_STR, pred_wrapper = pfun_STR, nsim = 10,
                     adjust = T, shap_only = F)

tibble::as_tibble(shaps_STR$shapley_values)

shv_STR <- shapviz(shaps_STR)
sv_waterfall(shv_STR,fill_colors = c('green','red'))
sv_importance(shv_STR)

rand_row <- sample(1:nrow(X_STR),size=1)
rand_pred <- X_STR[rand_row,]
shaps_STR_ind <- explain(ranger_STR, X = X_STR, pred_wrapper = pfun_STR
                         , newdata = rand_pred)
probs <- rep(NA,nrow(Chick_Sal_ARA_mgKg_STR))
for(i in 1:nrow(Chick_Sal_ARA_mgKg_STR)){probs[i] <- pfun_STR(ranger_STR,
                                                          Chick_Sal_ARA_mgKg_STR[i,])[1]}
baseline <- mean(probs)

shv_STR_ind <- shapviz(shaps_STR_ind, X=rand_pred,baseline = baseline)
sv_waterfall(shv_STR_ind,fill_colors = c('green','red'))
sv_importance(shv_TET)
sv_importance(shv_SSS)
