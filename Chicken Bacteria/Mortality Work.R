#Mortality Regression
library(olsrr)
library(regclass)
#Convert all of the MIC Variables to Numeric (STR already transformed)

for(i in 11:26){

  Chick_Sal_ARA_mgKg[,i] <- as.factor(Chick_Sal_ARA_mgKg[,i])

}

levels(Chick_Sal_ARA_mgKg[,11])
Chick_Sal_ARA_mgKg[,11] <- as.character(Chick_Sal_ARA_mgKg[,11])
for(i in 1:nrow(Chick_Sal_ARA_mgKg)){
  if(Chick_Sal_ARA_mgKg[i,11] == '<=1'){Chick_Sal_ARA_mgKg[i,11] <- .5}
  if(Chick_Sal_ARA_mgKg[i,11] == '>32'){Chick_Sal_ARA_mgKg[i,11]<- 32}
}
Chick_Sal_ARA_mgKg[,11] <- as.numeric(Chick_Sal_ARA_mgKg[,11])

levels(Chick_Sal_ARA_mgKg[,12])
Chick_Sal_ARA_mgKg[,12] <- as.character(Chick_Sal_ARA_mgKg[,12])
for(i in 1:nrow(Chick_Sal_ARA_mgKg)){
  if(Chick_Sal_ARA_mgKg[i,12] == '<=1'){Chick_Sal_ARA_mgKg[i,12] <- .5}
  if(Chick_Sal_ARA_mgKg[i,12] == '>32'){Chick_Sal_ARA_mgKg[i,12]<- 32}
}
Chick_Sal_ARA_mgKg[,12] <- as.numeric(Chick_Sal_ARA_mgKg[,12])

levels(Chick_Sal_ARA_mgKg[,13])
Chick_Sal_ARA_mgKg[,13] <- as.numeric(Chick_Sal_ARA_mgKg[,13])

levels(Chick_Sal_ARA_mgKg[,14])
Chick_Sal_ARA_mgKg[,14] <- as.character(Chick_Sal_ARA_mgKg[,14])
for(i in 1:nrow(Chick_Sal_ARA_mgKg)){
  if(Chick_Sal_ARA_mgKg[i,14] == '<=2'){Chick_Sal_ARA_mgKg[i,14] <- 1}
  if(Chick_Sal_ARA_mgKg[i,14] == '>32'){Chick_Sal_ARA_mgKg[i,14]<- 32}
}
Chick_Sal_ARA_mgKg[,14] <- as.numeric(Chick_Sal_ARA_mgKg[,14])

levels(Chick_Sal_ARA_mgKg[,15])
Chick_Sal_ARA_mgKg[,15] <- as.character(Chick_Sal_ARA_mgKg[,15])
for(i in 1:nrow(Chick_Sal_ARA_mgKg)){
  if(Chick_Sal_ARA_mgKg[i,15] == '<=0.015'){Chick_Sal_ARA_mgKg[i,15] <- .0075}
}
Chick_Sal_ARA_mgKg[,15] <- as.numeric(Chick_Sal_ARA_mgKg[,15])

levels(Chick_Sal_ARA_mgKg[,16])
Chick_Sal_ARA_mgKg[,16] <- as.character(Chick_Sal_ARA_mgKg[,16])
for(i in 1:nrow(Chick_Sal_ARA_mgKg)){
  if(Chick_Sal_ARA_mgKg[i,16] == '<=0.25'){Chick_Sal_ARA_mgKg[i,16] <- .125}
}
Chick_Sal_ARA_mgKg[,16] <- as.numeric(Chick_Sal_ARA_mgKg[,16])

levels(Chick_Sal_ARA_mgKg[,17])
Chick_Sal_ARA_mgKg[,17] <- as.character(Chick_Sal_ARA_mgKg[,17])
for(i in 1:nrow(Chick_Sal_ARA_mgKg)){
  if(Chick_Sal_ARA_mgKg[i,17] == '<=0.5'){Chick_Sal_ARA_mgKg[i,17] <- .25}
  if(Chick_Sal_ARA_mgKg[i,17] == '>32'){Chick_Sal_ARA_mgKg[i,17] <- 32}
}
Chick_Sal_ARA_mgKg[,17] <- as.numeric(Chick_Sal_ARA_mgKg[,17])

levels(Chick_Sal_ARA_mgKg[,18])
Chick_Sal_ARA_mgKg[,18] <- as.character(Chick_Sal_ARA_mgKg[,18])
for(i in 1:nrow(Chick_Sal_ARA_mgKg)){
  if(Chick_Sal_ARA_mgKg[i,18] == '<=0.25'){Chick_Sal_ARA_mgKg[i,18] <- .125}
  if(Chick_Sal_ARA_mgKg[i,18] == '>16'){Chick_Sal_ARA_mgKg[i,18] <- 16}
}
Chick_Sal_ARA_mgKg[,18] <- as.numeric(Chick_Sal_ARA_mgKg[,18])

levels(Chick_Sal_ARA_mgKg[,19])
length(which(Chick_Sal_ARA_mgKg[,19]==''))
#MIC_KAN must be left out (No Reading for 1669 entries)

levels(Chick_Sal_ARA_mgKg[,20])
length(which(Chick_Sal_ARA_mgKg[,20]==''))
#MIC_MEM must be left our (No reading for 801 entries)

levels(Chick_Sal_ARA_mgKg[,21])
Chick_Sal_ARA_mgKg[,21] <- as.character(Chick_Sal_ARA_mgKg[,21])
for(i in 1:nrow(Chick_Sal_ARA_mgKg)){
  if(Chick_Sal_ARA_mgKg[i,21] == '<=0.5'){Chick_Sal_ARA_mgKg[i,21] <- .25}
  if(Chick_Sal_ARA_mgKg[i,21] == '>32'){Chick_Sal_ARA_mgKg[i,21] <- 32}
}
Chick_Sal_ARA_mgKg[,21] <- as.numeric(Chick_Sal_ARA_mgKg[,21])

levels(Chick_Sal_ARA_mgKg[,22])
Chick_Sal_ARA_mgKg[,22] <- as.character(Chick_Sal_ARA_mgKg[,22])
for(i in 1:nrow(Chick_Sal_ARA_mgKg)){
  if(Chick_Sal_ARA_mgKg[i,22] == '<=16'){Chick_Sal_ARA_mgKg[i,22] <- 8}
  if(Chick_Sal_ARA_mgKg[i,22] == '>256'){Chick_Sal_ARA_mgKg[i,22] <- 256}
}
Chick_Sal_ARA_mgKg[,22] <- as.numeric(Chick_Sal_ARA_mgKg[,22])

levels(Chick_Sal_ARA_mgKg[,23])
Chick_Sal_ARA_mgKg[,23] <- as.numeric(Chick_Sal_ARA_mgKg[,23])

levels(Chick_Sal_ARA_mgKg[,24])
Chick_Sal_ARA_mgKg[,24] <- as.character(Chick_Sal_ARA_mgKg[,24])
for(i in 1:nrow(Chick_Sal_ARA_mgKg)){
  if(Chick_Sal_ARA_mgKg[i,24] == '<=0.12'){Chick_Sal_ARA_mgKg[i,24] <- .06}
  if(Chick_Sal_ARA_mgKg[i,24] == '>4'){Chick_Sal_ARA_mgKg[i,24] <- 4}
}
Chick_Sal_ARA_mgKg[,24] <- as.numeric(Chick_Sal_ARA_mgKg[,24])

levels(Chick_Sal_ARA_mgKg[,25])
Chick_Sal_ARA_mgKg[,25] <- as.character(Chick_Sal_ARA_mgKg[,25])
for(i in 1:nrow(Chick_Sal_ARA_mgKg)){
  if(Chick_Sal_ARA_mgKg[i,25] == '<=4'){Chick_Sal_ARA_mgKg[i,25] <- 2}
  if(Chick_Sal_ARA_mgKg[i,25] == '>32'){Chick_Sal_ARA_mgKg[i,25] <- 32}
}
Chick_Sal_ARA_mgKg[,25] <- as.numeric(Chick_Sal_ARA_mgKg[,25])

levels(Chick_Sal_ARA_mgKg[,26])
length(which(Chick_Sal_ARA_mgKg[,26]==''))
#MIC_TIO must be left out (No reading for 1097 observations)

#Add Ban Level Column

Chick_Sal_ARA_mgKg[,50] <- rep(NA, nrow(Chick_Sal_ARA_mgKg))
for(i in 1:nrow(Chick_Sal_ARA_mgKg)){
  if(Chick_Sal_ARA_mgKg[i,29] < '2014-01-01'){Chick_Sal_ARA_mgKg[i,50] <- 0}
  if(Chick_Sal_ARA_mgKg[i,29] >= '2014-01-01' & Chick_Sal_ARA_mgKg[i,29] < '2018-01-01'){
    Chick_Sal_ARA_mgKg[i,50] <- 1
  }
  if(Chick_Sal_ARA_mgKg[i,29] >= '2018-01-01'){Chick_Sal_ARA_mgKg[i,50] <- 2}
}

colnames(Chick_Sal_ARA_mgKg)[50] <- 'Ban_Level'

################################################################################

#Regress on Mortality % Floor




x <- data.matrix(Chick_Sal_ARA_mgKg[,c(11:18, 21:25 ,30:32, 37:50)])
y <- Chick_Sal_ARA_mgKg[,34]

full_model <- lm(as.formula(paste(colnames(Chick_Sal_ARA_mgKg)[34],
                                  "~",paste(colnames(Chick_Sal_ARA_mgKg)[c(11:18,21:25, 30:32,
                                                                           37:50)],
                                            collapse = "+"),sep = "")),
                 data = Chick_Sal_ARA_mgKg)
plot(full_model)

forward <- ols_step_forward_p(full_model, details = T, penter = .05)
plot(forward)
model_f <- lm(Mortality_percent_floor ~ ara_BACI_mgKg +
                stocking_density + ara_FQLS_mgKg + MIC_NAL + farm_capacity_n +
                ara_MACR_mgKg + ara_STRE_mgKg + ara_ORTH_mgKg + MIC_AMC + MIC_SSS +
                MIC_AZM + ara_PENI_mgKg + ara_FLAV_mgKg + ara_AMIN_mgKg + MIC_GEN +
                ara_TMPS_mgKg + MIC_STR + MIC_CIP,
              data = Chick_Sal_ARA_mgKg)
plot(model_f)
anova(model_f)
ols_mallows_cp(model_f, full_model)
#CP appx 11.98

backward <- ols_step_backward_p(full_model, details = T, prem = .05)
plot(backward)
model_b <- lm(Mortality_percent_floor ~ MIC_AMP + MIC_AZM + MIC_CIP + MIC_GEN +
                MIC_NAL + MIC_SSS + MIC_STR + stocking_density + farm_capacity_n +
                ara_FQLS_mgKg + ara_AMIN_mgKg + ara_LINC_AMCL_mgKg + ara_MACR_mgKg +
                ara_PENI_mgKg + ara_STRE_mgKg + ara_TMPS_mgKg + ara_BACI_mgKg +
                ara_FLAV_mgKg + ara_ORTH_mgKg,
               data = Chick_Sal_ARA_mgKg)
plot(model_b)
anova(model_b)
ols_mallows_cp(model_b, full_model)
#CP appx 12.94

stepwise <- ols_step_both_p(full_model, details = T, p_remove = .051, p_enter = .05)
plot(stepwise)
model_s <- lm( Mortality_percent_floor ~ ara_BACI_mgKg + stocking_density +
                 ara_FQLS_mgKg + MIC_NAL + farm_capacity_n + ara_MACR_mgKg +
                 ara_STRE_mgKg + ara_ORTH_mgKg + MIC_AMC + MIC_SSS + MIC_AZM +
                 ara_PENI_mgKg + ara_FLAV_mgKg,
               data = Chick_Sal_ARA_mgKg)
plot(model_s)
summary(model_s)
anova(model_s)
ols_mallows_cp(model_s, full_model)
VIF(model_s)
#CP appx 11.75, low VIF so no MCL

#Very Poor Results, Try Mortality % Barn

x <- data.matrix(Chick_Sal_ARA_mgKg[,c(11:18, 21:25 ,30:32, 37:50)])
y <- Chick_Sal_ARA_mgKg[,33]

full_model <- lm(as.formula(paste(colnames(Chick_Sal_ARA_mgKg)[33],
                                  "~",paste(colnames(Chick_Sal_ARA_mgKg)[c(11:18,21:25, 30:32,
                                                                           37:50)],
                                            collapse = "+"),sep = "")),
                 data = Chick_Sal_ARA_mgKg)
plot(full_model)
anova(full_model)



forward <- ols_step_forward_p(full_model, details = T, penter = .05)
plot(forward)
model_f <- lm(Mortality_percent_barn ~ age_sampled + MIC_TET + ara_BACI_mgKg +
                ara_ORTH_mgKg + ara_FLAV_mgKg + Ban_Level + stocking_density +
                ara_TMPS_mgKg + ara_FQLS_mgKg + MIC_AMP + ara_STRE_mgKg + ara_SULF_mgKg +
                MIC_STR + MIC_CIP + ara_MACR_mgKg,
              data = Chick_Sal_ARA_mgKg)
plot(model_f)
anova(model_f)
ols_mallows_cp(model_f, full_model)

#CP approx 8.62

backward <- ols_step_backward_p(full_model, details = T, prem = .05)
plot(backward)
model_b <- lm(Mortality_percent_barn ~ MIC_AMP + MIC_CIP + MIC_STR + MIC_TET +
                age_sampled + stocking_density + ara_FQLS_mgKg + ara_LINC_AMCL_mgKg +
                ara_STRE_mgKg + ara_TMPS_mgKg + ara_BACI_mgKg + ara_SULF_mgKg +
                ara_FLAV_mgKg + ara_ORTH_mgKg + Ban_Level,
              data = Chick_Sal_ARA_mgKg)
plot(model_b)
anova(model_b)
ols_mallows_cp(model_b, full_model)
#CP approx 9.80

stepwise <- ols_step_both_p(full_model, details = T, p_remove = .051, p_enter = .05)
plot(stepwise)
model_s <- lm( Mortality_percent_barn ~ age_sampled + MIC_TET + ara_BACI_mgKg +
                 ara_ORTH_mgKg + ara_FLAV_mgKg + Ban_Level + stocking_density +
                 ara_TMPS_mgKg + ara_FQLS_mgKg + MIC_AMP + ara_STRE_mgKg ,
               data = Chick_Sal_ARA_mgKg)
plot(model_s)
summary(model_s)
anova(model_s)
ols_mallows_cp(model_s, full_model)
VIF(model_s)
#CP approx 8.52, again low VIF






