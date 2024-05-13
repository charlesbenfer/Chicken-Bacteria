#Interested in
#o MIC_TET (tetracycline)
#o MIC_SSS (sulfamethoxazole)
#o MIC_STR (streptomycin)

library(ggplot2)
library(farver)


Chick_Sal_ARA_mgKg <- read.csv("Chick_Sal_ARA_mgKg.csv", row.names=1)

#Cleaning Region Names
for(i in 1:nrow(Chick_Sal_ARA_mgKg)){

  if(Chick_Sal_ARA_mgKg[i,3] == 'ALBERTA'){Chick_Sal_ARA_mgKg[i,3] <- 'Alberta'}
  if(Chick_Sal_ARA_mgKg[i,3] == 'BRITISH COLUMBIA'){Chick_Sal_ARA_mgKg[i,3] <- 'British Columbia'}
  if(Chick_Sal_ARA_mgKg[i,3] == 'ONTARIO'){Chick_Sal_ARA_mgKg[i,3] <- 'Ontario'}
  if(Chick_Sal_ARA_mgKg[i,3] == 'QUEBEC'){Chick_Sal_ARA_mgKg[i,3] <- 'Quebec'}
  if(Chick_Sal_ARA_mgKg[i,3] == 'SASKATCHEWAN'){Chick_Sal_ARA_mgKg[i,3] <- 'Saskatchewan'}

}

#Finding which variables have too many NA's
nas<-rep(NA,ncol(Chick_Sal_ARA_mgKg))
for(i in 1:ncol(Chick_Sal_ARA_mgKg)){
  nas[i] <- length(which(is.na(Chick_Sal_ARA_mgKg[,i])))
}

View(as.matrix(nas))
colnames(Chick_Sal_ARA_mgKg)[c(14,17,20,66,39,65,47)]
Chick_Sal_ARA_mgKg <- Chick_Sal_ARA_mgKg[,-c(14,17,20,66,39,65,47)]

#Removing Unnecessary 'C' Variables
View(Chick_Sal_ARA_mgKg)
Chick_Sal_ARA_mgKg <- Chick_Sal_ARA_mgKg[,-(29:45)]

#Removing Estab and Region Num variables
Chick_Sal_ARA_mgKg <- Chick_Sal_ARA_mgKg[,-c(5,8)]

#We are going to focus solely on the MIC_STR Variable since it has an even
#Distribution between classes:

Chick_Sal_ARA_mgKg[,23] <- as.factor(Chick_Sal_ARA_mgKg[,23])

levels(Chick_Sal_ARA_mgKg[,23])
Chick_Sal_ARA_mgKg[,23] <- as.character(Chick_Sal_ARA_mgKg[,23])

Chick_Sal_ARA_mgKg <- Chick_Sal_ARA_mgKg[-which(Chick_Sal_ARA_mgKg[,23] == ''),]



#1898 observations

Chick_Sal_ARA_mgKg[,23] <- as.character(Chick_Sal_ARA_mgKg[,23])
for(i in 1:nrow(Chick_Sal_ARA_mgKg)){
  if(Chick_Sal_ARA_mgKg[i,23]=='<=32'){Chick_Sal_ARA_mgKg[i,23]<-'32'}
  if(Chick_Sal_ARA_mgKg[i,23]=='<=2'){Chick_Sal_ARA_mgKg[i,23]<-'1'}
  if(Chick_Sal_ARA_mgKg[i,23]=='>64'){Chick_Sal_ARA_mgKg[i,23]<-'64'}
}

#Jitter the STR data to try and get more of a cloud

Chick_Sal_ARA_mgKg[,23] <- as.numeric(Chick_Sal_ARA_mgKg[,23])
jittered_data <- Chick_Sal_ARA_mgKg
jittered_data[,23] <- jitter(Chick_Sal_ARA_mgKg$MIC_STR, factor = 4)
plot(jittered_data$MIC_STR)

#transform the year data to a factor

jittered_data$YEAR <- as.factor(jittered_data$YEAR)

ggplot(data = jittered_data, aes(x = YEAR, y = MIC_STR)) +
  geom_boxplot(aes(fill = YEAR), width = 0.8 )





Chick_Sal_ARA_mgKg$datecollected_1 <- as.Date(Chick_Sal_ARA_mgKg$datecollected_1)
Chick_Sal_ARA_mgKg <- Chick_Sal_ARA_mgKg[order(Chick_Sal_ARA_mgKg$datecollected_1),]

which(Chick_Sal_ARA_mgKg$datecollected_1 >= '2015-01-01')

tsplot(Chick_Sal_ARA_mgKg$datecollected_1, y=Chick_Sal_ARA_mgKg$MIC_STR)



