library(astsa)
library(lubridate)
library(zoo)
library(imputeTS)

Chick_Sal_ARA_mgKg$datecollected_1 <- as.Date(Chick_Sal_ARA_mgKg$datecollected_1)
Chick_Sal_ARA_mgKg <- Chick_Sal_ARA_mgKg[order(Chick_Sal_ARA_mgKg$datecollected_1),]
Chick_Sal_ARA_mgKg$MIC_STR <- as.numeric(Chick_Sal_ARA_mgKg$MIC_STR)

#Create a time-series data set that finds the monthly average versus the day
Chick_Sal_ARA_mgKg$datecollected_1 <- format(Chick_Sal_ARA_mgKg$datecollected_1, format='%Y-%m')
Chick_Sal_ARA_mgKg$datecollected_1 <- as.factor(Chick_Sal_ARA_mgKg$datecollected_1)
length(levels(Chick_Sal_ARA_mgKg$datecollected_1))

month_avgs <- rep(NA, length(levels(Chick_Sal_ARA_mgKg$datecollected_1)))
store <- c()
for(month in 1:length(levels(Chick_Sal_ARA_mgKg$datecollected_1))){
  for(obs in 1:nrow(Chick_Sal_ARA_mgKg)){
    if(Chick_Sal_ARA_mgKg$datecollected_1[obs] == levels(Chick_Sal_ARA_mgKg$datecollected_1)[month]){

      store <- append(store,Chick_Sal_ARA_mgKg$MIC_STR[obs])

    }
  }
  month_avgs[month] <- mean(store)
  store <- c()
}

df_tet <- data.frame(date = levels(Chick_Sal_ARA_mgKg$datecollected_1), average_MIC=month_avgs)
df_tet_hold <- rbind(df_tet[1:8,], c('2014-01', NA))
df_tet_hold <- rbind(df_tet_hold, df_tet[9:19,])
df_tet_hold <- rbind(df_tet_hold, c('2015-01', NA))
df_tet_hold <- rbind(df_tet_hold, df_tet[20:23,])
df_tet_hold <- rbind(df_tet_hold, c('2015-06', NA))
df_tet <- rbind(df_tet_hold, df_tet[24:77,])
df_tet$average_MIC <- as.numeric(df_tet$average_MIC)

ts <- ts(df_tet$average_MIC, start=c(2013,5), end = c(2019,12), frequency = 12)
ts_nona <- na_kalman(ts)
ts.plot(ts, xlab = "Month of Collection", ylab = 'Average MIC_STR')
ts.plot(ts_nona, xlab = "Date of Collection", ylab = 'Average MIC_STR')

abline(reg = lm(ts~time(ts)))

#Analysis of the TS
acf(ts_nona)
AR <- arima(ts_nona, order = c(1,1,1))
print(AR)

ts.plot(ts_nona)
AR_fit <- ts_nona - residuals(AR)
points(AR_fit, type = "l", col = 2, lty = 2)

ts.plot(ts_nona, xlim = c(2013, 2022), ylim = c(0,120), main = 'Monthly Average MIC_STR Level',
        ylab = 'MIC_STR', xlab = 'Date of Sampling')
AR_forecast <- predict(AR, n.ahead = 24)$pred
AR_forecast_se <- predict(AR, n.ahead = 24)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)




which(Chick_Sal_ARA_mgKg$datecollected_1 >= '2015-01-01')
bans <- rep(NA, nrow(Chick_Sal_ARA_mgKg))
Chick_Sal_ARA_mgKg<- cbind(Chick_Sal_ARA_mgKg, bans)


