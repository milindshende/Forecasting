library(forecast)
library(fpp)
library(smooth)
library(readxl)
library(tseries)

airlines<-read_excel(file.choose()) # read the airlines.xlsx data
View(airlines)

# Converting data into time series object
amts<-ts(airlines$Passengers,frequency = 12,start=c(96))
View(amts)

# dividing entire data into training and testing data 
train<-amts[1:84] # 84 months
test<-amts[85:96] # 12 months

# converting time series object
train<-ts(train,frequency = 12)
test<-ts(test,frequency = 12)

# Plotting time series data
plot(amts) # Visualization shows that it has level, trend, seasonality

# Moving Average Model
ma_model<-Arima(train,order = c(0,0,12))
Forecasted<-forecast(ma_model,h=12)
MA<-accuracy(Forecasted,x=as.numeric(test))
MA # MAPE of Training 5.98 & Test 28.35
plot(Forecasted)

#### Models USING HoltWinters function ################
# Default value of alpha=0.2
hw_a<-HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
hw_a
hwa_pred<-data.frame(predict(hw_a,n.ahead=12))
plot(forecast(hw_a,h=12))
hwa_mape<-MAPE(hwa_pred$fit,test)*100
hwa_mape # 17.23

# with default values of alpha = 0.2, beta = 0.15
hw_ab<-HoltWinters(train,alpha = 0.2,beta = 0.15,gamma = F)
hw_ab
hwab_pred<-data.frame(predict(hw_ab,n.ahead = 12))
plot(forecast(hw_ab,h=12))
hwab_mape<-MAPE(hwab_pred$fit,test)*100
hwab_mape # 11.21

# with default values of alpha = 0.2, beta = 0.15, gamma = 0.05 
hw_abg<-HoltWinters(train,alpha = 0.2,beta = 0.15,gamma = 0.05)
hw_abg
hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 12))
plot(forecast(hw_abg,h=12))
hwabg_mape<-MAPE(hwabg_pred$fit,test)*100
hwabg_mape # 7.25

# With optimum values
hw_na<-HoltWinters(train,beta = F,gamma = F)
hw_na # alpha=0.99
hwna_pred<-data.frame(predict(hw_na,n.ahead = 12))
hwna_pred
plot(forecast(hw_na,h=12))
hwna_mape<-MAPE(hwna_pred$fit,test)*100
hwna_mape # 18.55

hw_nab<-HoltWinters(train,gamma=F)
hw_nab #alpha=1, beta=0.0010
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=12))
hwnab_pred
plot(forecast(hw_nab,h=12))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100
hwnab_mape # 13.10

hw_nabg<-HoltWinters(train)
hw_nabg # alpha=0.22,beta=0.04,gamma=1
hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =12))
hwnabg_pred
plot(forecast(hw_nabg,h=12))
hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100
hwnabg_mape # 1.73

######################## MAPE TABLE ########################

df_mape<-data.frame(c("hwa_mape","hwab_mape","hwabg_mape","hwna_mape","hwnab_mape","hwnabg_mape"),c(hwa_mape,hwab_mape,hwabg_mape,hwna_mape,hwnab_mape,hwnabg_mape))

colnames(df_mape)<-c("Model","MAPE")
View(df_mape)

##############################################################
# Based on the MAPE TABLE-> MAPE of 'hwnabg' is minimum (1.73). 
# Build new model

new_model_1 <- HoltWinters(amts)
plot(forecast(new_model_1,n.ahead=12))

# Forecasted values for the next 12 months
forecast_new_1 <- data.frame(predict(new_model_1,n.ahead=12))
forecast_new_1

###############################################################

############## USING ses,holt,hw functions ####################

# Simple Exponential smoothing with default alpha value
ses_a<-ses(train,alpha = 0.2,h=12) 
ses_a
sesa_pred<-data.frame(predict(ses_a,n.ahead=12))
plot(forecast(ses_a,h=12))
sesa_mape<-MAPE(sesa_pred$Point.Forecast,test)*100
sesa_mape # 17.23

# holt with alpha = 0.2, beta = 0.15
holt_ab<-holt(train,alpha = 0.2,beta = 0.15,h=12)
holt_ab
holtab_pred<-data.frame(predict(holt_ab,n.ahead=12))
plot(forecast(holt_ab,h=12))
holtab_mape<-MAPE(holtab_pred$Point.Forecast,test)*100
holtab_mape # 62.89

# hw with alpha = 0.2, beta = 0.15, gamma = 0.05
hw_abg_new<-hw(train,alpha = 0.2,beta = 0.15,gamma = 0.05,h=12)
hw_abg_new
hwabg_pred_new<-data.frame(predict(hw_abg_new,n.ahead=12))
plot(forecast(hw_abg_new,h=12))
hwabg_mape_new<-MAPE(hwabg_pred_new$Point.Forecast,test)*100
hwabg_mape_new # 11.48

# simple exponential method with optimum values
ses_na<-ses(train,alpha=NULL,h=12)
ses_na
sesna_pred<-data.frame(predict(ses_na,h = 12))
sesna_pred
plot(forecast(ses_na,h=12))
sesna_mape<-MAPE(sesna_pred$Point.Forecast,test)*100
sesna_mape # 18.55

# holt method with optimum values
holt_nab<-holt(train,alpha = NULL,beta = NULL,h=12)
holt_nab
holtnab_pred<-data.frame(predict(holt_nab,h=12))
holtnab_pred
plot(forecast(holt_nab,h=12))
holtnab_mape<-MAPE(holtnab_pred$Point.Forecast,test)*100
holtnab_mape # 14.86

# hw method with optimum values
hw_nabg_new<-hw(train,alpha=NULL,beta=NULL,gamma = NULL,h=12)
hw_nabg_new
hwnabg_pred_new<-data.frame(predict(hw_nabg_new,h=12))
hwnabg_pred_new
plot(forecast(hw_nabg_new,h=12))
hwnabg_mape_new<-MAPE(hwnabg_pred_new$Point.Forecast,test)*100
hwnabg_mape_new # 6.96

#################### MAPE TABLE ##########################

df_mapes_new<-data.frame(c("sesa_mape","holtab_mape","hwabg_mape_new","sesna_mape","holtnab_mape","hwnabg_mape_new"),c(sesa_mape,holtab_mape,hwabg_mape_new,sesna_mape,holtnab_mape,hwnabg_mape_new))
colnames(df_mapes_new)<-c("Model","MAPE")
View(df_mapes_new)

####################################################

# Moving Avergae
ma_model1<-sma(train)
ma_pred<-data.frame(predict(ma_model1,h=12))
ma_pred
plot(forecast(ma_model1))
ma_mape<-MAPE(ma_pred$Point.Forecast,test)*100
ma_mape # 18.55

######################################################

# Based on the above MAPE TABLE NEW-> MAPE of 'hwnabg_mape_new' is minimum (6.96).
#Build new model

new_model_2<-hw(amts,alpha=NULL,beta=NULL,gamma = NULL,h=12)

plot(forecast(new_model_2,h=12))

# Forecasted values for the next 12 months
forecast_new_2 <- data.frame(predict(new_model_2,h=12))
forecast_new_2

############################# END ##############################