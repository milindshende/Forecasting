library(forecast)
library(fpp)
library(smooth)
library(readxl)
library(tseries)

cocacola<-read_excel(file.choose()) # read the cocacola.xlsx data
View(cocacola)

# Converting data into time series object
amts<-ts(cocacola$Sales,frequency = 4,start=c(42))
View(amts)

# dividing entire data into training and testing data 
train<-amts[1:38] # 38 Qtr
test<-amts[39:42] # 4 Qtr

# converting time series object
train<-ts(train,frequency = 4)
test<-ts(test,frequency = 4)

# Plotting time series data
plot(amts) # Visualization shows that it has level, trend, seasonality

# Moving Average Model
ma_model<-Arima(train,order = c(0,0,4))
Forecasted<-forecast(ma_model,h=4)
MA<-accuracy(Forecasted,x=as.numeric(test))
MA # MAPE of Training 9.13 & Test 20.97
plot(Forecasted)

#### Models USING HoltWinters function ################
# Default value of alpha=0.2
hw_a<-HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
hw_a
hwa_pred<-data.frame(predict(hw_a,n.ahead=4))
plot(forecast(hw_a,h=4))
hwa_mape<-MAPE(hwa_pred$fit,test)*100
hwa_mape # 16.12

# with default values of alpha = 0.2, beta = 0.15
hw_ab<-HoltWinters(train,alpha = 0.2,beta = 0.15,gamma = F)
hw_ab
hwab_pred<-data.frame(predict(hw_ab,n.ahead = 4))
plot(forecast(hw_ab,h=4))
hwab_mape<-MAPE(hwab_pred$fit,test)*100
hwab_mape # 8.74

# with default values of alpha = 0.2, beta = 0.15, gamma = 0.05 
hw_abg<-HoltWinters(train,alpha = 0.2,beta = 0.15,gamma = 0.05)
hw_abg
hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 4))
plot(forecast(hw_abg,h=4))
hwabg_mape<-MAPE(hwabg_pred$fit,test)*100
hwabg_mape # 3.58

# With optimum values
hw_na<-HoltWinters(train,beta = F,gamma = F)
hw_na # alpha=0.50
hwna_pred<-data.frame(predict(hw_na,n.ahead = 4))
hwna_pred
plot(forecast(hw_na,h=4))
hwna_mape<-MAPE(hwna_pred$fit,test)*100
hwna_mape # 9.09

hw_nab<-HoltWinters(train,gamma=F)
hw_nab #alpha=0.57, beta=0.31
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=4))
hwnab_pred
plot(forecast(hw_nab,h=4))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100
hwnab_mape # 8.62

hw_nabg<-HoltWinters(train)
hw_nabg # alpha=0.37,beta=0.25,gamma=0.88
hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =4))
hwnabg_pred
plot(forecast(hw_nabg,h=4))
hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100
hwnabg_mape # 2.39

######################## MAPE TABLE ########################

df_mape<-data.frame(c("hwa_mape","hwab_mape","hwabg_mape","hwna_mape","hwnab_mape","hwnabg_mape"),c(hwa_mape,hwab_mape,hwabg_mape,hwna_mape,hwnab_mape,hwnabg_mape))

colnames(df_mape)<-c("Model","MAPE")
View(df_mape)

##############################################################
# Based on the MAPE TABLE-> MAPE of 'hwnabg' is minimum (2.39). 
# Build new model

new_model_1 <- HoltWinters(amts)
plot(forecast(new_model_1,n.ahead=4))

# Forecasted values for the next 12 months
forecast_new_1 <- data.frame(predict(new_model_1,n.ahead=4))
forecast_new_1

###############################################################

############## USING ses,holt,hw functions ####################

# Simple Exponential smoothing with default alpha value
ses_a<-ses(train,alpha = 0.2,h=4) 
ses_a
sesa_pred<-data.frame(predict(ses_a,n.ahead=4))
plot(forecast(ses_a,h=4))
sesa_mape<-MAPE(sesa_pred$Point.Forecast,test)*100
sesa_mape # 16.12

# holt with alpha = 0.2, beta = 0.15
holt_ab<-holt(train,alpha = 0.2,beta = 0.15,h=4)
holt_ab
holtab_pred<-data.frame(predict(holt_ab,n.ahead=4))
plot(forecast(holt_ab,h=4))
holtab_mape<-MAPE(holtab_pred$Point.Forecast,test)*100
holtab_mape # 8.26

# hw with alpha = 0.2, beta = 0.15, gamma = 0.05
hw_abg_new<-hw(train,alpha = 0.2,beta = 0.15,gamma = 0.05,h=4)
hw_abg_new
hwabg_pred_new<-data.frame(predict(hw_abg_new,n.ahead=4))
plot(forecast(hw_abg_new,h=4))
hwabg_mape_new<-MAPE(hwabg_pred_new$Point.Forecast,test)*100
hwabg_mape_new # 6.14

# simple exponential method with optimum values
ses_na<-ses(train,alpha=NULL,h=4)
ses_na
sesna_pred<-data.frame(predict(ses_na,h = 4))
sesna_pred
plot(forecast(ses_na,h=4))
sesna_mape<-MAPE(sesna_pred$Point.Forecast,test)*100
sesna_mape # 9.13

# holt method with optimum values
holt_nab<-holt(train,alpha = NULL,beta = NULL,h=4)
holt_nab
holtnab_pred<-data.frame(predict(holt_nab,h=4))
holtnab_pred
plot(forecast(holt_nab,h=4))
holtnab_mape<-MAPE(holtnab_pred$Point.Forecast,test)*100
holtnab_mape # 8.92

# hw method with optimum values
hw_nabg_new<-hw(train,alpha=NULL,beta=NULL,gamma = NULL,h=4)
hw_nabg_new
hwnabg_pred_new<-data.frame(predict(hw_nabg_new,h=4))
hwnabg_pred_new
plot(forecast(hw_nabg_new,h=4))
hwnabg_mape_new<-MAPE(hwnabg_pred_new$Point.Forecast,test)*100
hwnabg_mape_new # 3.82

#################### MAPE TABLE ##########################

df_mapes_new<-data.frame(c("sesa_mape","holtab_mape","hwabg_mape_new","sesna_mape","holtnab_mape","hwnabg_mape_new"),c(sesa_mape,holtab_mape,hwabg_mape_new,sesna_mape,holtnab_mape,hwnabg_mape_new))
colnames(df_mapes_new)<-c("Model","MAPE")
View(df_mapes_new)

####################################################

# Moving Avergae
ma_model1<-sma(train)
ma_pred<-data.frame(predict(ma_model1,h=4))
ma_pred
plot(forecast(ma_model1))
ma_mape<-MAPE(ma_pred$Point.Forecast,test)*100
ma_mape # 8.90

######################################################

# Based on the above MAPE TABLE NEW-> MAPE of 'hwnabg_mape_new' is minimum (3.82).
#Build new model

new_model_2<-hw(amts,alpha=NULL,beta=NULL,gamma = NULL,h=4)

plot(forecast(new_model_2,h=4))

# Forecasted values for the next 12 months
forecast_new_2 <- data.frame(predict(new_model_2,h=4))
forecast_new_2

############################# END ##############################