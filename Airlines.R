library(readr)
install.packages("readxl")
library(readxl)

airlines<-read_excel(file.choose()) # read the airlines.xls data
View(airlines) # Seasonality 12 months 
windows()
plot(airlines$Passengers,type="o") # Seems upward trend based on plot

# So creating 12 dummy variables 
X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
View(X)
colnames(X)<-month.abb # Assigning month names to dummy
View(X)
trakdata<-cbind(airlines,X)
View(trakdata)
trakdata["t"]<- 1:96 # add new column of "t" by assigning timeindex to each row/record 
View(trakdata)
trakdata["log_Passenger"]<-log(trakdata["Passengers"])
trakdata["t_sq"]<-trakdata["t"]*trakdata["t"]
attach(trakdata)

# Split the data into train & test dataset
train<-trakdata[1:84,] # 84 months
test<-trakdata[85:96,] # 12 months

########################### LINEAR MODEL #############################

linear_model<-lm(Passengers~t,data=train)
summary(linear_model) # R^2: 0.79
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear # 53.199

######################### Exponential #################################

expo_model<-lm(log_Passenger~t,data=train)
summary(expo_model) # R^2 : 0.82
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 46.057

######################### Quadratic ####################################

Quad_model<-lm(Passengers~t+t_sq,data=train)
summary(Quad_model) # R^2:0.796
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 48.051

######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model) #R^2:0.16
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_Add_sea<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_Add_sea # 132.819

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model) # R^2:0.955
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 35.348

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+t_sq+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model) # R^2:0.959
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 26.360

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Passenger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model) # R^2:0.15
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 140.06

######################## Multiplicative Seasonality Linear #######################

multi_add_sea_model<-lm(log_Passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) # R^2:0.976
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_sea_Linear<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea_Linear # 10.519

######################## Multiplicative Seasonality Quad #######################

multi_sea_quad_model<-lm(log_Passenger~t+t_sq+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_quad_model) # R^2:0.977
multi_sea_quad_pred<-data.frame(predict(multi_sea_quad_model,newdata=test,interval='predict'))
rmse_multi_sea_quad<-sqrt(mean((test$Passengers-exp(multi_sea_quad_pred$fit))^2,na.rm = T))
rmse_multi_sea_quad # 18.372


# Preparing table on above 9 model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_Add_sea","rmse_Add_sea_Linear","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_sea_Linear","rmse_multi_sea_quad"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_Add_sea,rmse_Add_sea_Linear,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_sea_Linear,rmse_multi_sea_quad))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multi seasonality with Linear has least RMSE value (10.51)
# Build new model on complete data (train+test)

new_model<-lm(log_Passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=trakdata )
new_model$fitted.values
summary(new_model) # R^2: 0.982

####################### Predicting forecast #############################
library(readxl)
test_data<-read_excel(file.choose(),1) # reading new file prepared to forecast next 12 months
View(test_data)
pred_new<-exp(predict(new_model,newdata = test_data))
pred_new

test_data["Forecasted Passengers"]<-pred_new
View(test_data)

# But we need to check wheather errors are having information or not
# If errors are not having any information then above is our final forecast
# Else we have to use Autoregression

# New Model(from above) Residuals :
# new_model$residuals
resid <- residuals(new_model)
resid[1:10]
windows()
acf(resid,lag.max = 12)
# Ifany one lag is showing significance (crossing +/-2SE line) then apply Autoregression
# By principal of parcimony we will consider lag - 1  as we have so 
# many significant lags 
# Building Autoregressive model on residuals consider lag-1 

k <- arima(resid, order=c(1,0,0))
str(k)
summary(k)
View(data.frame(res=resid,newresid=k$residuals))
plot(k$residuals,resid)
windows()
acf(k$residuals,lag.max = 12)

pred_res<-predict(k,n.ahead = 12)
#pred_res<- predict(arima(k$residuals,order=c(1,0,0)),n.ahead = 12)
str(pred_res)
pred_res$pred

test_data["forecasted errors"]<-pred_res$pred
test_data["final forecast"]<-round(test_data$`Forecasted Passengers`+test_data$`forecasted errors`)
View(test_data)

# ARIMA model takes p,d,q as input resp for AR,I,MA
# I value can be 0 or 1 for stationary model & integrated model resp
# Find q value for ARIMA (lag value before first insignificant lag) 
acf(airlines$Passengers,lag.max = 35) # 26 is the first insignificant means q=25
# find p value for arima (partial autocorelation function) 1st significant lag
pacf(airlines$Passengers,lag.max = 12) # p=1

arima_k<-arima(airlines$Passengers,order = c(1,1,25))
pred_arima4<-predict(arima_k,n.ahead=12)
pred_arima4$pred
test_data["forecast by ARIMA"]<-round(pred_arima4$pred)

#without adjustment with autoregression of errors (order 0 or 1)

arima_k<-arima(airlines$Passengers,order = c(1,0,25))
pred_arima3<-predict(arima_k,n.ahead = 12)
pred_arima3$pred
test_data["forecast by ARIMA w/o I"]<-round(pred_arima3$pred)

write.csv(test_data,file="Airlines_Forecast.csv",row.names = F)
getwd()

################################ END ##############################

