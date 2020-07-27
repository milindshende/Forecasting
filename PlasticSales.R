library(readr)
install.packages("readxl")
library(readxl)

plastic<-read.csv(file.choose()) # read the plastic sales data
View(plastic) # Seasonality 12 months 
windows()
plot(plastic$Sales,type="o") # Seems upward trend based on plot

# So creating 12 dummy variables for each month
X<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )# Creating dummies for 12 months
View(X)
colnames(X)<-month.abb # Assigning month names to dummy
View(X)
trakdata<-cbind(plastic,X)
View(trakdata)
trakdata["t"]<- 1:60 # add new column of "t" by assigning timeindex to each row/record 
View(trakdata)
trakdata["t_sq"]<-trakdata["t"]*trakdata["t"]
trakdata["log_Sales"]<-log(trakdata["Sales"])
attach(trakdata)

# Split the data into train & test dataset
train<-trakdata[1:48,] # 48 months
test<-trakdata[49:60,] # 12 months

########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)
summary(linear_model) # R^2: 0.33
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear # 260.93

######################### Exponential #################################

expo_model<-lm(log_Sales~t,data=train)
summary(expo_model) # R^2 : 0.31
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 268.69

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_sq,data=train)
summary(Quad_model) # R^2:0.33
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 297.40

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model) #R^2:0.76
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_Add_sea<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_Add_sea # 235.60

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model) # R^2:0.97
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 135.55

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_sq+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model) # R^2:0.98
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 218.19

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model) # R^2:0.79
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 239.65

######################## Multiplicative Seasonality Linear #######################

multi_add_sea_model<-lm(log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) # R^2:0.98
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_sea_Linear<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea_Linear # 160.68

######################## Multiplicative Seasonality Quad #######################

multi_sea_quad_model<-lm(log_Sales~t+t_sq+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_quad_model) # R^2:0.98
multi_sea_quad_pred<-data.frame(predict(multi_sea_quad_model,newdata=test,interval='predict'))
rmse_multi_sea_quad<-sqrt(mean((test$Sales-exp(multi_sea_quad_pred$fit))^2,na.rm = T))
rmse_multi_sea_quad # 239.60


# Preparing table on above 9 model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_Add_sea","rmse_Add_sea_Linear","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_sea_Linear","rmse_multi_sea_quad"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_Add_sea,rmse_Add_sea_Linear,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_sea_Linear,rmse_multi_sea_quad))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Additive seasonality with Linear has least RMSE value (135.53)
# Build new model on complete data (train+test)
new_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=trakdata)
new_model$fitted.values
summary(new_model) # R^2: 0.94

####################### Predicting forecast #############################
library(readxl)
test_data<-read_excel(file.choose(),1) # reading new file prepared to forecast next 12 months
View(test_data)
pred_new<-predict(new_model,newdata = test_data)
pred_new

test_data["Forecasted Sales"]<-pred_new
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
test_data["final forecast"]<-(test_data$`Forecasted Sales`+test_data$`forecasted errors`)
View(test_data)

# ARIMA model takes p,d,q as input resp for AR,I,MA
# I value can be 0 or 1 for stationary model & integrated model resp
# Find q value for ARIMA (lag value before first insignificant lag) 
acf(plastic$Sales,lag.max = 12) # 3 is the first insignificant means q=2
# find p value for arima (partial autocorelation function) 1st significant lag
pacf(plastic$Sales,lag.max = 12) # p=1

arima_k<-arima(plastic$Sales,order = c(1,1,2))
pred_arima4<-predict(arima_k,n.ahead=12)
pred_arima4$pred
test_data["forecast by ARIMA"]<-(pred_arima4$pred)

#without adjustment with autoregression of errors (order 0 or 1)

arima_k<-arima(plastic$Sales,order = c(1,0,2))
pred_arima3<-predict(arima_k,n.ahead = 12)
pred_arima3$pred
test_data["forecast by ARIMA w/o I"]<-(pred_arima3$pred)

write.csv(test_data,file="PlasticSales_Forecast.csv",row.names = F)
getwd()

################################ END ##############################

