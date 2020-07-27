library(readr)
install.packages("readxl")
library(readxl)

cocacola<-read_excel(file.choose()) # read the cocacola.xlsx data
View(cocacola) # Seasonality 12 months 
windows()
plot(cocacola$Sales,type="o") # Seems upward trend based on plot

# So creating 4 dummy variables for Q1,Q2,Q3,Q4
cocacola$Q1<-NA
cocacola$Q2<-NA
cocacola$Q3<-NA
cocacola$Q4<-NA
# Recognise the respective dummy variable
cocacola$Q1<-ifelse(grepl("Q1",cocacola$Quarter),'1','0')
cocacola$Q2<-ifelse(grepl("Q2",cocacola$Quarter),'1','0')
cocacola$Q3<-ifelse(grepl("Q3",cocacola$Quarter),'1','0')
cocacola$Q4<-ifelse(grepl("Q4",cocacola$Quarter),'1','0')
View(cocacola)

cocacola["t"]<- 1:42 # add new column of "t" by assigning timeindex to each row/record 
cocacola["t_sq"]<-cocacola$t*cocacola$t # new column for t sq
cocacola["log_Sales"]<-log(cocacola$Sales) # new column for log(sales)
View(cocacola)

# convert the dummy variables from Chr to Factor
cocacola$Q1<-as.numeric(cocacola$Q1)
cocacola$Q2<-as.numeric(cocacola$Q2)
cocacola$Q3<-as.numeric(cocacola$Q3)
cocacola$Q4<-as.numeric(cocacola$Q4)

str(cocacola)
attach(cocacola)

# Split the data into train & test dataset
train<-cocacola[1:36,] # 36 Qtrs
test<-cocacola[37:42,] # 6 Qtrs

########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)
summary(linear_model) # R^2: 0.79
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
RMSE_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
RMSE_linear # 667.42

######################### Exponential #################################

expo_model<-lm(log_Sales~t,data=train)
summary(expo_model) # R^2 : 0.80
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
RMSE_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
RMSE_expo # 526.76

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_sq,data=train)
summary(Quad_model) # R^2:0.86
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
RMSE_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
RMSE_Quad # 485.14

######################### Additive Seasonality #########################

add_sea_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(add_sea_model) #R^2:0.11
add_sea_pred<-data.frame(predict(add_sea_model,newdata=test,interval='predict'))
RMSE_Add_sea<-sqrt(mean((test$Sales-add_sea_pred$fit)^2,na.rm = T))
RMSE_Add_sea # 1895.55

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Linear_model) # R^2:0.89
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
RMSE_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
RMSE_Add_sea_Linear # 555.34

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_sq+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Quad_model) # R^2:0.96
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
RMSE_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
RMSE_Add_sea_Quad # 283.06

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Sales~Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_model) # R^2:0.13
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
RMSE_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
RMSE_multi_sea # 1980.53

######################## Multiplicative Seasonality Linear #######################

multi_sea_linear_model<-lm(log_Sales~t+Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_linear_model) # R^2:0.91
multi_sea_linear_pred<-data.frame(predict(multi_sea_linear_model,newdata=test,interval='predict'))
RMSE_multi_sea_Linear<-sqrt(mean((test$Sales-exp(multi_sea_linear_pred$fit))^2,na.rm = T))
RMSE_multi_sea_Linear # 323.21

######################## Multiplicative Seasonality Quad #######################

multi_sea_quad_model<-lm(log_Sales~t+t_sq+Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_quad_model) # R^2:0.94
multi_sea_quad_pred<-data.frame(predict(multi_sea_quad_model,newdata=test,interval='predict'))
RMSE_multi_sea_quad<-sqrt(mean((test$Sales-exp(multi_sea_quad_pred$fit))^2,na.rm = T))
RMSE_multi_sea_quad # 602.50


# Preparing table on above 9 model and it's RMSE values 

table_RMSE<-data.frame(c("RMSE_linear","RMSE_expo","RMSE_Quad","RMSE_Add_sea","RMSE_Add_sea_Linear","RMSE_Add_sea_Quad","RMSE_multi_sea","RMSE_multi_sea_Linear","RMSE_multi_sea_quad"),c(RMSE_linear,RMSE_expo,RMSE_Quad,RMSE_Add_sea,RMSE_Add_sea_Linear,RMSE_Add_sea_Quad,RMSE_multi_sea,RMSE_multi_sea_Linear,RMSE_multi_sea_quad))
colnames(table_RMSE)<-c("Model","RMSE")
View(table_RMSE)

# Additive seasonality with Quadratic Model has least RMSE value (283.06)
# Build new model on complete data (train+test)

new_model<-lm(Sales~t+t_sq+Q1+Q2+Q3+Q4,data=cocacola )
new_model$fitted.values
summary(new_model) # R^2: 0.97

RMSE_new_model<-sqrt(mean((cocacola$Sales-new_model$fitted.values)^2,na.rm = T))
RMSE_new_model # 159.055

####################### Predicting forecast #############################
library(readxl)
test_data<-read_excel(file.choose(),1) # reading new file prepared to forecast next 4 Qtr
View(test_data)
str(test_data)

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
acf(resid,lag.max = 4)
# Ifany one lag is showing significance (crossing +/-2SE line) then apply Autoregression
# By principal of parcimony we will consider lag - 1  as we have so 
# many significant lags 
# Building Autoregressive model on residuals consider lag-1 

k <- arima(resid, order=c(2,0,0))
View(data.frame(res=resid,newresid=k$residuals))
plot(k$residuals,resid)
windows()
acf(k$residuals,lag.max = 4)

pred_res<-predict(k,n.ahead = 4)
str(pred_res)
pred_res$pred

test_data["forecasted errors"]<-pred_res$pred
test_data["final forecast"]<-test_data$`Forecasted Sales`+test_data$`forecasted errors`
View(test_data)

# ARIMA model takes p,d,q as input resp for AR,I,MA
# I value can be 0 or 1 for stationary model & integrated model resp
# Find q value for ARIMA (lag value before first insignificant lag) 
acf(cocacola$Sales,lag.max = 10) # 10 is the first insignificant means q=9
# find p value for arima (partial autocorelation function) 1st significant lag
pacf(cocacola$Sales,lag.max = 10) # p=1

arima_k<-arima(cocacola$Sales,order = c(1,1,9))
pred_arima4<-predict(arima_k,n.ahead=4)
pred_arima4$pred
test_data["forecast by ARIMA"]<-pred_arima4$pred

#without adjustment with autoregression of errors (order 0 or 1)

arima_k<-arima(cocacola$Sales,order = c(1,0,9))
pred_arima3<-predict(arima_k,n.ahead = 4)
pred_arima3$pred
test_data["forecast by ARIMA w/o I"]<-pred_arima3$pred

write.csv(test_data,file="Cocacola_Forecast_rev.csv",row.names = F)
getwd()

################################ END ##############################

