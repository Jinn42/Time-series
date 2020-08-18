####using outdoor_temp
#download the data

library(readxl)
raw_data<-read_excel("/Users/jinli/Desktop/elec_data.xlsx",sheet=1,na="NA")
raw_data
whole_data <- ts(raw_data,start=c(0,0))
whole_data
plot(whole_data)

#withdraw the training data
library(forecast)
data_train=window(whole_data,start=0, end=4505)
data_train
data_test=window(whole_data,start=4506, end=4601)
data_test

####1.forecast of power with temparature---VARS
#forecast with vars
library(vars)
VARselect(data_train, lag.max=96, type="const")

var <- VAR(data_train, p=96,type = "const") 
prev=forecast(var,h=96,PI=FALSE) 
plot(prev)
setwd("/Users/jinli/Desktop/")
write.table(prev,"prev_vars.csv",sep=",")

#Forecasting efficiency(tested with Temp)
print(sqrt(mean((prev$forecast$Temp$mean-data_test[,"Temp"])^2)))

###2.forecast of power without temparature----ARIMA

mod=auto.arima(data_train[,1]) 
pred=forecast(mod,h =96)
plot(pred)

setwd("/Users/jinli/Desktop/")
write.table(pred,"pred.csv",sep=",")

data_train[,1] %>% diff(lag=96) %>% ggtsdisplay()


