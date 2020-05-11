# Time series object in R
data("AirPassengers")
str(AirPassengers)

# from dataset to time series object
data=read.csv(file="http://eric.univ-lyon2.fr/~jjacques/Download/DataSet/varicelle.csv")
plot(data$x)

varicelle<-ts(data$x,start=c(1931,1),end=c(1972,6),freq=12)
plot(varicelle)

#plot using ggplot
# install.packages('forecast')
library(forecast) # from the book Forecasting: Principles and Practice, OTexts, 2013
library(ggplot2)
autoplot(varicelle) +
  ggtitle('Number of varicella per months')+
  xlab('year')+
  ylab('Number of varicella')

# seasonal plot
ggseasonplot(varicelle,year.labels= TRUE,year.labels.left=TRUE)
# polar seasonal plot
ggseasonplot(varicelle,polar=TRUE)

# descriptive stats for TS
# empirical mean and variance
mean(varicelle)
var(varicelle)

# Empirical auto-covariance
tmp=acf(varicelle,type="cov",plot = FALSE)
tmp$acf[1:3,1,1]
plot(tmp)

# Empirical auto-correlation (between -1 and 1) 
# plot called crelogram
# between blue lines -> not significantly different from 0 (+-2/sqrt(n))
tmp=acf(varicelle,type="cor",plot = FALSE)
tmp$acf[1:3,1,1]
plot(tmp)

# auto correlation link with linear trend pattern
serie=2*(1:1000)+4
par(mfrow=c(1,2))
plot(ts(serie))
acf(serie)

# auto correlation link with seasonal pattern 
serie=cos(2*pi/12*(1:100))
par(mfrow=c(1,2))
plot(ts(serie))
acf(serie)

# partial auto-correlation
tmp=pacf(varicelle,type="cor",plot = FALSE)
tmp$acf[1:3,1,1]
plot(tmp)
