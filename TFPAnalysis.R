setwd('/Users/tmgoncalves/Documents/')
getwd()

#Open the TFP.csv file attached.
#The series is composed by TFP (rtfpna variable) at constant national prices (2005 = 1) 
#for three countries: United States (USA), Canada (CAN) and Mexico (MEX).

#Make an exploratory data analysis;

#Forecast 10 years of the series (if you are performing the exercise in R,
#use package “forecast”);

#Check in the following link pages 2 and 3: 
#https://cran.r-project.org/web/packages/pwt8/pwt8.pdf 
#to see a list of all variables in the original dataset. 
#Can you think about another feature that could be helpful in explaining TFP series?

library(forecast)
library(zoo)
library(ggplot2)

#load data
data = read.csv('/Users/tmgoncalves/Downloads/Challenge_4i/TFP.csv')
View(data)

#type of data
class(data)
str(data)

#transform year to Date
data$year <- as.Date(as.character(data$year), format = "%Y")

#time serie plot rtfpna
ggplot(data, aes(x = year, y = rtfpna)) + 
  geom_line(aes(color = isocode), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_x_date(date_labels = "%Y", date_breaks ="5 year")+
  theme_minimal()

#boxplot
ggplot(data, aes(x = isocode, y = rtfpna,
                 fill=as.factor(isocode)))+
  geom_boxplot(fill=c("#00AFBB", "#E7B800", "#FC4E07"))+
  theme_minimal()

#USA trend analysis
ggplot(subset(data, isocode =="USA"), aes(x = year, y = rtfpna))+
  geom_line(aes(color=isocode), size=1)+
  scale_color_manual(values = c("#FC4E07")) +
  scale_x_date(date_labels = "%Y", date_breaks ="5 year")+
  stat_smooth(color = "#00AFBB", fill = "#00AFBB", method = "loess")+
  theme_minimal()

#CAN trend analysis
ggplot(subset(data, isocode =="CAN"), aes(x = year, y = rtfpna))+
  geom_line(aes(color=isocode), size=1)+
  scale_color_manual(values = c("#FC4E07")) +
  scale_x_date(date_labels = "%Y", date_breaks ="5 year")+
  stat_smooth(color = "#00AFBB", fill = "#00AFBB", method = "loess")+
  theme_minimal()

#MEX trend analysis
ggplot(subset(data, isocode =="MEX"), aes(x = year, y = rtfpna))+
  geom_line(aes(color=isocode), size=1)+
  scale_color_manual(values = c("#FC4E07")) +
  scale_x_date(date_labels = "%Y", date_breaks ="5 year")+
  stat_smooth(color = "#00AFBB", fill = "#00AFBB", method = "loess")+
  theme_minimal()

#USA dataset
USA <- subset(data, isocode=='USA')
USA$isocode <- NULL
View(USA)

#set time serie data 
tsdata <- ts(USA$rtfpna, frequency = 1, start = (1950))

#plot time-serie
plot(tsdata, xlab='Years', ylab='USA',
     main='Time-Serie')

#get the optimal auto arima model
autoarima1 <- auto.arima(tsdata)
forecast1 <- forecast(autoarima1, h = 10)

#plot forecast
plot(forecast1)
plot(forecast1$residuals)

#plot the residuals
qqnorm(forecast1$residuals)

#acf
acf(forecast1$residuals)
pacf(forecast1$residuals)

#summary
summary(autoarima1)


#CAN dataset
CAN <- subset(data, isocode=='CAN')
CAN$isocode <- NULL
View(CAN)

#set time serie data 
tsdata2 <- ts(CAN$rtfpna, frequency = 1, start = (1950))

#plot time-serie
plot(tsdata2, xlab='Years', ylab='CAN',
     main='Time-Serie')

#get the optimal auto arima model
autoarima2 <- auto.arima(tsdata2)
forecast2 <- forecast(autoarima2, h = 10)

#plot forecast
plot(forecast2)
plot(forecast2$residuals)

#plot the residuals
qqnorm(forecast2$residuals)

#acf
acf(forecast2$residuals)
pacf(forecast2$residuals)

#summary
summary(autoarima2)

#MEX dataset
MEX <- subset(data, isocode=='MEX')
MEX$isocode <- NULL
View(MEX)

#set time serie data 
tsdata3 <- ts(MEX$rtfpna, frequency = 1, start = (1950))

#plot time-serie
plot(tsdata, xlab='Years', ylab='MEX',
     main='Time-Serie')

#get the optimal auto arima model
autoarima3 <- auto.arima(tsdata3)
forecast3 <- forecast(autoarima3, h = 10)

#plot forecast
plot(forecast3)
plot(forecast3$residuals)

#plot the residuals
qqnorm(forecast3$residuals)

#acf
acf(forecast3$residuals)
pacf(forecast3$residuals)

#summary
summary(autoarima3)







