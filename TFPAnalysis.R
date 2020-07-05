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
library(tidyverse)
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
