#importing Data
library(readxl)
AirQualityUCI <- read_excel("E:/VISHAL YADAV/Msc sksc/SEMESTER 3/5 Time Series Analysis/Project/AirQualityUCI.xlsx")
View(AirQualityUCI)

#Start and End of Data
head(AirQualityUCI)
tail(AirQualityUCI)

#Ploting of all variable
ts.plot(AirQualityUCI$CO.R)
ts.plot(AirQualityUCI$CO.T)
ts.plot(AirQualityUCI$NMHC.R)
ts.plot(AirQualityUCI$C6H6.R)
ts.plot(AirQualityUCI$NMHC.T)
ts.plot(AirQualityUCI$NOx.R)
ts.plot(AirQualityUCI$NOx.T)
ts.plot(AirQualityUCI$NO2.R)
ts.plot(AirQualityUCI$NO2.T)
ts.plot(AirQualityUCI$O3.T)
ts.plot(AirQualityUCI$T)
ts.plot(AirQualityUCI$RH)
ts.plot(AirQualityUCI$AH)

# To view class of variable
summary(AirQualityUCI)


#Creating Variable from AirQualityUCI for Using imputets function
CO.R=as.numeric(AirQualityUCI$CO.R)
CO.T=as.numeric(AirQualityUCI$CO.T)
NMHC.R=as.numeric(AirQualityUCI$NMHC.R)
NMHC.T=as.numeric(AirQualityUCI$NMHC.T)
NOx.R=as.numeric(AirQualityUCI$NOx.R)
NOx.T=as.numeric(AirQualityUCI$NOx.T)
NO2.R=as.numeric(AirQualityUCI$NO2.R)
NO2.T=as.numeric(AirQualityUCI$NO2.T)
C6H6.R=as.numeric(AirQualityUCI$C6H6.R)
O3.T=as.numeric(AirQualityUCI$O3.T)
T=as.numeric(AirQualityUCI$T)
RH=as.numeric(AirQualityUCI$RH)
AH=as.numeric(AirQualityUCI$AH)

#Creating Data frame to check classes of variable 
pollution=data.frame(CO.R,CO.T,NMHC.R,NMHC.T,NOx.R,NOx.T,NO2.R,NO2.T,C6H6.R,O3.T,T,RH,AH)

#checking classes of variable 
summary(pollution)


#library for handling missing Value 
library(imputeTS)

#replaceing missing value by moving average
pollution$CO.R=na_ma(pollution$CO.R)
pollution$CO.T=na_ma(pollution$CO.T)
pollution$NMHC.R=na_ma(pollution$NMHC.R)
pollution$NMHC.T=na_ma(pollution$NMHC.T)
pollution$NOx.R=na_ma(pollution$NOx.R)
pollution$NOx.T=na_ma(pollution$NOx.T)
pollution$NO2.R=na_ma(pollution$NO2.R)
pollution$NO2.T=na_ma(pollution$NO2.T)
pollution$C6H6.R=na_ma(pollution$C6H6.R)
pollution$O3.T=na_ma(pollution$O3.T)
pollution$T=na_ma(pollution$T)
pollution$RH=na_ma(pollution$RH)
pollution$AH=na_ma(pollution$AH)

#plotting variable after replacement by moving average
ggplot_na_distribution(pollution$CO.R)
ggplot_na_distribution(pollution$CO.T)
ggplot_na_distribution(pollution$NMHC.R)
ggplot_na_distribution(pollution$NMHC.T)
ggplot_na_distribution(pollution$NOx.R)
ggplot_na_distribution(pollution$NOx.T)
ggplot_na_distribution(pollution$NO2.R)
ggplot_na_distribution(pollution$NO2.T)
ggplot_na_distribution(pollution$C6H6.R)
ggplot_na_distribution(pollution$O3.T)
ggplot_na_distribution(pollution$T)
ggplot_na_distribution(pollution$RH)
ggplot_na_distribution(pollution$AH)


#getting data into excel 
library(writexl)
write_xlsx(pollution, path = "AirQualityUCI_with_replacement.xlsx")

#importing Data from excel 
library(readxl)
AQ<- read_excel("E:/VISHAL YADAV/Msc sksc/SEMESTER 3/5 Time Series Analysis/Project/AirQualityUCI_average_of_the_day.xlsx")
View(AQ)

#Start and End of Data
head(AQ)
tail(AQ)

#Ploting of all variable
ts.plot(AQ$CO.R)
ts.plot(AQ$CO.T)
ts.plot(AQ$NMHC.R)
ts.plot(AQ$C6H6.R)
ts.plot(AQ$NMHC.T)
ts.plot(AQ$NOx.R)
ts.plot(AQ$NOx.T)
ts.plot(AQ$NO2.R)
ts.plot(AQ$NO2.T)
ts.plot(AQ$O3.T)
ts.plot(AQ$T)
ts.plot(AQ$RH)
ts.plot(AQ$AH)

# To view class of variable
summary(AQ)

library(imputeTS)

#plotting variable by ggplot_na_distribution function as its graphic is good 
ggplot_na_distribution(AQ$CO.R)
ggplot_na_distribution(AQ$CO.T)
ggplot_na_distribution(AQ$NMHC.R)
ggplot_na_distribution(AQ$NMHC.T)
ggplot_na_distribution(AQ$NOx.R)
ggplot_na_distribution(AQ$NOx.T)
ggplot_na_distribution(AQ$NO2.R)
ggplot_na_distribution(AQ$NO2.T)
ggplot_na_distribution(AQ$C6H6.R)
ggplot_na_distribution(AQ$O3.T)
ggplot_na_distribution(AQ$T)
ggplot_na_distribution(AQ$RH)
ggplot_na_distribution(AQ$AH)

#simple Moving Aevrage 
library(TTR)
AQ1=SMA(AQ$CO.R,n=39)
AQ2=SMA(AQ$CO.T,n=39)
AQ3=SMA(AQ$NMHC.R,n=39)
AQ4=SMA(AQ$NMHC.T,n=39)
AQ5=SMA(AQ$NOx.R,n=39)
AQ6=SMA(AQ$NOx.T,n=39)
AQ7=SMA(AQ$NO2.R,n=39)
AQ8=SMA(AQ$NO2.T,n=39)
AQ9=SMA(AQ$C6H6.R,n=39)
AQ10=SMA(AQ$O3.T,n=39)
AQ11=SMA(AQ$T,n=39)
AQ12=SMA(AQ$RH,n=39)
AQ13=SMA(AQ$AH,n=39)

#ploting Simple Moving Average on observed data 

plot.ts(AQ$CO.R)
lines(AQ1,col='green')

plot.ts(AQ$CO.T)
lines(AQ2,col='green')

plot.ts(AQ$NMHC.R)
lines(AQ3,col='green')

plot.ts(AQ$NMHC.T)
lines(AQ4,col='green')

plot.ts(AQ$NOx.R)
lines(AQ5,col='green')

plot.ts(AQ$NOx.T)
lines(AQ6,col='green')

plot.ts(AQ$NO2.R)
lines(AQ7,col='green')

plot.ts(AQ$NO2.T)
lines(AQ8,col='green')

plot.ts(AQ$C6H6.R)
lines(AQ9,col='green')

plot.ts(AQ$O3.T)
lines(AQ10,col='green')

plot.ts(AQ$T)
lines(AQ11,col='green')

plot.ts(AQ$RH)
lines(AQ12,col='green')

plot.ts(AQ$AH)
lines(AQ13,col='green')


#simple exponential Smoothing
ses_AQ1=HoltWinters(AQ$CO.R,beta = FALSE,gamma = FALSE)
ses_AQ2=HoltWinters(AQ$CO.T,beta = FALSE,gamma = FALSE)
ses_AQ3=HoltWinters(AQ$NMHC.R,beta = FALSE,gamma = FALSE)
ses_AQ4=HoltWinters(AQ$NMHC.T,beta = FALSE,gamma = FALSE)
ses_AQ5=HoltWinters(AQ$NOx.R,beta = FALSE,gamma = FALSE)
ses_AQ6=HoltWinters(AQ$NOx.T,beta = FALSE,gamma = FALSE)
ses_AQ7=HoltWinters(AQ$NO2.R,beta = FALSE,gamma = FALSE)
ses_AQ8=HoltWinters(AQ$NO2.T,beta = FALSE,gamma = FALSE)
ses_AQ9=HoltWinters(AQ$C6H6.R,beta = FALSE,gamma = FALSE)
ses_AQ10=HoltWinters(AQ$O3.T,beta = FALSE,gamma = FALSE)
ses_AQ11=HoltWinters(AQ$T,beta = FALSE,gamma = FALSE)
ses_AQ12=HoltWinters(AQ$RH,beta = FALSE,gamma = FALSE)
ses_AQ13=HoltWinters(AQ$AH,beta = FALSE,gamma = FALSE)

#Obtaining alpha value of simple exponential smoothing 
ses_AQ1$alpha
ses_AQ2$alpha
ses_AQ3$alpha
ses_AQ4$alpha
ses_AQ5$alpha
ses_AQ6$alpha
ses_AQ7$alpha
ses_AQ8$alpha
ses_AQ9$alpha
ses_AQ10$alpha
ses_AQ11$alpha
ses_AQ12$alpha
ses_AQ13$alpha

#plotting observed value and Simple exponential smoothing value 

plot(ses_AQ1)
plot(ses_AQ2)
plot(ses_AQ3)
plot(ses_AQ4)
plot(ses_AQ5)
plot(ses_AQ6)
plot(ses_AQ7)
plot(ses_AQ8)
plot(ses_AQ9)
plot(ses_AQ10)
plot(ses_AQ11)
plot(ses_AQ12)
plot(ses_AQ13)

#double exponential smoothing 

des_AQ1=HoltWinters(AQ$CO.R,gamma=FALSE)
des_AQ2=HoltWinters(AQ$CO.T,gamma=FALSE)
des_AQ3=HoltWinters(AQ$NMHC.R,gamma=FALSE)
des_AQ4=HoltWinters(AQ$NMHC.T,gamma=FALSE)
des_AQ5=HoltWinters(AQ$NOx.R,gamma=FALSE)
des_AQ6=HoltWinters(AQ$NOx.T,gamma=FALSE)
des_AQ7=HoltWinters(AQ$NO2.R,gamma=FALSE)
des_AQ8=HoltWinters(AQ$NO2.T,gamma=FALSE)
des_AQ9=HoltWinters(AQ$C6H6.R,gamma=FALSE)
des_AQ10=HoltWinters(AQ$O3.T,gamma=FALSE)
des_AQ11=HoltWinters(AQ$T,gamma=FALSE)
des_AQ12=HoltWinters(AQ$RH,gamma=FALSE)
des_AQ13=HoltWinters(AQ$AH,gamma=FALSE)

#obtaining alpha value of double exponential smoothing 
des_AQ1$alpha
des_AQ2$alpha
des_AQ3$alpha
des_AQ4$alpha
des_AQ5$alpha
des_AQ6$alpha
des_AQ7$alpha
des_AQ8$alpha
des_AQ9$alpha
des_AQ10$alpha
des_AQ11$alpha
des_AQ12$alpha
des_AQ13$alpha

#Obtaining beta value of double exponential smoothing
des_AQ1$beta
des_AQ2$beta
des_AQ3$beta
des_AQ4$beta
des_AQ5$beta
des_AQ6$beta
des_AQ7$beta
des_AQ8$beta
des_AQ9$beta
des_AQ10$beta
des_AQ11$beta
des_AQ12$beta
des_AQ13$beta

#plotting observed and double exponential smoothing 
plot(des_AQ1)
plot(des_AQ2)
plot(des_AQ3)
plot(des_AQ4)
plot(des_AQ5)
plot(des_AQ6)
plot(des_AQ7)
plot(des_AQ8)
plot(des_AQ9)
plot(des_AQ10)
plot(des_AQ11)
plot(des_AQ12)
plot(des_AQ13)


#stationary checking by ad.test function (alternative is sationary)
library(aTSA)
adf.test(AQ$CO.R)

#stationary checking by Kpss.test function 

kpss.test(AQ$CO.R)

#stationary checking by pp.test function 

pp.test(AQ$CO.R)

#applying TS function
AQ1=ts(AQ1,start=2004,frequency=365)
AQ1
plot(AQ1)
library(ggplot2)
autoplot(AQ1)

decompose(AQ1)







