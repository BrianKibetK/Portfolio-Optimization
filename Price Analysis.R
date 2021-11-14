#Importing data into R
Data.Nse<- read.csv("Price Analysis.csv")
print(Data.Nse)
print(head(Data.Nse))
print(tail(Data.Nse))

#Converting Variable DATE from a Factor to a Date
class(Data.Nse$DATE)
date<-as.Date(Data.Nse$DATE)
head(date)
tail(date)
class(date)

#Combine date and Data.NSE
Data.Nse<-cbind(date,Data.Nse[,-1])
head(Data.Nse)
tail(Data.Nse)

class(Data.Nse)
#Convert from dataframe object to xts object 
install.packages("xts")
library(xts)
data.NSE<-xts(Data.Nse[,2:64],order.by = Data.Nse[,1])
head(data.NSE)
tail(data.NSE)
class(data.NSE)

#Plotting the data 
plot(data.NSE$EABL)
plot(data.NSE$COOP)
summary(data.NSE)

#Manipulating the data 
data.scom<-data.NSE[,52]
print(head(data.scom))
print(tail(data.scom))
plot(data.scom)

#Subsetting data using dates
wk<-data.NSE
data.wkly<-to.weekly(wk, OHLC=FALSE)
print(tail(data.wkly))

nse.mo<-data.NSE
nse.monthly<-to.monthly(nse.mo,OHLC=FALSE)
print(head(nse.monthly))
print(tail(scom.monthly))

scom.Mo<-data.scom
scom.monthly<-to.monthly(scom.Mo, OHLC=FALSE)
print(head(scom.monthly))
print(tail(scom.monthly))

#Plotting candlesticks chart(Data should be in the form of OHLC)
install.packages('quantmod')
library(quantmod)
