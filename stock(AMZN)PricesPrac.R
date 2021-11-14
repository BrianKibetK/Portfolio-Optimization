setwd("C:/Users/Brian Kibet/Documents/R")
getwd()
amazon<-read.csv("AMZN_Yahoo.csv",header=TRUE)
amazon
#viewing the last few data of amazon
head(amazon)
#viewing the first few data of amazon
tail(amazon)

#check the class of variable Date
class(amazon$Date)
#Convert it from factor to class date
DATE<-as.Date(amazon$Date,"%d/%m/%Y")
head(DATE)
#Combining DATE and amazon and replacing Date with DATE
amazon<-cbind(DATE,amazon[,-1])
head(amazon)
tail(amazon)

library(zoo)
library(xts)
install.package("xts")
install.packages("xts", repos="http://cloud.r-project.org")
install.packages("http://cran.r-project.org/src/contrib/zoo_1.8-0.tar.gz", type="source")
any(grepl("xts",installed.packages()))

#Converting amazon into xts object
amazon<-xts(amazon[,2:7],order.by = amazon[,1])
amazon

#the plot the data to ensure we have complete data
plot(amazon)
data.missing<-amazon[-700:-800,]
plot(data.missing)

#Converting daily prices into weekly and monthly prices
wk<-amazon
wk
data.weekly<-to.weekly(wk)
data.weekly
#Converting into monthly prices
mo<-amazon
data.monthly<-to.monthly(mo)
data.monthly

#calculating the price returns of amazon
#subset the data to include only the closing prices
amznCP<-amazon[,4]
amznCP
