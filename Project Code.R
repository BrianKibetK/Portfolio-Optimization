#importing Barclays data
setwd("C:/Users/Brian Kibet/Documents/data new")
getwd()
Barclays<-read.csv("barc.csv")
head(Barclays)
tail(Barclays)
date<-as.Date(Barclays$Date,"%d/%m/%Y")
date
bk<-cbind(date,Barclays[,-1])
bbk<-bk[,-(3:7)]
head(bbk)
tail(bbk)
#plot(bbk,col="black",main="Barclays Bank of Kenya Plot of Prices",lty=1,type="l")


#importing EABL data0
setwd("C:/Users/Brian Kibet/Documents/data new")
getwd()
EABL<-read.csv("eablf.csv")
head(EABL)
tail(EABL)
date<-as.Date(EABL$Date,"%d/%m/%Y")
date
E<-cbind(date,EABL[,-1])
eabl<-E[,-(3:7)]
head(eabl)
tail(eabl)
plot(eabl,col="brown",main="EABL plot of Prices",lty=1,type="l")


#importing Britam data
setwd("C:/Users/Brian Kibet/Documents/data new")
getwd()
Britam<-read.csv("britamF.csv")
head(Britam)
tail(Britam)
date<-as.Date(Britam$Date,"%d/%m/%Y")
date
Br<-cbind(date,Britam[,-1])
britam<-Br[,-(3:7)]
head(britam)
tail(britam)
plot(britam,col="green",main="Britam Plot of Prices",lty=1,type="l")


#importing Centum data
setwd("C:/Users/Brian Kibet/Documents/data new")
getwd()
Centum<-read.csv("centumF.csv")
head(Centum)
tail(Centum)
date<-as.Date(Centum$Date,"%d/%m/%Y")
date
C<-cbind(date,Centum[,-1])
centum<-C[,-(3:7)]
head(centum)
tail(centum)
plot(centum,col="grey",main="Centum plot of prices",lty=1,type="l")

#importing KPLC data
setwd("C:/Users/Brian Kibet/Documents/data new")
getwd()
KPLC<-read.csv("kplcf.csv")
head(KPLC)
tail(KPLC)
date<-as.Date(KPLC$Date,"%d/%m/%Y")
date
K<-cbind(date,KPLC[,-1])
kplc<-K[,-(3:7)]
head(kplc)
tail(kplc)
plot(kplc,col="blue",main="KPLC plot of prices",lty=1,type="l")

portfolio<-cbind(bbk,eabl[,2],britam[,2],centum[,2],kplc[,2])
names(portfolio)<-paste(c("Date","BBK.P","EABL.P","Britam.P","Centum.P","KPLC.P"))
head(portfolio)
tail(portfolio)
portf<-portfolio[order(portfolio$Date),]
head(portf)
tail(portf)

plot(portf[,6],col="blue",main="KPLC plot of prices",lty=1,type="l")
plot(portf[,5],col="yellow",main="Centum plot of prices")
plot(portf[,4],col="green",main="Britam Plot of Prices")
plot(portf$EABL.P,col="brown",main="EABL plot of Prices")
plot(portf$BBK.P,col="black",main="Barclays Bank of Kenya Plot of Prices")

#Converting to an xts 
library(xts)
portf<-xts(portf[,2:6],order.by=portf[,1])
head(portf)
tail(portf)
#calculating log returns
logRet<-diff(log(portf))
names(logRet)<-paste(c("BBK.Ret","EABL.Ret","Britam.Ret","Centum.Ret","KPLC.Ret"))
logRet$BBK.Ret[1]<-0
logRet$EABL.Ret[1]<-0
logRet$Britam.Ret[1]<-0
logRet$Centum.Ret[1]<-0
logRet$KPLC.Ret[1]<-0
head(logRet)
tail(logRet)
library(ggplot2)

plot(date,logRet$BBK.ret,type="n
library(zoo)
library(curl)
library(TTR)
library(quantmod)
library(quadprog)
library(tseries)

#Testing for stationarity
acf(logRet)
pacf(logRet[,1])
acf(logRet[,2])
pacf(logRet[,2])
acf(logRet[,3])
pacf(logRet[,3])
acf(logRet[,4])
pacf(logRet[,4])
acf(logRet[,5])
pacf(logRet[,5])

#Augmented dickey fuller test for stationarity
adf.test(logRet[,1])
adf.test(logRet[,2])
adf.test(logRet[,3])
adf.test(logRet[,4])
adf.test(logRet[,5])

library(vars)
library(strucchange)
VARselect(logRet[,1])
VARselect(logRet[,2])
VARselect(logRet[,3])
VARselect(logRet[,4])
VARselect(logRet[,5])

#Ljung-Box Test for Autocorrelation(HO:No autocorrelation vs H1: Autocorrelation present)
Box.test(logRet[,1],lag=2,type="Ljung")
Box.test(logRet[,2],lag=2,type="Ljung")
acf(logRet[,2])
pacf(logRet[,2])
Box.test(logRet[,3],lag=5,type="Ljung")
Box.test(logRet[,4],lag=1,type="Ljung")
Box.test(logRet[,5],lag=2,type="Ljung")


#Jarque Bera Test for normality
jarque.bera.test(logRet[,1])
jarque.bera.test(logRet[,2])
jarque.bera.test(logRet[,3])
jarque.bera.test(logRet[,4])
jarque.bera.test(logRet[,5])

#plot of log returns
plot(logRet[,1],col="black",main="Barclays plot of log returns",grid.axis=TRUE,yaxis.right=FALSE,type=)
plot(logRet[,2],col="brown",main="EABL plot of log returns",grid.axis=TRUE,yaxis.right=FALSE)
plot(logRet[,3],col="green",main="Britam plot of log returns",grid.axis=TRUE,yaxis.right=FALSE)
plot(logRet[,3],col="grey",main="Centum plot of log returns",grid.axis=TRUE,yaxis.right=FALSE)
plot(logRet[,3],col="blue",main="KPLC plot of log returns",grid.axis=TRUE,yaxis.right=FALSE)

#dataframe of log returns
returnsFrame<-data.frame(logRet$BBK.Ret,logRet$EABL.Ret,logRet$Britam.Ret ,logRet$Centum.Ret ,logRet$KPLC.Ret)
head(returnsFrame)
tail(returnsFrame)
meplot(logRet[,1])
meplot(logRet[,2])
meplot(logRet[,3])
meplot(logRet[,4])
meplot(logRet[,5])

gpdfit<-gpd(logRet[,1],threshold=0.01)
gpdfit
plot(gpdfit)

library(timeSeries)
library(fBasics)

basicStats(logRet[,1])
basicStats(logRet[,2])
basicStats(logRet[,3])
basicStats(logRet[,4])
basicStats(logRet[,5])

#QQ Plot of log returns
library(evir)
qplot(logRet[,1],col="black",main="Q-Q Plot for Barclays log returns")
qplot(logRet[,2],col="brown",main="Q-Q Plot for EABL log returns")
qplot(logRet[,3],col="green",main="Q-Q Plot for Britam log returns")
qplot(logRet[,4],col="grey",main="Q-Q Plot for Centum log returns")
qplot(logRet[,5],col="blue",main="Q-Q Plot for KPLC log returns")

#Back to the Log Returns(MVO)
head(logRet)
tail(logRet)
#Convert the combined data into a matrix
mat.ret<-matrix(logRet,nrow(logRet))
head(mat.ret)
tail(mat.ret)
colnames(mat.ret)<-c("Barclays","EABL","Britam","Centum","KPLC")
head(mat.ret)
tail(mat.ret)

#Calculate Variance Covariance matrix of log returns
varCov<-cov(mat.ret)
varCov
#correlation matrix
Corr<-cor(mat.ret)
Corr

#Constructing the target portfolio return vector(mean of securities)
head(mat.ret)
avgRet<-matrix(apply(mat.ret,2,mean))
avgRet
rownames(avgRet)<-c("Barclays","EABL","Britam","Centum","KPLC")
colnames(avgRet)<-c("Average Returns")
avgRet
min.ret<-(avgRet[2,])
min.ret
max.ret<-max(avgRet)
max.ret

#Using 100 increments to generate target returns btwn minimum and maximum
increments=100
tgt.ret<-seq(min.ret,max.ret,length=increments)
head(tgt.ret)
tail(tgt.ret)

#Construct dummy portfolio standard deviation and weights vector
tgt.sd<-rep(0,length=increments)
tgt.sd
wgt<-matrix(0,nrow=increments,ncol=length(avgRet))
wgt

#Run the quadprog optimizer
library(quadprog)

for(i in 1:increments){
   Dmat<-2*varCov
   dvec<-c(rep(0,length(avgRet)))
   Amat<-cbind(rep(1,length(avgRet)),avgRet,diag(1,nrow=ncol(logRet)))
bvec<-c(1,tgt.ret[i],rep(0,ncol(logRet)))
soln<-solve.QP(Dmat,dvec,Amat,bvec=bvec,meq=2)
tgt.sd[i]<-sqrt(soln$value)
wgt[i,]<-soln$solution
}
head(tgt.sd)
tail(tgt.sd)
head(wgt)
tail(wgt)

wgt[,1]<-ifelse(abs(wgt[,1])<=0.0000001,0,wgt[,1])
wgt[,2]<-ifelse(abs(wgt[,2])<=0.0000001,0,wgt[,2])
wgt[,3]<-ifelse(abs(wgt[,3])<=0.0000001,0,wgt[,3])
wgt[,4]<-ifelse(abs(wgt[,4])<=0.0000001,0,wgt[,4])
wgt[,5]<-ifelse(abs(wgt[,5])<=0.0000001,0,wgt[,5])

colnames(wgt)<-c("Bbk.Wgt","EABL.Wgt","Brit.Wgt","Cen.Wgt","KPLC.Wgt")
head(wgt)
tail(wgt)


#check if the weights add up to one
check<-rowSums(wgt)
check

#combine portfolio returns, portfolio standard deviation and portfolio weights
tgtPortf<-data.frame(cbind(wgt,tgt.ret,tgt.sd))
tgtPortf
head(tgtPortf)
tail(tgtPortf)

#Identify the minimum variance portfolio(Portfolio with the lowest standard deviation)
minvarPortf<-subset(tgtPortf,tgtPortf$tgt.sd==min(tgtPortf$tgt.sd))
minvarPortf

#Calculate the Sharpe Ratio and identify the portfolio with highest Sharpe Ratio
rF<-0.09
rF

tgtPortf$Sharpe_Ratio<-(tgtPortf$tgt.ret-rF)/tgtPortf$tgt.sd
head(tgtPortf)
tail(tgtPortf)

#tangency portfolio(Highest Ratio)
tan.port<-subset(tgtPortf,tgtPortf$Sharpe_Ratio==max(tgtPortf$Sharpe_Ratio))
tan.port
tansd<-tan.port$tgt.sd
tansVar<-(tansd)^2
tansVar

#Efficient Portfolios(Portfolios with returns higher than the min.var portfolio)
EffPortf<-subset(tgtPortf,tgtPortf$tgt.ret>=minvarPortf$tgt.ret)
EffPortf
head(EffPortf)
plot(x=tgt.sd,y=tgt.ret,xlab="Portfolio Risk",ylab="Portfolio Returns",main="Mean Variance Efficient Frontier of Three Assets",col="Black")

#Threshold accepting algorithm
#Initialize n Rounds and n Steps
n=250 #number of rounds
n=250 #number of steps
	
#Set the threshold required Tr
Tr=-0.0005474836#we assume our minimum portfolio return to be the MVO return 
	
#Randomly generate current solution XC ? X
#for r = 1:n Rounds do

for(r in 1:n)
	{
Xc=rnorm(250)
	}
Xc


#for i = 1: n Steps do
i=1:n
#Generate Xn ? N (XC) and compute change = F (Xn) - F (Xc)
for(i in 1:n)
change=0
	{
Xn<-rnorm(250)
diff.change=Xc[i]-Xn[i]
mean.change=mean(diff.change)
	}
change=mean.change
change
mean(Xn)
var(Xn)

#if change< Tr then Xc = Xn

if(change<Tr)
	{
portiflio.returnTA<-mean(Xn)
portiflio.variance.TA<-var(Xn)
	}
portiflio.returnTA=mean(Xn)
portiflio.returnTA
portiflio.variance.TA=var(Xn)
portiflio.variance.TA

	#dataframe for the TA algorithim fitted results

TA.fitted.outputs<-data.frame(threshold=Tr,portiflio.returnTA,portiflio.variance.TA)
TA.fitted.outputs
