library(quantmod)
getSymbols('AAPL')
dim(AAPL)
head(AAPL)
tail(AAPL)
chartSeries(AAPL,theme = 'white')
chartSeries(AAPL)
getSymbols('AAPL',from='2005-01-02',to='2010-12-31')
head(AAPL)
getSymbols('UNRATE',src='FRED')
getSymbols('INTC',src='google')
getSymbols('^TNX')
getSymbols('AAPL',from='2007-01-03',to='2011-12-02')
AAPL.rtn<-diff(log(AAPL$AAPL.Adjusted)) #???????????????????????????
chartSeries(AAPL.rtn,theme='white')
getSymbols('^TNX',from='2007-01-03',to='2011-12-02')
TNX.rtn<-diff(TNX$TNX.Adjusted)
chartSeries(TNX.rtn,theme='white')
getSymbols('DEXUSEU',src='FRED')
USEU.rtn<-diff(log(DEXUSEU$DEXUSEU))
chartSeries(DEXUSEU,theme='white')
chartSeries(USEU.rtn,theme='white')
library(fBasics)
da=read.table("d-mmm-0111.txt",header = T)
mmm<-da[,2]#???????????????
basicStats(mmm)#??????????????????
mean(mmm)
var(mmm)
stdev(mmm)
t.test(mmm)
s3<-skewness(mmm)
T<-length(mmm)# sample size
t3<-s3/sqrt(6/T)# skewness test
pp<-2*(1-pnorm(t3))#compute p-value
s4<-kurtosis(mmm)
t4<-s4/sqrt(24/T)#kurtosis test
normalTest(mmm,method = 'jb')# jb-test
hist(mmm,nclass=30)#?????????
d1=density(mmm)#????????????
range(mmm)
plot(d1$x,d1$y,xlab='rtn',ylab='density',type='l')#normal density
x<-seq(-.1,.1,.001)
y1=dnorm(x,mean(x),stdev(x))#????????????????????????
lines(x,y1,lty=2)
getSymbols('AAPL',from='2011-01-03',to='2011-06-30')
X=AAPL[,1:4]
xx<-cbind(as.numeric(X[,1]),as.numeric(X[,2]),as.numeric(X[,3]),as.numeric(X[,4]))
source('ohlc.R')
ohlc_plot(xx,title='Apple Stock',xl='days',yl='price')
source('ma.R')
getSymbols('AAPL',from='2010-01-02',to='2011-12-08')
x1<-as.numeric(AAPL$AAPL.Close)
ma(x1,21)#????????????21??????????????????
da<-read.table('m-ibmsp-2611.txt',head=T)
ibm<-log(da$ibm+1)# transform to log returns
sp<-log(da$sp+1)
tdx<-c(1:nrow(da))/12+1926#create time index
par(mfcol=c(2,1))
plot(tdx,ibm,xlab='year',ylab='lrtn',type='l')
cor(ibm,sp)#????????????
m1=lm(ibm~sp)
summary(m1)
plot(sp,ibm,cex=0.8)
dim(da)
rt=cbind(ibm,sp)
m1=apply(rt,2,mean)
v1=cov(rt)
library('mnormt')
x=rmnorm(1029,mean=m1,varcov = v1)
dim(x)
plot(x[,2],x[,1],xlab='sim-sp',ylab='sim-ibm',cex=0.8)











