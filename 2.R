da=read.table('m-ibmsp6709.txt',header = T)
ibm=da$ibm
sp=da$sp
cor(sp,ibm)
cor(sp,ibm,method='spearman')
cor(sp,ibm,method = 'kendall')
da=read.table('m-dec12910.txt',header = T)
d10=da$dec10
dec10=ts(d10,frequency = 12,start = c(1967,1))
par(mfcol=c(2,1))
plot(dec10,xlab='year',ylab='returns')
title(main='(a):Simple returns')
f1=acf(d10,lag=13)
f1$acf
tt=f1$acf[13]*sqrt(516)
da=read.table('m-ibmsp6709.txt',header=T)
ibm=da$ibm
lnibm=log(1+ibm)
Box.test(ibm,lag=12,type='Ljung')
Box.test(lnibm,lag=12,type='Ljung')
da=read.table('q-gnp4710.txt',header = T)
G=da$VALUE
LG=log(G)
gnp=diff(LG)
dim(da)
tdx=c(1:253)/4+1947
par(mfcol=c(2,1))
plot(tdx,LG,xlab='year',ylab='Log(GNP)',type='l')
plot(tdx[2:253],gnp,type='l',xlab='year',ylab='growth')
acf(gnp,lag=12)
pacf(gnp,lag=12)
m1=arima(gnp,order=c(3,0,0))
tsdiag(m1,gof=12)
p1=c(1,-m1$coef[1:3])
r1=polyroot(p1)
Mod(r1)
k=2*pi/acos(1.616116/1.832674)
mm1=ar(gnp,method = 'mle')
print(mm1$aic,digits = 3)
aic=mm1$aic
length(aic)
plot(c(0:12),aic,type='h',xlab='order',ylab='aic')
lines(0:12,aic,lty=2)
vw=read.table('m-ibm3dx2609.txt',header = T)
vw=vw[,3]
vw=read.table('m-ibm3dx2608.txt',header = T)[,3]
m3=arima(vw,order = c(3,0,0))
(1-.1158+.0187+.1042)*mean(vw)
sqrt(m3$sigma2)
Box.test(m3$residuals,lag=12,type='Ljung')
pv=1-pchisq(16.35,9)
m3=arima(vw,order=c(3,0,0),fixed = c(NA,0,NA,NA))
m3$coef
(1-.1136+.1063)*.0089
sqrt(m3$sigma2)
Box.test(m3$residuals,lag=12,type='Ljung')
pv=1-pchisq(16.83,10)
da=read.table('m-ibm3dx2608.txt',header=T)
head(da)
ew=da$ewrtn#等权重指数
m1=arima(ew,order=c(0,0,9))
m1=arima(ew,order=c(0,0,9),fixed=c(NA,0,NA,0,0,0,0,0,NA,NA))
sqrt(m1$sigma2)
Box.test(m1$residuals,lag=12,type='Ljung')
pv=1-pchisq(17.6,9)
m1=arima(ew[1:986],order=c(0,0,9),fixed = c(NA,0,NA,0,0,0,0,0,NA,NA))




