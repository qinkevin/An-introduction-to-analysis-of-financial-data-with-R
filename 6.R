setwd('/Users/qinkevin/PycharmProjects/An-introduction-to-analysis-of-financial-data-with-R')
da=read.table('taq-cat-t-jan042010.txt',header = T)
vol=da$size/100
da1=read.table('taq-cat-cpch-jan042010.txt')
cpch=da1[,1]
pch=da1[,2]
cf=as.factor(cpch)
y=cf[4:37715]
y1=cf[3:37714]
y2=cf[2:37713]
vol=vol[2:37716]
v2=vol[2:37713]
cp1=pch[3:37714]
cp2=pch[2:37713]
cp3=pch[1:37712]
library(MASS)
m1=polr(y~v2+cp1+cp2+cp3+y1+y2,method='probit')
da=read.table('taq-cat-cpch-jan042010.txt')
pch=da[,2]
idx=c(1:37715)[pch>0]
jdx=c(1:37715)[pch<0]
A=rep(0,37715)
A[idx]=1
A[jdx]=1
D=rep(0,37715)
D[idx]=1
D[jdx]=-1
S=abs(da[,1]-4)
Ai=A[2:37715]
Aim1=A[1:37714]
Di=D[2:37715]
Dim1=D[1:37714]
Si=S[2:37715]
Sim1=S[1:37714]
m1=glm(Ai~Aim1,family = 'binomial')
di=Di[Ai==1]
dim1=Dim1[Ai==1]
di=(di+abs(di))/2
m2=glm(di~dim1,family = 'binomial')
si=Si[Di==1]
sim1=Sim1[Di==1]
source('GeoSize.R')
m3=GeoSize(si,sim1)
nsi=Si[Di==-1]
nsim1=Sim1[Di==-1]
m4=GeoSize(nsi,nsim1)
da=read.table('taq-cat-t-jan04t082010.txt',header = T)
sec=3600*da$hour+60*da$minute+da$second
ist=3600*9+30*60
end=3600*16
lunch=3600*12
length(sec)
idx=c(1:155267)[sec<ist]
jdx=c(1:155267)[sec>end]
sec=sec[-c(idx,jdx)]
dt=diff(sec)
kdx=c(1:length(dt))[dt>0]
length(kdx)
ti=sec[2:155077]
dt=dt[kdx]
ti=ti[kdx]
plot(dt,type='l',xlab='index',ylab='duration')
st=3600*6.5
f1=(ti-lunch)/st
ft=cbind(f1,f1^2)
m2=lm(log(dt)~ft)
fit=m2$fitted.values
adjdt=dt/exp(fit)
plot(adjdt,type='l',xlab='index',ylab='Adjdt')
source('acd.R')
m2=acd(adjdt,order=c(1,1),cond.dist = 'exp')
names(m2)
m5=acd(adjdt,order=c(1,2),cond.dist = 'weibull')
ep5=m5$epsilon
acf(ep5,ylim=c(-0.05,0.25))
adt1=adjdt[1:1200]
plot(adt1,type='l')
m6=acd(adt1,order=c(1,1),cond.dist = 'weibull')
ep6=m6$epsilon
Box.test(ep6,lag=10,type='Ljung')
Box.test(ep6,lag=20,type='Ljung')
Box.test(ep6^2,lag=10,type='Ljung')
Box.test(ep6^2,lag=20,type='Ljung')
par(mfcol=c(2,1))
plot(ep6,type='l',xlab='index',ylab='epsilon_t')
acf(ep6,ylim=c(-0.1,.25))

source('hfanal.R')
da=read.table('taq-cat-jan2010.txt',header = T)
m1=hfanal(da,1)

da=read.table('d-ibm-0110.txt',header = T)
ibm=log(da[,2]+1)*100
source('RMfit.R')
mm=RMfit(ibm)

da1=read.table('d-useu0111.txt',header = T)
par(mfcol=c(2,1))
rate=da1$rate
plot(rate,type='l')
rt=diff(log(rate))
plot(rt,type='l')
m2=RMfit(rt)

xt=-log(da$return+1)
library(fGarch)
m1=garchFit(~garch(1,1),data = xt,trace = F)
predict(m1,3)
source('RMeasure.R')
m11=RMeasure(-.000601,.0078243)
m2=garchFit(~garch(1,1),data=xt,trace = F,cond.dist = 'std')
predict(m2,3)
m22=RMeasure(-.0004113,.0081009,cond.dist = 'std',df=5.751)
M1=predict(m1,15)
names(M1)
mf=M1$meanForecast
merr=M1$meanError
pmean=sum(mf)
pvar=sum(merr^2)
pstd=sqrt(pvar)
M11=RMeasure(pmean,pstd)

source('SimGarcht.R')
vol=volatility(m2)
a1=c(1.922*10^(-6),0.06448)
b1=0.9286
mu=-4.113*10^(-4)
ini=c(ibm[2515],vol[2515])
mm=SimGarcht(h=15,mu=mu,alpha=a1,b1=b1,df=5.751,ini=ini,nter=30000)
rr=mm$rtn
mean(rr)
quantile(rr,c(0.95,0.99))
idx=c(1:30000)[rr>0.0479]


da=read.table('d-ibm-0110.txt',header = T)
ibm=-log(da[,2]+1)
prob1=c(0.9,0.95,0.99,0.999)
quantile(ibm,prob1)
sibm=sort(ibm)
es=sum(sibm[2390:2515])/(2515-2389)

dd=read.table('d-ibm-rq.txt',header = T)
dd[,3]=dd[,3]/100
library(quantreg)
mm=rq(nibm~vol+vix,tau=0.95,data=dd)
summary(mm)
names(mm)
fit=mm$fitted.values
tdx=c(2:2515)/252+2001
plot(tdx,dd$nibm,type='l',xlab='year',ylab='neg-log-rtn')
lines(tdx,fit,col='red')
v1[2515]


