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

