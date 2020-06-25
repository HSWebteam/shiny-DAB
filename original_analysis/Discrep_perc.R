# mogelijke veranderingen!!!
###################
# 1. 
###################

library(gamlss)
library(eeptools)

# samengevoegde data
A<-read.table('Discrep_data.txt',header=T)

### de conditionele Johnson SU verdeling en mu, sigma, nu en tau ###

#ODT<-gamlss(DT~cs(lftd,cv=T),sigma.formula=~cs(lftd,df=0.8),nu.formula=~cs(lftd,df=0.8),tau.formula=~cs(lftd,df=0.8),weights=psw,family=JSU,data=A)
ODT<-gamlss(DT~lftd,sigma.formula=~lftd,nu.formula=~lftd,tau.formula=~lftd,weights=psw,family=JSU,data=A)
summary(ODT)

# worm plot
wp(resid=resid(ODT),ylim.all=6/10 * sqrt(1/length(resid)))
wp(resid=resid(ODT))
fittedPlot(ODT,x=A$lftd)

plot(ODT)
pdf('Discrep_plot.pdf')
centiles(ODT,A$lftd,cent=c(1,5,10,25,50,75,90,95,99),xlab='Leeftijd in dagen',pch=1,ylab='Discrepantie-score (L-A)',main="Percentielcurves discrepantie-score (L-A)")
dev.off()

# onderstaande functie pcds() geeft voor leeftijd x (in dagen) en discrepantie score y de proportie kinderen van dezelfde leeftijd die een discrepantie score hebben

pcds<-function(x,y){
newx<-data.frame(lftd=x)
mu<-predict(ODT,what='mu',newdata=newx,type='response',data=A)
sigma<-predict(ODT,what='sigma',newdata=newx,type='response',data=A)
nu<-predict(ODT,what='nu',newdata=newx,type='response',data=A)
tau<-predict(ODT,what='tau',newdata=newx,type='response',data=A)
pcy<-pJSU(y,mu,sigma,nu,tau)*100
#if (pcy<=100){pcw<-c('Zeer hoog\n')}
#if (pcy<95){pcw<-c('Hoog\n')}
#if (pcy<90){pcw<-c('Boven gemiddeld\n')}
#if (pcy<75){pcw<-c('Gemiddeld\n')}
#if (pcy<25){pcw<-c('Beneden gemiddeld\n')}
#if (pcy<10){pcw<-c('Laag\n')}
#if (pcy<5){pcw<-c('Zeer laag\n')}
#cat(pcy,pcw)
cat(pcy)}

# leeftijdsrange: 2000-5000
pcds(3000,0)

