# mogelijke veranderingen!!!
###################
# 1. 
###################

library(gamlss)
library(eeptools)

# samengevoegde data
A2<-read.table('Discrep2_data.txt',header=T)
Am<-A2[A2$sekse==1,]
Aj<-A2[A2$sekse==0,]

### de conditionele Johnson SU verdeling en mu, sigma, nu en tau ###

ODTm<-gamlss(DT~cs(lftd,cv=T),sigma.formula=~lftd,nu.formula=~lftd,tau.formula=~lftd,weights=psw,family=JSU,data=Am)
summary(ODTm)
ODTj<-gamlss(DT~lftd,sigma.formula=~lftd,nu.formula=~lftd,tau.formula=~lftd,weights=psw,family=JSU,data=Aj)
summary(ODTj)

# worm plot
wp(resid=resid(ODTm),ylim.all=6/10 * sqrt(1/length(resid)))
wp(resid=resid(ODTm))
fittedPlot(ODTm,x=Am$lftd)
wp(resid=resid(ODTj),ylim.all=6/10 * sqrt(1/length(resid)))
wp(resid=resid(ODTj))
fittedPlot(ODTj,x=Aj$lftd)

plot(ODTm)
pdf('Discrep2_plot_m.pdf')
centiles(ODTm,Am$lftd,cent=c(1,5,10,25,50,75,90,95,99),xlab='Leeftijd in dagen',pch=1,ylab='Score (Ln goed-fout verhouding)',main="Leeuwenspel: percentielcurves voor meisjes")
dev.off()
plot(ODTj)
pdf('Discrep2_plot_j.pdf')
centiles(ODTj,Aj$lftd,cent=c(1,5,10,25,50,75,90,95,99),xlab='Leeftijd in dagen',pch=1,ylab='Score (Ln goed-fout verhouding)',main="Leeuwenspel: percentielcurves voor jongens")
dev.off()

# onderstaande functie pcds2() geeft voor leeftijd x (in dagen), discrepantie score y en sekse z (1=meisje/2=jongen) de proportie kinderen van dezelfde leeftijd en sekse die een lagere discrepantie score hebben

pcds2<-function(x,y,z){
newx<-data.frame(lftd=x)
if (z==1){
mu<-predict(ODTm,what='mu',newdata=newx,type='response',data=Am)
sigma<-predict(ODTm,what='sigma',newdata=newx,type='response',data=Am)
nu<-predict(ODTm,what='nu',newdata=newx,type='response',data=Am)
tau<-predict(ODTm,what='tau',newdata=newx,type='response',data=Am)
pcy<-pJSU(y,mu,sigma,nu,tau)*100
cat(pcy)}
if (z==2){
mu<-predict(ODTj,what='mu',newdata=newx,type='response',data=Aj)
sigma<-predict(ODTj,what='sigma',newdata=newx,type='response',data=Aj)
nu<-predict(ODTj,what='nu',newdata=newx,type='response',data=Aj)
tau<-predict(ODTj,what='tau',newdata=newx,type='response',data=Aj)
pcy<-pJSU(y,mu,sigma,nu,tau)*100
cat(pcy)}
}

# leeftijdsrange: 2000-5000
pcds2(3000,0,1)
pcds2(3000,0,2)
