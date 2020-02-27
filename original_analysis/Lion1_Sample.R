# veranderingen!!!
###################
# 1. functie pcs is aangepast. Deze heeft nu ook een kwalificatie als output.
# 2. log goud-fout verhouding is doorgevoerd
# 3. nieuwe functie grafiek()
###################

library(gamlss)
library(eeptools)

D<-read.table('Lion1_Sample.dat',header=T)
names(D)<-c('lftd','score','sekse','noord','oost','zuid','west','regio','lftj','psw')
LOscore<-log(D$score/(1-D$score))
D<-data.frame(D,LOscore)

### de conditionele Johnson SU verdeling en mu, sigma, nu en tau ###

out<-gamlss(LOscore~cs(lftd,cv=T),sigma.formula=~cs(lftd,df=0.8),nu.formula=~cs(lftd,df=0.8),tau.formula=~cs(lftd,df=0.8),weights=psw,family=JSU,data=D)
summary(out)

# worm plot
wp(resid=resid(out),ylim.all=6/10 * sqrt(1/length(resid)))
wp(resid=resid(out))
fittedPlot(out,x=D$lftd)

plot(out)
pdf('Lion1_Sample.pdf')
centiles(out,D$lftd,cent=c(1,5,10,25,50,75,90,95,99),xlab='Leeftijd in dagen',pch=1,ylab='Gemiddelde score',main="Leeuwenspel: percentielcurves")
dev.off()

# onderstaande functie pcs() geeft voor leeftijd x (in dagen) en gem. score y de proportie kinderen van dezelfde leeftijd die een lagere gem. score hebben

pcs<-function(x,y){
newx<-data.frame(lftd=x)
mu<-predict(out,what='mu',newdata=newx,type='response',data=D)
sigma<-predict(out,what='sigma',newdata=newx,type='response',data=D)
nu<-predict(out,what='nu',newdata=newx,type='response',data=D)
tau<-predict(out,what='tau',newdata=newx,type='response',data=D)
pcy<-round(pJSU(log(y/(1-y)),mu,sigma,nu,tau)*100,1)
if (pcy<=100){pcw<-c('Zeer hoog\n')}
if (pcy<95){pcw<-c('Hoog\n')}
if (pcy<90){pcw<-c('Boven gemiddeld\n')}
if (pcy<75){pcw<-c('Gemiddeld\n')}
if (pcy<25){pcw<-c('Beneden gemiddeld\n')}
if (pcy<10){pcw<-c('Laag\n')}
if (pcy<5){pcw<-c('Zeer laag\n')}
cat(pcy,pcw)}

# hieronder wordt met de geboortedatum (gebdat) en de testdatum (testdat) de leeftijd in dagen berekend (ld)

gebdat<-as.Date('2012-02-11')
testdat<-as.Date('2019-02-11')
ld<-as.numeric(age_calc(gebdat,enddate=testdat,units='days'))

# hieronder de berekening van de proportie kinderen van leefdtijd ld die een lagere gem. score hebben dan .3 en de bijbehorende kwalificatie

pcs(ld,.3)

# hieronder de functie voor de density plot

dplot<-function(x,y){
newx<-data.frame(lftd=x)
mu<-predict(out,what='mu',newdata=newx,type='response',data=D)
sigma<-predict(out,what='sigma',newdata=newx,type='response',data=D)
nu<-predict(out,what='nu',newdata=newx,type='response',data=D)
tau<-predict(out,what='tau',newdata=newx,type='response',data=D)
a<-seq(round(min(LOscore),0),round(max(LOscore),0),.01)
b<-dJSU(a,mu,sigma,nu,tau)
plot(a,b,type='l',ylim=c(0.01,max(b)),main='De verdeling gegeven leeftijd in dagen',xlab='de log goed-fout verhouding',ylab='kansdichtheid',las=1)
abline(v=log(y/(1-y)),col='red')}

dplot(ld,.3)

# hieronder de functie voor het betrouwbaarheidsinterval voor de proportie correct score

bi<-function(x,y){
if (trunc(x/365,0)==6){z<-c(y-1.96*.0683,y+1.96*.0683)}
if (trunc(x/365,0)==7){z<-c(y-1.96*.0636,y+1.96*.0636)}
if (trunc(x/365,0)==8){z<-c(y-1.96*.0626,y+1.96*.0626)}
if (trunc(x/365,0)==9){z<-c(y-1.96*.0561,y+1.96*.0561)}
if (trunc(x/365,0)==10){z<-c(y-1.96*.0572,y+1.96*.0572)}
if (trunc(x/365,0)==11){z<-c(y-1.96*.0536,y+1.96*.0536)}
if (trunc(x/365,0)==12){z<-c(y-1.96*.0540,y+1.96*.0540)}
if (z[1]<0){z[1]<-0}
if (z[2]>1){z[2]<-1}
round(z,3)}

bi(ld,.3)

# hieronder de functie voor de t-score en bijbehorende 95% betrouwbaarheidsinterval

tscore<-function(x,y){
newx<-data.frame(lftd=x)
mu<-predict(out,what='mu',newdata=newx,type='response',data=D)
sigma<-predict(out,what='sigma',newdata=newx,type='response',data=D)
tr<-(y-mu)*10/sigma+50
if (trunc(x/365,0)==6){ts<-c(tr,tr-1.96*10*sqrt(1-.86),tr+1.96*10*sqrt(1-.86))}
if (trunc(x/365,0)==7){ts<-c(tr,tr-1.96*10*sqrt(1-.88),tr+1.96*10*sqrt(1-.88))}
if (trunc(x/365,0)==8){ts<-c(tr,tr-1.96*10*sqrt(1-.87),tr+1.96*10*sqrt(1-.87))}
if (trunc(x/365,0)==9){ts<-c(tr,tr-1.96*10*sqrt(1-.85),tr+1.96*10*sqrt(1-.85))}
if (trunc(x/365,0)==10){ts<-c(tr,tr-1.96*10*sqrt(1-.84),tr+1.96*10*sqrt(1-.84))}
if (trunc(x/365,0)==11){ts<-c(tr,tr-1.96*10*sqrt(1-.85),tr+1.96*10*sqrt(1-.85))}
if (trunc(x/365,0)==12){ts<-c(tr,tr-1.96*10*sqrt(1-.86),tr+1.96*10*sqrt(1-.86))}
round(ts,1)}

tscore(ld,.3)

# hieronder de functie voor het plotten van iemands score gegeven leeftijd t.o.v. de percentielcurves

grafiek<-function(x,y){
pdf('Lion_Score.pdf')
par(las=1)
centiles(out,D$lftd/365,legend=FALSE,cent=c(5,10,25,50,75,90,95),xlim=c(6,13),xaxp=c(6,13,7),col.centiles=c(1,3,4,5,6,7,'orange'),lwd.centiles=1.5,xlab='Leeftijd in jaren',ylab='Log goed-fout verhouding',main='Leeuwenspel: percentielcurves',points=F)
abline(h=log(y/(1-y)),col='lightgray')
abline(v=x/365,col='lightgray')
#abline(h=seq(-3,3,1/2),col='lightgray')
#abline(v=seq(6,13,1/2),col='lightgray')
legend(11.8,-1.2,legend=c('95','90','75','50','25','10','5'),col=c('orange',7,6,5,4,3,1),lwd=1.5,bty='o',bg='white')
points(x/365,log(y/(1-y)),pch=19,col="red")
dev.off()}

grafiek(ld,.3)
