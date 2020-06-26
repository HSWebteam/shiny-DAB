# mogelijke veranderingen!!!
###################
# 1. 
###################

library(gamlss)
library(eeptools)

# hieronder wordt met de geboortedatum (gebdat) en de testdatum (testdat) de leeftijd in dagen berekend (ld)
# Deze zijn uitgecommentarieerd om er voor te zorgen dat TestScriptDiscrepantie.R werkt.
# gebdat<-as.Date('2007-02-11')
# testdat<-as.Date('2019-02-11')
# ld<-as.numeric(age_calc(gebdat,enddate=testdat,units='days'))

# data Leeuwenspel
D1<-read.table('Lion1_Sample.dat',header=T)
names(D1)<-c('lftd','score','sekse','noord','oost','zuid','west','regio','lftj','psw')
LOscore<-log(D1$score/(1-D1$score))
D1<-data.frame(D1,LOscore)

### de conditionele Johnson SU verdeling en mu, sigma, nu en tau ###

out1<-gamlss(LOscore~cs(lftd,cv=T),sigma.formula=~cs(lftd,df=0.8),nu.formula=~cs(lftd,df=0.8),tau.formula=~cs(lftd,df=0.8),weights=psw,family=JSU,data=D1)

# data Apensepl
D2<-read.table('Monkey_Sample.dat',header=T)
names(D2)<-c('lftd','score','sekse','noord','oost','zuid','west','regio','lftj','psw')
LOscore<-log(D2$score/(1-D2$score))
D2<-data.frame(D2,LOscore)

### de conditionele Johnson SU verdeling en mu, sigma, nu en tau ###

out2<-gamlss(LOscore~cs(lftd,df=2),sigma.formula=~cs(lftd,df=0.6),nu.formula=~cs(lftd,df=2),tau.formula=~1,weights=psw,family=JSU,data=D2)

# hieronder de functie disc() voor de discrepantie tussen t-scores
# x is leeftijd in dagen, y1 en y2 zijn proporties correct
# y1 is de proportie correct leeuwenspel
# y2 is de proportie correct apenspel
# a is het nominale significantieniveau in procenten

disc<-function(x,y1,y2,a){
newx<-data.frame(lftd=x)
mu1<-predict(out1,what='mu',newdata=newx,type='response',data=D1)
sigma1<-predict(out1,what='sigma',newdata=newx,type='response',data=D1)
nu1<-predict(out1,what='nu',newdata=newx,type='response',data=D1)
tau1<-predict(out1,what='tau',newdata=newx,type='response',data=D1)
tr1<-qnorm(pJSU(log(y1/(1-y1)),mu1,sigma1,nu1,tau1))*10+50 # from standard normal to T with mean 50 and sd 10 
mu2<-predict(out2,what='mu',newdata=newx,type='response',data=D2)
sigma2<-predict(out2,what='sigma',newdata=newx,type='response',data=D2)
nu2<-predict(out2,what='nu',newdata=newx,type='response',data=D2)
tau2<-predict(out2,what='tau',newdata=newx,type='response',data=D2)
tr2<-qnorm(pJSU(log(y2/(1-y2)),mu2,sigma2,nu2,tau2))*10+50 # from standard normal to T with mean 50 and sd 10 

print(tr1)
print(tr2)
if (trunc(x/365,0)==6){
se1<-10*sqrt(1-.86)
se2<-10*sqrt(1-.87)}
if (trunc(x/365,0)==7){
se1<-10*sqrt(1-.88)
se2<-10*sqrt(1-.86)}
if (trunc(x/365,0)==8){
se1<-10*sqrt(1-.87)
se2<-10*sqrt(1-.82)}
if (trunc(x/365,0)==9){
se1<-10*sqrt(1-.85)
se2<-10*sqrt(1-.82)}
if (trunc(x/365,0)==10){
se1<-10*sqrt(1-.84)
se2<-10*sqrt(1-.80)}
if (trunc(x/365,0)==11){
se1<-10*sqrt(1-.85)
se2<-10*sqrt(1-.81)}
if (trunc(x/365,0)==12){
se1<-10*sqrt(1-.86)
se2<-10*sqrt(1-.84)}
if (tr1-tr2>=0){z<-qnorm(1-a/200)}
if (tr1-tr2<0){z<-qnorm(a/200)}
cr<-z*sqrt(se1^2+se2^2)
return(c(tr1-tr2, cr))}

disc(ld,.3,.35,5)
disc(ld,.3,.35,10)
