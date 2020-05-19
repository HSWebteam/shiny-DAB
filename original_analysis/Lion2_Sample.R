# veranderingen!!!
###################
# 1.
# 2. 
# 3. 
###################

library(gamlss)
library(eeptools)

# hieronder wordt met de geboortedatum (gebdat) en de testdatum (testdat) de 
# leeftijd in dagen berekend (ld)
# Deze zijn uitgecommentarieerd om er voor te zorgen dat TestScriptLions.R werkt.
# gebdat<-as.Date('2010-01-01')
# testdat<-as.Date('2019-09-05 09:28:26')
# ld<-as.numeric(age_calc(gebdat,enddate=testdat,units='days'))
# score=.06

D<-read.table('Lion2_Sample.dat',header=T)
names(D)<-c('lftd','score','sekse','noord','oost','zuid','west','regio','lftj','psw')
LOscore<-log(D$score/(1-D$score))
D<-data.frame(D,LOscore)
Dm<-D[D$sekse==1,]
Dj<-D[D$sekse==0,]

### de conditionele Johnson SU verdeling en mu, sigma, nu en tau ###

outm<-gamlss(LOscore~cs(lftd,cv=T),sigma.formula=~cs(lftd,df=0.8),nu.formula=~cs(lftd,df=0.8),tau.formula=~cs(lftd,df=0.8),weights=psw,family=JSU,data=Dm)
summary(outm)
outj<-gamlss(LOscore~cs(lftd,cv=T),sigma.formula=~1,nu.formula=~cs(lftd,df=0.8),tau.formula=~cs(lftd,df=1),weights=psw,family=JSU,data=Dj)
summary(outj)

# worm plot
wp(resid=resid(outm),ylim.all=6/10 * sqrt(1/length(resid)))
wp(resid=resid(outm))
fittedPlot(outm,x=Dm$lftd)
wp(resid=resid(outj),ylim.all=6/10 * sqrt(1/length(resid)))
wp(resid=resid(outj))
fittedPlot(outj,x=Dj$lftd)

plot(outm)
pdf('output/Lion2_Sample_m.pdf')
centiles(outm,Dm$lftd,cent=c(1,5,10,25,50,75,90,95,99),xlab='Leeftijd in dagen',pch=1,ylab='Score (Ln goed-fout verhouding)',main="Leeuwenspel: percentielcurves voor meisjes")
dev.off()
plot(outj)
pdf('output/Lion2_Sample_j.pdf')
centiles(outj,Dj$lftd,cent=c(1,5,10,25,50,75,90,95,99),xlab='Leeftijd in dagen',pch=1,ylab='Score (Ln goed-fout verhouding)',main="Leeuwenspel: percentielcurves voor jongens")
dev.off()

# onderstaande functie pcs() geeft voor leeftijd x (in dagen), gem. score y en geslacht z (1=meisje/2=jongen) de proportie kinderen van dezelfde leeftijd en geslacht die een lagere gem. score hebben

pcs<-function(x,y,z){
  newx<-data.frame(lftd=x)
  if (z==1){
    mu<-predict(outm,what='mu',newdata=newx,type='response',data=Dm)
    sigma<-predict(outm,what='sigma',newdata=newx,type='response',data=Dm)
    nu<-predict(outm,what='nu',newdata=newx,type='response',data=Dm)
    tau<-predict(outm,what='tau',newdata=newx,type='response',data=Dm)}
  if (z==2){
    mu<-predict(outj,what='mu',newdata=newx,type='response',data=Dj)
    sigma<-predict(outj,what='sigma',newdata=newx,type='response',data=Dj)
    nu<-predict(outj,what='nu',newdata=newx,type='response',data=Dj)
    tau<-predict(outj,what='tau',newdata=newx,type='response',data=Dj)}
  pcy<-pJSU(log(y/(1-y)),mu,sigma,nu,tau)*100
  if (pcy<=100){pcw<-c('Zeer hoog\n')}
  if (pcy<95){pcw<-c('Hoog\n')}
  if (pcy<90){pcw<-c('Boven gemiddeld\n')}
  if (pcy<75){pcw<-c('Gemiddeld\n')}
  if (pcy<25){pcw<-c('Beneden gemiddeld\n')}
  if (pcy<10){pcw<-c('Laag\n')}
  if (pcy<5){pcw<-c('Zeer laag\n')}
  cat(pcy,pcw)}

# hieronder de berekening van de proportie kinderen van leefdtijd ld en geslacht z die een lagere gem. score hebben dan .3 en de bijbehorende kwalificatie

#options(warn=-1)
pcs(ld, score, 1)
pcs(ld, score, 2)

# hieronder de functie voor de density plot

dplot<-function(x,y,z){
  newx<-data.frame(lftd=x)
  if (z==1){
    mu<-predict(outm,what='mu',newdata=newx,type='response',data=Dm)
    sigma<-predict(outm,what='sigma',newdata=newx,type='response',data=Dm)
    nu<-predict(outm,what='nu',newdata=newx,type='response',data=Dm)
    tau<-predict(outm,what='tau',newdata=newx,type='response',data=Dm)
    a<-seq(round(min(Dm$LOscore),0),round(max(Dm$LOscore),0),.01)
    b<-dJSU(a,mu,sigma,nu,tau)
    plot(a,b,type='l',ylim=c(0.01,max(b)),main='De verdeling voor meisjes gegeven leeftijd in dagen',xlab='Score (Ln goed-fout verhouding)',ylab='kansdichtheid',las=1)}
  if (z==2){
    mu<-predict(outj,what='mu',newdata=newx,type='response',data=Dj)
    sigma<-predict(outj,what='sigma',newdata=newx,type='response',data=Dj)
    nu<-predict(outj,what='nu',newdata=newx,type='response',data=Dj)
    tau<-predict(outj,what='tau',newdata=newx,type='response',data=Dj)
    a<-seq(round(min(Dj$LOscore),0),round(max(Dj$LOscore),0),.01)
    b<-dJSU(a,mu,sigma,nu,tau)
    plot(a,b,type='l',ylim=c(0.01,max(b)),main='De verdeling voor jongens gegeven leeftijd in dagen',xlab='Score (Ln goed-fout verhouding)',ylab='kansdichtheid',las=1)}
  abline(v=log(y/(1-y)),col='red')}

dplot(ld, score, 1)
dplot(ld, score, 2)

# hieronder de functie voor het betrouwbaarheidsinterval voor de proportie correct score
bi<-function(x,y,z){
  if (z==1){
    if (trunc(x/365,0)==6){u<-c(y-1.96*.0685,y+1.96*.0685)}
    if (trunc(x/365,0)==7){u<-c(y-1.96*.0666,y+1.96*.0666)}
    if (trunc(x/365,0)==8){u<-c(y-1.96*.0586,y+1.96*.0586)}
    if (trunc(x/365,0)==9){u<-c(y-1.96*.0580,y+1.96*.0580)}
    if (trunc(x/365,0)==10){u<-c(y-1.96*.0558,y+1.96*.0558)}
    if (trunc(x/365,0)==11){u<-c(y-1.96*.0513,y+1.96*.0513)}
    if (trunc(x/365,0)==12){u<-c(y-1.96*.0530,y+1.96*.0530)}}
  if (z==2){
    if (trunc(x/365,0)==6){u<-c(y-1.96*.0676,y+1.96*.0676)}
    if (trunc(x/365,0)==7){u<-c(y-1.96*.0660,y+1.96*.0660)}
    if (trunc(x/365,0)==8){u<-c(y-1.96*.0632,y+1.96*.0632)}
    if (trunc(x/365,0)==9){u<-c(y-1.96*.0593,y+1.96*.0593)}
    if (trunc(x/365,0)==10){u<-c(y-1.96*.0571,y+1.96*.0571)}
    if (trunc(x/365,0)==11){u<-c(y-1.96*.0554,y+1.96*.0554)}
    if (trunc(x/365,0)==12){u<-c(y-1.96*.0543,y+1.96*.0543)}}
  if (u[1]<0){u[1]<-0}
  if (u[2]>1){u[2]<-1}
  u}

bi(ld, score, 1)
bi(ld, score, 2)

# hieronder de functie voor de t-score en bijbehorende 95% betrouwbaarheidsinterval

tscore<-function(x,y,z){
  newx<-data.frame(lftd=x)
  if (z==1){
    mu<-predict(outm,what='mu',newdata=newx,type='response',data=Dm)
    sigma<-predict(outm,what='sigma',newdata=newx,type='response',data=Dm)
    nu<-predict(outm,what='nu',newdata=newx,type='response',data=Dm)
    tau<-predict(outm,what='tau',newdata=newx,type='response',data=Dm)
    # from standard normal to T with mean 50 and sd 10 
    tr<-qnorm(pJSU(log(y/(1-y)),mu,sigma,nu,tau))*10+50
    if (trunc(x/365,0)==6){ts<-c(tr,tr-1.96*10*sqrt(1-.86),tr+1.96*10*sqrt(1-.86))}
    if (trunc(x/365,0)==7){ts<-c(tr,tr-1.96*10*sqrt(1-.88),tr+1.96*10*sqrt(1-.88))}
    if (trunc(x/365,0)==8){ts<-c(tr,tr-1.96*10*sqrt(1-.87),tr+1.96*10*sqrt(1-.87))}
    if (trunc(x/365,0)==9){ts<-c(tr,tr-1.96*10*sqrt(1-.83),tr+1.96*10*sqrt(1-.83))}
    if (trunc(x/365,0)==10){ts<-c(tr,tr-1.96*10*sqrt(1-.84),tr+1.96*10*sqrt(1-.84))}
    if (trunc(x/365,0)==11){ts<-c(tr,tr-1.96*10*sqrt(1-.83),tr+1.96*10*sqrt(1-.83))}
    if (trunc(x/365,0)==12){ts<-c(tr,tr-1.96*10*sqrt(1-.83),tr+1.96*10*sqrt(1-.83))}}
  if (z==2){
    mu<-predict(outj,what='mu',newdata=newx,type='response',data=Dj)
    sigma<-predict(outj,what='sigma',newdata=newx,type='response',data=Dj)
    nu<-predict(outj,what='nu',newdata=newx,type='response',data=Dj)
    tau<-predict(outj,what='tau',newdata=newx,type='response',data=Dj)
    # from standard normal to T with mean 50 and sd 10 
    tr<-qnorm(pJSU(log(y/(1-y)),mu,sigma,nu,tau))*10+50
    if (trunc(x/365,0)==6){ts<-c(tr,tr-1.96*10*sqrt(1-.86),tr+1.96*10*sqrt(1-.86))}
    if (trunc(x/365,0)==7){ts<-c(tr,tr-1.96*10*sqrt(1-.87),tr+1.96*10*sqrt(1-.87))}
    if (trunc(x/365,0)==8){ts<-c(tr,tr-1.96*10*sqrt(1-.86),tr+1.96*10*sqrt(1-.86))}
    if (trunc(x/365,0)==9){ts<-c(tr,tr-1.96*10*sqrt(1-.86),tr+1.96*10*sqrt(1-.86))}
    if (trunc(x/365,0)==10){ts<-c(tr,tr-1.96*10*sqrt(1-.84),tr+1.96*10*sqrt(1-.84))}
    if (trunc(x/365,0)==11){ts<-c(tr,tr-1.96*10*sqrt(1-.86),tr+1.96*10*sqrt(1-.86))}
    if (trunc(x/365,0)==12){ts<-c(tr,tr-1.96*10*sqrt(1-.88),tr+1.96*10*sqrt(1-.88))}}
  ts}

tscore(ld, score, 1)
tscore(ld, score, 2)

# hieronder de functie voor het plotten van iemands score gegeven leeftijd t.o.v. de percentielcurves

grafiek<-function(x,y,z){
  par(las=1)
  if (z==1){
    pdf('output/Lion_Score_m.pdf')
    centiles(outm,Dm$lftd/365,legend=FALSE,
             cent=c(5,10,25,50,75,90,95),
             xlim=c(6,13),xaxp=c(6,13,7),
             ylim=c(-4,4),yaxp=c(-4,4,4),
             col.centiles=c(1,3,4,5,6,7,'orange'),
             lwd.centiles=1.5,
             xlab='Leeftijd in jaren',
             ylab='Score (Ln goed-fout verhouding)',
             main='Leeuwenspel: percentielcurves voor meisjes',
             points=F)}
  if (z==2){
    pdf('output/Lion_Score_j.pdf')
    centiles(outj,Dj$lftd/365,legend=FALSE,
             cent=c(5,10,25,50,75,90,95),
             xlim=c(6,13),xaxp=c(6,13,7),
             ylim=c(-4,4),yaxp=c(-4,4,4),
             col.centiles=c(1,3,4,5,6,7,'orange'),
             lwd.centiles=1.5,
             xlab='Leeftijd in jaren',
             ylab='Score (Ln goed-fout verhouding)',
             main='Leeuwenspel: percentielcurves voor jongens',
             points=F)}
  abline(h=log(y/(1-y)),col='black',lty='dashed',lwd=1.5)
  abline(v=x/365,col='black',lty='dashed',lwd=1.5)
  #abline(h=seq(-3,3,1/2),col='lightgray')
  #abline(v=seq(6,13,1/2),col='lightgray')
  legend(11.8,-1.35,legend=c('95','90','75','50','25','10','5'),col=c('orange',7,6,5,4,3,1),lwd=1.5,bty='o',bg='white')
  points(x/365,log(y/(1-y)),pch=19,col="red")
  dev.off()}

grafiek(ld, score, 1)
grafiek(ld, score, 2)
