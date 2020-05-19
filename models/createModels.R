####
# This script is used to calculate the models and store them. 
# In the r-shiny app the stored models are fetched, this increases the performance 
# of the shiny application
####

library(gamlss)
library(eeptools)

#####
# calculate the Lion Model
#####

D<-read.table('../original_analysis/Lion1_Sample.dat',header=T)
names(D)<-c('lftd','score','sekse','noord','oost','zuid','west','regio','lftj','psw')
LOscore<-log(D$score/(1-D$score))

saveRDS(LOscore, file="lionLogData.rds")
storedLogData <- readRDS("lionLogData.rds")
identical(LOscore, storedLogData, ignore.environment = TRUE)

D<-data.frame(D,LOscore)
saveRDS(D, file="lionData.rds")
storedData <- readRDS("lionData.rds")
identical(D, storedData, ignore.environment = TRUE)

out<-gamlss(
    LOscore~cs(lftd,cv=T),
    sigma.formula=~cs(lftd,df=0.8),
    nu.formula=~cs(lftd,df=0.8),
    tau.formula=~cs(lftd,df=0.8),
    weights=psw,
    family=JSU,
    data=D
)
saveRDS(out, file="lionModel.rds")
lionModel <- readRDS("lionModel.rds")
identical(out, lionModel, ignore.environment = TRUE)

#####
# calculate the Lion Model For boys
#####
D<-read.table('../original_analysis/Lion2_Sample.dat',header=T)
names(D)<-c('lftd','score','sekse','noord','oost','zuid','west','regio','lftj','psw')
LOscore<-log(D$score/(1-D$score))
saveRDS(LOscore, file="lionLogDataBoys.rds")
storedLogData <- readRDS("lionLogDataBoys.rds")
identical(LOscore, storedLogData, ignore.environment = TRUE)

D<-data.frame(D,LOscore)
Dj<-D[D$sekse==0,]
saveRDS(Dj, file="lionDataBoys.rds")
storedData <- readRDS("lionDataBoys.rds")
identical(Dj, storedData, ignore.environment = TRUE)

### de conditionele Johnson SU verdeling en mu, sigma, nu en tau ###
outj<-gamlss(
  LOscore~cs(lftd,cv=T),
  sigma.formula=~1,
  nu.formula=~cs(lftd,df=0.8),
  tau.formula=~cs(lftd,df=1),
  weights=psw,
  family=JSU,
  data=Dj)

saveRDS(outj, file="lionModelBoys.rds")
lionModel <- readRDS("lionModelBoys.rds")
identical(outj, lionModel, ignore.environment = TRUE)

#####
# calculate the Lion Model For girls
#####
D<-read.table('../original_analysis/Lion2_Sample.dat',header=T)
names(D)<-c('lftd','score','sekse','noord','oost','zuid','west','regio','lftj','psw')
LOscore<-log(D$score/(1-D$score))
saveRDS(LOscore, file="lionLogDataGirls.rds")
storedLogData <- readRDS("lionLogDataGirls.rds")
identical(LOscore, storedLogData, ignore.environment = TRUE)

D<-data.frame(D,LOscore)
Dm<-D[D$sekse==1,]
saveRDS(Dm, file="lionDataGirls.rds")
storedData <- readRDS("lionDataGirls.rds")
identical(Dm, storedData, ignore.environment = TRUE)

outm<-gamlss(
  LOscore~cs(lftd,cv=T),
  sigma.formula=~cs(lftd,df=0.8),
  nu.formula=~cs(lftd,df=0.8),
  tau.formula=~cs(lftd,df=0.8),
  weights=psw,
  family=JSU,
  data=Dm)

saveRDS(outm, file="lionModelGirls.rds")
lionModel <- readRDS("lionModelGirls.rds")
identical(outm, lionModel, ignore.environment = TRUE)

#####
# calculate the Monkey Model
#####

D<-read.table('../original_analysis/Monkey_Sample.dat',header=T)
names(D)<-c('lftd','score','sekse','noord','oost','zuid','west','regio','lftj','psw')
LOscore<-log(D$score/(1-D$score))

saveRDS(LOscore, file="monkeyLogData.rds")
storedLogData <- readRDS("monkeyLogData.rds")
identical(LOscore, storedLogData, ignore.environment = TRUE)

D<-data.frame(D,LOscore)
saveRDS(D, file="monkeyData.rds")
storedData <- readRDS("monkeyData.rds")
identical(D, storedData, ignore.environment = TRUE)
### de conditionele Johnson SU verdeling en mu, sigma, nu en tau ###

out<-gamlss(
    LOscore~cs(lftd,df=2),
    sigma.formula=~cs(lftd,df=0.6),
    nu.formula=~cs(lftd,df=2),
    tau.formula=~1,
    weights=psw,
    family=JSU,data=D)

saveRDS(out, file="monkeyModel.rds")
monkeyModel <- readRDS("monkeyModel.rds")
identical(out, monkeyModel, ignore.environment = TRUE)

#####
# calculate the Monkey Model for boys
#####
D<-read.table('../original_analysis/Monkey2_Sample.dat',header=T)
names(D)<-c('lftd','score','sekse','noord','oost','zuid','west','regio','lftj','psw')
LOscore<-log(D$score/(1-D$score))

saveRDS(LOscore, file="monkeyLogDataBoys.rds")
storedLogData <- readRDS("monkeyLogDataBoys.rds")
identical(LOscore, storedLogData, ignore.environment = TRUE)

D<-data.frame(D,LOscore)
Dj<-D[D$sekse==0,]
saveRDS(Dj, file="monkeyDataBoys.rds")
storedData <- readRDS("monkeyDataBoys.rds")
identical(Dj, storedData, ignore.environment = TRUE)

### de conditionele Johnson SU verdeling en mu, sigma, nu en tau ###
outj<-gamlss(
  LOscore~cs(lftd,df=2.2),
  sigma.formula=~lftd,
  nu.formula=~lftd,
  tau.formula=~lftd,
  weights=psw,
  family=JSU,
  data=Dj)

saveRDS(outj, file="monkeyModelBoys.rds")
monkeyModel <- readRDS("monkeyModelBoys.rds")
identical(outj, monkeyModel, ignore.environment = TRUE)

#####
# calculate the Monkey Model for girls
#####
D<-read.table('../original_analysis/Monkey2_Sample.dat',header=T)
names(D)<-c('lftd','score','sekse','noord','oost','zuid','west','regio','lftj','psw')
LOscore<-log(D$score/(1-D$score))

saveRDS(LOscore, file="monkeyLogDataGirls.rds")
storedLogData <- readRDS("monkeyLogDataGirls.rds")
identical(LOscore, storedLogData, ignore.environment = TRUE)

D<-data.frame(D,LOscore)
Dm<-D[D$sekse==1,]

saveRDS(Dm, file="monkeyDataGirls.rds")
storedData <- readRDS("monkeyDataGirls.rds")
identical(Dm, storedData, ignore.environment = TRUE)

### de conditionele Johnson SU verdeling en mu, sigma, nu en tau ###
outm<-gamlss(
  LOscore~cs(lftd,df=2),
  sigma.formula=~cs(lftd,df=1.3),
  nu.formula=~cs(lftd,df=1),
  tau.formula=~1,
  weights=psw,
  family=JSU,
  data=Dm)

saveRDS(outm, file="monkeyModelGirls.rds")
monkeyModel <- readRDS("monkeyModelGirls.rds")
identical(outm, monkeyModel, ignore.environment = TRUE)
