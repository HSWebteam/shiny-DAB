####
# This script is used to calculate the models and store them. 
# In the r-shiny app the stored models are fetched, this increases the performance 
# of the shiny application
####

library(gamlss)
library(eeptools)


D<-read.table('Lion1_Sample.dat',header=T)
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
# calculate the Monkey Model
#####

D<-read.table('Monkey_Sample.dat',header=T)
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
lionModel <- readRDS("monkeyModel.rds")
identical(out, lionModel, ignore.environment = TRUE)