library(gamlss)
library(eeptools)

pcs <- function(ageDays,score, task){
  # fetch saved model and data
  if(task == '1') {
    storedModel <- readRDS(file = paste(getwd(), "models/lionModel.rds", sep = '/'))
    storedData <- readRDS(file = paste(getwd(), "models/lionData.rds", sep = '/'))
  }
  if(task == '2' || task == '3') {
    storedModel <- readRDS(file = paste(getwd(), "models/monkeyModel.rds", sep = '/'))
    storedData <- readRDS(file = paste(getwd(), "models/monkeyData.rds", sep = '/'))
  }

  newx <- data.frame(lftd=ageDays)
  mu <- predict(storedModel, what='mu', newdata=newx, type='response', data=storedData)
  sigma <- predict(storedModel, what='sigma', newdata=newx, type='response', data=storedData)
  nu <- predict(storedModel, what='nu', newdata=newx, type='response', data=storedData)
  tau <- predict(storedModel, what='tau', newdata=newx, type='response', data=storedData)
  round(pJSU(log(score/(1-score)), mu, sigma, nu, tau)*100, 1)
}

density <- function(ageDays,score, task){
  # fetch saved model and data
  if(task == '1') {
    storedModel <- readRDS(file = paste(getwd(), "models/lionModel.rds", sep = '/'))
    storedData <- readRDS(file = paste(getwd(), "models/lionData.rds", sep = '/'))
  }
  if(task == '2' || task == '3') {
    storedModel <- readRDS(file = paste(getwd(), "models/monkeyModel.rds", sep = '/'))
    storedData <- readRDS(file = paste(getwd(), "models/monkeyData.rds", sep = '/'))
  }

  LOscore<-log(storedData$score/(1-storedData$score))

  newx <- data.frame(lftd=ageDays)
  mu <- predict(storedModel, what='mu', newdata=newx, type='response', data=storedData)
  sigma <- predict(storedModel, what='sigma', newdata=newx, type='response', data=storedData)
  nu <- predict(storedModel, what='nu', newdata=newx, type='response', data=storedData)
  tau <- predict(storedModel, what='tau', newdata=newx, type='response', data=storedData)
  return(data.frame(
    a = seq(round(min(LOscore), 0), round(max(LOscore), 0), .01),
    b = dJSU(seq(round(min(LOscore), 0), round(max(LOscore), 0), .01), mu, sigma, nu, tau),
    abline = log(score/(1-score)))
  )
}

biLion <- function(ageDays,score){
  z <- c(0, 1)
  if (trunc(ageDays/365, 0)==6){z <- c(score-1.96*.0683, score+1.96*.0683)}
  if (trunc(ageDays/365, 0)==7){z <- c(score-1.96*.0636, score+1.96*.0636)}
  if (trunc(ageDays/365, 0)==8){z <- c(score-1.96*.0626, score+1.96*.0626)}
  if (trunc(ageDays/365, 0)==9){z <- c(score-1.96*.0561, score+1.96*.0561)}
  if (trunc(ageDays/365, 0)==10){z <- c(score-1.96*.0572, score+1.96*.0572)}
  if (trunc(ageDays/365, 0)==11){z <- c(score-1.96*.0536, score+1.96*.0536)}
  if (trunc(ageDays/365, 0)==12){z <- c(score-1.96*.0540, score+1.96*.0540)}
  if (z[1]<0){z[1]<-0}
  if (z[2]>1){z[2]<-1}
  round(z*100, 0)
}

biMonkey <- function(ageDays, score) {
  z <- c(0, 1)
  if (trunc(ageDays/365, 0)==6){z <- c(score-1.96*0.163*sqrt(1-.87), score+1.96*0.163*sqrt(1-.87))}
  if (trunc(ageDays/365, 0)==7){z <- c(score-1.96*0.154*sqrt(1-.86), score+1.96*0.154*sqrt(1-.86))}
  if (trunc(ageDays/365, 0)==8){z <- c(score-1.96*0.137*sqrt(1-.82), score+1.96*0.137*sqrt(1-.82))}
  if (trunc(ageDays/365, 0)==9){z <- c(score-1.96*0.134*sqrt(1-.82), score+1.96*0.134*sqrt(1-.82))}
  if (trunc(ageDays/365, 0)==10){z <- c(score-1.96*0.125*sqrt(1-.80), score+1.96*0.125*sqrt(1-.80))}
  if (trunc(ageDays/365, 0)==11){z <- c(score-1.96*0.128*sqrt(1-.81), score+1.96*0.128*sqrt(1-.81))}
  if (trunc(ageDays/365, 0)==12){z <- c(score-1.96*0.141*sqrt(1-.84), score+1.96*0.141*sqrt(1-.84))}
  if (z[1]<0){z[1]<-0}
  if (z[2]>1){z[2]<-1}
  round(z*100,0)
}

tscoreLion <- function(ageDays, score){
  # fetch saved model and data
  
  storedModel <- readRDS(file = paste(getwd(), "models/lionModel.rds", sep = '/'))
  storedData <- readRDS(file = paste(getwd(), "models/lionData.rds", sep = '/'))
  
  ts <- c(0, 0, 0)
  newx <- data.frame(lftd = ageDays)
  mu <- predict(storedModel, what = 'mu', newdata = newx, type = 'response', data = storedData)
  sigma <- predict(storedModel, what = 'sigma', newdata = newx, type = 'response', data = storedData)
  tr <- (score-mu)*10/sigma+50
  if (trunc(ageDays/365, 0)==6){ts <- c(tr, tr-1.96*10*sqrt(1-.86), tr+1.96*10*sqrt(1-.86))}
  if (trunc(ageDays/365, 0)==7){ts <- c(tr, tr-1.96*10*sqrt(1-.88), tr+1.96*10*sqrt(1-.88))}
  if (trunc(ageDays/365, 0)==8){ts <- c(tr, tr-1.96*10*sqrt(1-.87), tr+1.96*10*sqrt(1-.87))}
  if (trunc(ageDays/365, 0)==9){ts <- c(tr, tr-1.96*10*sqrt(1-.85), tr+1.96*10*sqrt(1-.85))}
  if (trunc(ageDays/365, 0)==10){ts <- c(tr, tr-1.96*10*sqrt(1-.84), tr+1.96*10*sqrt(1-.84))}
  if (trunc(ageDays/365, 0)==11){ts <- c(tr, tr-1.96*10*sqrt(1-.85), tr+1.96*10*sqrt(1-.85))}
  if (trunc(ageDays/365, 0)==12){ts <- c(tr, tr-1.96*10*sqrt(1-.86), tr+1.96*10*sqrt(1-.86))}
  round(ts,1)
}


tscoreMonkey <- function(ageDays, score){
  # fetch saved model and data
  storedModel <- readRDS(file = paste(getwd(), "models/monkeyModel.rds", sep = '/'))
  storedData <- readRDS(file = paste(getwd(), "models/monkeyData.rds", sep = '/'))
  
  ts <- c(0, 0, 0)
  newx<-data.frame(lftd = ageDays)
  mu<-predict(storedModel, what = 'mu', newdata = newx, type = 'response', data = storedData)
  sigma<-predict(storedModel, what = 'sigma', newdata = newx, type = 'response', data = storedData)
  tr<-(score-mu)*10/sigma+50
  if (trunc(ageDays/365,0)==6){ts<-c(tr,tr-1.96*10*sqrt(1-.87),tr+1.96*10*sqrt(1-.87))}
  if (trunc(ageDays/365,0)==7){ts<-c(tr,tr-1.96*10*sqrt(1-.86),tr+1.96*10*sqrt(1-.86))}
  if (trunc(ageDays/365,0)==8){ts<-c(tr,tr-1.96*10*sqrt(1-.82),tr+1.96*10*sqrt(1-.82))}
  if (trunc(ageDays/365,0)==9){ts<-c(tr,tr-1.96*10*sqrt(1-.82),tr+1.96*10*sqrt(1-.82))}
  if (trunc(ageDays/365,0)==10){ts<-c(tr,tr-1.96*10*sqrt(1-.80),tr+1.96*10*sqrt(1-.80))}
  if (trunc(ageDays/365,0)==11){ts<-c(tr,tr-1.96*10*sqrt(1-.81),tr+1.96*10*sqrt(1-.81))}
  if (trunc(ageDays/365,0)==12){ts<-c(tr,tr-1.96*10*sqrt(1-.84),tr+1.96*10*sqrt(1-.84))}
  round(ts,1)
}

