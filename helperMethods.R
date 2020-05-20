library(gamlss)
library(eeptools)

biList <- function(){
  lion        <- c(.0683, .0636, .0626, .0561, .0572, .0536, .0540)
  lionboys    <- c(.0676, .0660, .0632, .0593, .0571, .0554, .0543)
  liongirls   <- c(.0685, .0666, .0586, .0580, .0558, .0513, .0530)
  monkey      <- c(0.163*sqrt(1-.87), 0.154*sqrt(1-.86), 0.137*sqrt(1-.82), 
                   0.134*sqrt(1-.82), 0.125*sqrt(1-.80), 0.128*sqrt(1-.81), 0.141*sqrt(1-.84))
  monkeyboys  <- c(.0580, .0590, .0595, .0586, .0576, .0557, .0583)
  monkeygirls <- c(.0590, .0565, .0568, .0560, .0558, .0551, .0542)
  years <- c(6, 7, 8, 9, 10, 11, 12)
  
  return(data.frame(years, lion, lionboys, liongirls, monkey, monkeyboys, monkeygirls))
}

tscoresList <- function(){
  lion        <- c(.86, .88, .87, .85, .84, .85, .86)
  lionboys    <- c(.86, .87, .86, .86, .84, .86, .88)
  liongirls   <- c(.86, .88, .87, .83, .84, .83, .83)
  monkey      <- c(.87, .86, .82, .82, .80, .81, .84)
  monkeyboys  <- c(.88, .85, .81, .80, .79, .82, .83)
  monkeygirls <- c(.86, .89, .84, .81, .82, .81, .85)
  years <- c(6, 7, 8, 9, 10, 11, 12)
  
  return(data.frame(years, lion, lionboys, liongirls, monkey, monkeyboys, monkeygirls))
}

tasksArray<-function(){
  c('Leeuwenspel' = 1, 'Apenspel tekst' = 2, 'Apenspel plaatjes' = 3)
}

getStoredData <- function (task, gender) {
  if(task == '1') {
    if(gender == 'man'){
      storedData <- readRDS(file = paste(getwd(), "models/lionDataBoys.rds", sep = '/'))
    }
    if(gender == 'vrouw'){
      storedData <- readRDS(file = paste(getwd(), "models/lionDataGirls.rds", sep = '/'))
    }
    if(gender == 'na') {
      storedData <- readRDS(file = paste(getwd(), "models/lionData.rds", sep = '/'))
    }
  }
  
  if(task == '2' || task == '3') {    
    if(gender == 'man'){
      storedData <- readRDS(file = paste(getwd(), "models/monkeyDataBoys.rds", sep = '/'))
    }
    if(gender == 'vrouw'){
      storedData <- readRDS(file = paste(getwd(), "models/monkeyDataGirls.rds", sep = '/'))
    }
    if(gender == 'na') {
      storedData <- readRDS(file = paste(getwd(), "models/monkeyData.rds", sep = '/'))
    }
  }
  return(storedData)
}


getStoredModel <- function (task, gender) {
  if(task == '1') {
    if(gender == 'man'){
      storedModel <- readRDS(file = paste(getwd(), "models/lionModelBoys.rds", sep = '/'))
    }
    if(gender == 'vrouw'){
      storedModel <- readRDS(file = paste(getwd(), "models/lionModelGirls.rds", sep = '/'))
    }
    if(gender == 'na') {
      storedModel <- readRDS(file = paste(getwd(), "models/lionModel.rds", sep = '/'))
    }
  }
  
  if(task == '2' || task == '3') {    
    if(gender == 'man'){
      storedModel <- readRDS(file = paste(getwd(), "models/monkeyModelBoys.rds", sep = '/'))
    }
    if(gender == 'vrouw'){
      storedModel <- readRDS(file = paste(getwd(), "models/monkeyModelGirls.rds", sep = '/'))
    }
    if(gender == 'na') {
      storedModel <- readRDS(file = paste(getwd(), "models/monkeyModel.rds", sep = '/'))
    }
  }
  return(storedModel)
}

getColumnName <- function (task, gender) {
  if(task == '1') {
    if(gender == 'man'){
      return('lionboys')
    }
    if(gender == 'vrouw'){
      return('liongirls')
    }
    if(gender == 'na') {
      return('lion')
    }
  }
  
  if(task == '2' || task == '3') {    
    if(gender == 'man'){
      return('monkeyboys')
    }
    if(gender == 'vrouw'){
      return('monkeygirls')
    }
    if(gender == 'na') {
      return('monkey')
    }
  }
}

ageAtTestDay<-function(birthDate, testDate){
  as.integer(age_calc(birthDate, units='days')) - 
    as.integer(Sys.Date() - as.Date(testDate))
}

ageString<-function(birthDate, testDate){
  years <- as.integer(ageAtTestDay(birthDate, testDate)/365)
  months<-as.integer((ageAtTestDay(birthDate, testDate)- 365*years)/30)
  paste(toString(years), 'jr', toString(months), 'mnd')
}