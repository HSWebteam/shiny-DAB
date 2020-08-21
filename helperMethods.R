library(gamlss)
library(eeptools)

biList <- function(){
  lion        <- c(.0683, .0636, .0626, .0561, .0572, .0536, .0540)
  lionboys    <- c(.0676, .0660, .0632, .0593, .0571, .0554, .0543)
  liongirls   <- c(.0685, .0666, .0586, .0580, .0558, .0513, .0530)
  monkey      <- c(0.163*sqrt(1 - .87), 0.154*sqrt(1 - .86), 0.137*sqrt(1 - .82),
                   0.134*sqrt(1 - .82), 0.125*sqrt(1 - .80), 0.128*sqrt(1 - .81), 0.141*sqrt(1 - .84))
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

tasksArray <- function(){
  c('Leeuwenspel' = 1, 'Apenspel tekst' = 2, 'Apenspel plaatjes' = 3)
}

getStoredData <- function(task, gender) {
  if (task == '1') {
    if (gender == 'man') {
      storedData <- readRDS(file = paste(getwd(), "models/lionDataBoys.rds", sep = '/'))
    }
    if (gender == 'vrouw') {
      storedData <- readRDS(file = paste(getwd(), "models/lionDataGirls.rds", sep = '/'))
    }
    if (gender == 'nvt') {
      storedData <- readRDS(file = paste(getwd(), "models/lionData.rds", sep = '/'))
    }
  }

  if (task == '2' || task == '3') {
    if (gender == 'man') {
      storedData <- readRDS(file = paste(getwd(), "models/monkeyDataBoys.rds", sep = '/'))
    }
    if (gender == 'vrouw') {
      storedData <- readRDS(file = paste(getwd(), "models/monkeyDataGirls.rds", sep = '/'))
    }
    if (gender == 'nvt') {
      storedData <- readRDS(file = paste(getwd(), "models/monkeyData.rds", sep = '/'))
    }
  }
  if (task == '4') {
    if (gender == 'man') {
      storedData <- readRDS(file = paste(getwd(), "models/discrepantieDataBoys.rds", sep = '/'))
    }
    if (gender == 'vrouw') {
      storedData <- readRDS(file = paste(getwd(), "models/discrepantieDataGirls.rds", sep = '/'))
    }
    if (gender == 'nvt') {
      storedData <- readRDS(file = paste(getwd(), "models/discrepantiePercentageData.rds", sep = '/'))
    }
  }
  return(storedData)
}

getStoredModel <- function(task, gender) {
  if (task == '1') {
    if (gender == 'man') {
      storedModel <- readRDS(file = paste(getwd(), "models/lionModelBoys.rds", sep = '/'))
    }
    if (gender == 'vrouw') {
      storedModel <- readRDS(file = paste(getwd(), "models/lionModelGirls.rds", sep = '/'))
    }
    if (gender == 'nvt') {
      storedModel <- readRDS(file = paste(getwd(), "models/lionModel.rds", sep = '/'))
    }
  }

  if (task == '2' || task == '3') {
    if (gender == 'man') {
      storedModel <- readRDS(file = paste(getwd(), "models/monkeyModelBoys.rds", sep = '/'))
    }
    if (gender == 'vrouw') {
      storedModel <- readRDS(file = paste(getwd(), "models/monkeyModelGirls.rds", sep = '/'))
    }
    if (gender == 'nvt') {
      storedModel <- readRDS(file = paste(getwd(), "models/monkeyModel.rds", sep = '/'))
    }
  }
  if (task == '4') {
    if (gender == 'man') {
      storedModel <- readRDS(file = paste(getwd(), "models/discrepantieModelBoys.rds", sep = '/'))
    }
    if (gender == 'vrouw') {
      storedModel <- readRDS(file = paste(getwd(), "models/discrepantieModelGirls.rds", sep = '/'))
    }
    if (gender == 'nvt') {
      storedModel <- readRDS(file = paste(getwd(), "models/discrepantiePercentage.rds", sep = '/'))
    }
  }
  return(storedModel)
}

getColumnName <- function(task, gender) {
  if (task == '1') {
    if (gender == 'man') {
      return('lionboys')
    }
    if (gender == 'vrouw') {
      return('liongirls')
    }
    if (gender == 'nvt') {
      return('lion')
    }
  }

  if (task == '2' || task == '3') {
    if (gender == 'man') {
      return('monkeyboys')
    }
    if (gender == 'vrouw') {
      return('monkeygirls')
    }
    if (gender == 'nvt') {
      return('monkey')
    }
  }
}

ageAtTestDay <- function(birthDate, testDate){
  as.integer(age_calc(birthDate, units = 'days')) -
    as.integer(Sys.Date() - as.Date(testDate))
}

ageString <- function(birthDate, testDate){
  years <- as.integer(ageAtTestDay(birthDate, testDate)/365)
  months <- as.integer((ageAtTestDay(birthDate, testDate) - 365*years)/30)
  paste(toString(years), 'jr', toString(months), 'mnd')
}


listNormData <- function(filteredData, task, taskId, gender, date) {
  groupname <- unique(filteredData$group_name)
  taskname <- names(tasksArray()[as.numeric(task)])
  caseNumber <- unique(filteredData$participant_case_number)

  storedModel = getStoredModel(task, gender)
  storedData = getStoredData(task, gender)

  averageData <- getAverage(filteredData)
  testDate = min(filteredData$created_at)
  ageDays = ageAtTestDay(date, testDate)

  return(list(
    groupname = groupname,
    taskname = taskname,
    path = getwd(),
    storedModel = storedModel,
    storedData = storedData,
    ageDays = ageDays,
    meanproportion = averageData$meanprop,
    participantData = participantData(
      caseNumber,
      gender,
      date,
      unique(filteredData$participant_name)),
    taskData = taskData(filteredData, date),
    analizeData = analizeData(
      filteredData,
      date,
      task,
      gender)
  ))
}

listCounterpartData <- function(filteredData, filteredDataCounterpart, task, taskCounterpart, taskId, tasIdCounterpart, gender, date) {
  groupname <- unique(filteredData$group_name)
  caseNumber <- unique(filteredData$participant_case_number)

  averageData <- getAverage(filteredData)
  averageDataCounterpart <- getAverage(filteredDataCounterpart)
  testDateLion = min(filteredData$created_at)
  testDateMonkey = min(filteredDataCounterpart$created_at)
  ageDays = ageAtTestDay(date, testDateLion)

  return(list(
    groupname = groupname,
    taskname = names(tasksArray()[as.numeric(task)]),
    tasknameCounterpart = names(tasksArray()[as.numeric(taskCounterpart)]),
    path = getwd(),
    storedModel = getStoredModel(task, gender),
    storedModelCounterpart = getStoredModel(taskCounterpart, gender),
    storedData = getStoredData(task, gender),
    storedDataCounterpart = getStoredData(taskCounterpart, gender),
    discrepantie = discrepantieAnalyze(date, testDateLion, testDateMonkey, averageData$meanprop, averageDataCounterpart$meanprop, gender),
    ageDays = ageDays,
    meanproportion = averageData$meanprop,
    meanproportionCounterpart = averageDataCounterpart$meanprop,
    participantData = participantData(
      caseNumber,
      gender,
      date,
      unique(filteredData$participant_name)),
    taskData = taskData(filteredData, date),
    taskDataCounterpart = taskData(filteredDataCounterpart, date),
    analizeData = analizeData(
      filteredData,
      date,
      task,
      gender),
    analizeDataCounterpart = analizeData(
      filteredDataCounterpart,
      date,
      taskCounterpart,
      gender)
  ))
}
