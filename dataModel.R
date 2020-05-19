source("helperMethods.R")
library("eeptools")
library("gamlss")

getData<-function(search){
  query <- parseQueryString(search)
  if(length(query) == 0) {
    return(NA)
  }
  if(query$returnurl == 'https://taken.vagrant') {
    httr::set_config(config(ssl_verifypeer = 0L))
  }
  resp<-GET(query$returnurl, path = 'api/results', query = list(token = query$token))
  # TODO check status: data$status_code
  if (resp$status_code != "200") {
    stop("API did not returned error", call. = FALSE)
  }
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  responseData <- jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = TRUE)

  col_headings <- c("project_name", "organisation_name", "group_name", "participant_case_number", 
                    "participant_name", "participant_symbol", "participant_color", 
                    "participant_token", "taak", "#levels", "#items", "language", "theme", 
                    "matrix_size", "id", "task_id", "participant_id", "level", "item",
                    "question_number", "correct_answer", "correct_matrix_number", 
                    "response", "response_matrix_number", "response_time",
                    "score", "created_at", "updated_at", "deleted_at")

  if(!is.atomic(list(responseData['results']))) {
    return  
  } 
  
  data<-data.frame(
    matrix(
      unlist(
        responseData['results'], 
        use.names = TRUE), 
      ncol = length(col_headings), 
      byrow = FALSE),
    check.rows = TRUE, 
    stringsAsFactors = FALSE)
  names(data) <- col_headings
  return(data)
}


getTaskStatus<-function(search){
  query <- parseQueryString(search)
  if(length(query) == 0) {
    return(NA)
  }
  if(query$returnurl == 'https://taken.vagrant') {
    httr::set_config(config(ssl_verifypeer = 0L))
  }
  resp<-GET(query$returnurl, path = 'api/task', query = list(token = query$token))

  if (resp$status_code != "200") {
    stop("API did not returned error", call. = FALSE)
  }
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  responseData <- jsonlite::fromJSON(
    content(resp, "text", encoding = "UTF-8"), 
    simplifyVector = TRUE
  )
  
  col_headings <- c("task_id", "finished")
  if(!is.atomic(list(responseData['finised']))) {
    return  
  } 
  data<-data.frame(
    matrix(
      unlist(
        responseData['finised'], 
        use.names = TRUE), 
      ncol = length(col_headings),
      byrow = FALSE),
    check.rows = TRUE,
    stringsAsFactors = FALSE)
    
  names(data) <- col_headings
  return(data)
}

percentielScoreAge<-function(birthDate, testDate, meanprop, task, gender){
  if(is.null(meanprop)) {
    return(FALSE)
  }
  ageDays = ageAtTestDay(birthDate, testDate)
  
  # fetch saved model and data
  storedData = getStoredData(task, gender)
  storedModel = getStoredModel(task, gender)
  
  newx <- data.frame(lftd=ageDays)
  mu <- predict(storedModel, what='mu', newdata=newx, type='response', data=storedData)
  sigma <- predict(storedModel, what='sigma', newdata=newx, type='response', data=storedData)
  nu <- predict(storedModel, what='nu', newdata=newx, type='response', data=storedData)
  tau <- predict(storedModel, what='tau', newdata=newx, type='response', data=storedData)
  return(pJSU(log(meanprop/(1-meanprop)), mu, sigma, nu, tau)*100)
}

confidenceInterval <- function(birthDate, testDate, meanprop, task, gender){
  if(is.null(meanprop)) {
    return(FALSE)
  }
  ageDays = ageAtTestDay(birthDate, testDate)
  biList = biList()
  column = getColumnName(task, gender)
  z <- c(0, 1)
  
  betrouwInterval = biList[biList$years == trunc(ageDays/365, 0), ]
  if(length(betrouwInterval[,column]) == 1){
    z <- c(meanprop-1.96*betrouwInterval[,column], 
           meanprop+1.96*betrouwInterval[,column])
  }
  if (z[1]<0){z[1]<-0}
  if (z[2]>1){z[2]<-1}
  return(z*100)
}

tScore <- function(birthDate, testDate, meanprop, task, gender){
  if(is.null(meanprop)) {
    return(FALSE)
  }
  ageDays = ageAtTestDay(birthDate, testDate)
  tscoresList = tscoresList()
  # fetch saved model and data
  storedData = getStoredData(task, gender)
  storedModel = getStoredModel(task, gender)
  column = getColumnName(task, gender)
  
  ts <- c(0, 0, 0)
  newx <- data.frame(lftd = ageDays)
  mu <- predict(storedModel, what = 'mu', newdata = newx, type = 'response', data = storedData)
  sigma <- predict(storedModel, what = 'sigma', newdata = newx, type = 'response', data = storedData)
  nu<-predict(storedModel, what='nu', newdata=newx, type = 'response',data = storedData)
  tau<-predict(storedModel, what='tau', newdata=newx, type = 'response',data = storedData)
  
  # from standard normal to T with mean 50 and sd 10     
  tr <- qnorm(pJSU(log(meanprop/(1-meanprop)), mu, sigma, nu, tau))*10+50 
 
  tscoreInterval = tscoresList[tscoresList$years == trunc(ageDays/365, 0), ]
  if(length(tscoreInterval[,column]) == 1){
    ts <- c(
      tr, 
      tr-1.96*10*sqrt(1-tscoreInterval[,column]), 
      tr+1.96*10*sqrt(1-tscoreInterval[,column]))
  }
  return(ts)
}

getJSUDistribution()

disc<-function(birthDate, testDate, meanLion, meanMonkey, percentage){
  # 'percentage' is het nominale significantieniveau in procenten
  ageDays = ageAtTestDay(birthDate, testDate)
  
  
  
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
  return(c(round(tr1-tr2,1),round(cr,1)))}


density <- function(ageDays, score, task, gender){
  # fetch saved model and data
  storedData = getStoredData(task, gender)
  storedModel = getStoredModel(task, gender)
  
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

participantData <- function(casenumber, gender, date, naam){
  return(
    data.frame(
      'Casenumber' = toString(casenumber), 
      'Geslacht' = gender, 
      'Geboortedatum' = as.character(date, "%Y-%m-%d"),
      'Naam' = toString(naam),
      check.names = FALSE
    )
  )
}

taskData <- function(fetchedData, date) {
    testDate = min(fetchedData$created_at)
    return(
        data.frame(
            'Test datum' = as.character(testDate, "%Y-%m-%d"), 
            'Leeftijd op testdatum' = ageString(date, testDate),
            check.names = FALSE
        )
    )
}

analizeData <- function(filteredData, date, task, gender, digets = 0) {
  averageData <- getAverage(filteredData)
  practiceScore <- getPracticeScore(filteredData)
  averagePracticeData <- getAveragePractice(filteredData)
  testDate = min(filteredData$created_at)
  confidence = confidenceInterval(date, testDate, averageData$meanprop, task, gender)
  tscore = tScore(date, testDate, averageData$meanprop, task, gender)
  percentiel = percentielScoreAge(date, testDate, averageData$meanprop, task, gender)
  description <- data.frame(
    min = c(0,5,10,25,75,90,95), 
    max = c(5,10,25,75,90,95,100), 
    description = c('Zeer laag', 'Laag', 'Beneden gemiddeld',
      'Gemiddeld', 'Boven gemiddeld', 'Hoog','Zeer hoog'))
  
  description = description[description$min < percentiel, ]
  description = description[description$max >= percentiel, ]

  return(data.frame(
    'Oefen score' = as.character(round(practiceScore$practice_items_correct, digets)),
    'Ruwe score' = as.character(round(averageData$meanprop * 100, digets)),
    'BI 95%' = paste(as.character(round(confidence, digets)), collapse='-'),
    'Percentiel score' = as.character(round(percentiel, digets)),
    'T-score' = as.character(round(tscore[1], digets)),
    'T-score BI 95%' = paste(
        as.character(round(tscore[2], digets)), 
        as.character(round(tscore[3], digets)), sep='-'),
    'kwalitatieve beschrijving' = description$description, 
    check.names=FALSE
    )
  )
}

analizeDataManual <- function(meanprop, testDate, birthDate, task, gender, digets = 0) {
  confidence = confidenceInterval(birthDate, testDate, meanprop, task, gender)
  tscore = tScore(birthDate, testDate, meanprop, task, gender)
  percentiel = percentielScoreAge(birthDate, testDate, meanprop, task, gender)
  description <- data.frame(
    min = c(0,5,10,25,75,90,95), 
    max = c(5,10,25,75,90,95,100), 
    description = c('Zeer laag', 'Laag', 'Beneden gemiddeld',
                    'Gemiddeld', 'Boven gemiddeld', 'Hoog','Zeer hoog'))
  
  description = description[description$min < percentiel, ]
  description = description[description$max >= percentiel, ]
  if(length(tscore) == 1) {
    # This is a strange rShinny thing, this method is passed twice. The first time 
    # without values. This gives an ugly "arguments imply differing number of rows: 0, 1"
    # warning.
    # The second time the tscore has lenght 3, then the data.frame is filled and returned.
    return()
  }
  return(data.frame(
    'Ruwe score' = as.character(round(meanprop * 100, digets)),
    'BI 95%' = paste(as.character(round(confidence, digets)), collapse='-'),
    'Percentiel score' = as.character(round(percentiel, digets)),
    'T-score' = as.character(round(tscore[1], digets)),
    'T-score BI 95%' = paste(
      as.character(round(tscore[2], digets)),
      as.character(round(tscore[3], digets)), sep='-'),
    'kwalitatieve beschrijving' = description$description, 
    check.names=FALSE
  ))
}