source("DABmodel.R")
library("eeptools")
library("gamlss")

getData<-function(search){
  data
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
  
  data <- jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = TRUE)
  
  col_headings <- c("project_name", "organisation_name", "group_name", "participant_case_number",
                    "participant_symbol", "participant_color", "participant_token", "taak", "#levels", "#items",
                    "language", "theme", "matrix_size", "id", "task_id", "participant_id", "level", "item",
                    "question_number", "correct_answer", "correct_matrix_number", "response",
                    "response_matrix_number", "response_time", "score", "created_at", "updated_at", "deleted_at")

  if(!is.atomic(list(data["data"]))) {
    return  
  } 
  
  data<-data.frame(
    matrix(
      unlist(
        data["data"], 
        use.names = TRUE), 
      ncol = length(col_headings), 
      byrow = FALSE),
    check.rows = TRUE, 
    stringsAsFactors = FALSE)
  names(data) <- col_headings

  return(data[order(
    data$task_id,
    data$participant_id),])
}

ageAtTestDay<-function(birthDate, testDate){
  as.integer(age_calc(birthDate, units='days')) - 
    as.integer(Sys.Date() - as.Date(testDate))
}

ageString<-function(birthDate, testDate){
  years <- as.integer(ageAtTestDay(birthDate, testDate)/365)
  months<-as.integer((ageAtTestDay(birthDate, testDate)- 365*years)/30)
  days<-as.integer(ageAtTestDay(birthDate, testDate)- 365*years - 30*months)
  paste(toString(years), 'jr', toString(months), 'mnd', toString(days), 'dgn')
}

percentielScoreAge<-function(birthDate, testDate, meanprop, task){
  if(is.null(meanprop)) {
    return(FALSE)
  }
  pcs(ageAtTestDay(birthDate, testDate), meanprop, task)
}

confidenceInterval <- function(birthDate, testDate, meanprop, task){
  if(is.null(meanprop)) {
    return(FALSE)
  }
  
  if(task == '1') {
    return(biLion(ageAtTestDay(birthDate, testDate), meanprop))
  }
  if(task == '2' || task == '3') {
    return(biMonkey(ageAtTestDay(birthDate, testDate), meanprop))
  }
}

tScore <- function(birthDate, testDate, meanprop, task){
  if(is.null(meanprop)) {
    return(FALSE)
  }
  if(task == '1') {
    return(tscoreLion(ageAtTestDay(birthDate, testDate), meanprop))
  }
  if(task == '2' || task == '3') {
    return(tscoreMonkey(ageAtTestDay(birthDate, testDate), meanprop))
  }
}

participantData <- function(gender, date){
  return(
    data.frame(
      'Dossiernummer' = NA, 
      'Geslacht' = gender, 
      'Geboortedatum' = as.character(date, "%Y-%m-%d"),
      'Naam' = NA,
      check.names = FALSE
    )
  )
}

testData <- function(fetchedData, date) {
    testDate = min(fetchedData$created_at)
    return(
        data.frame(
            'Test datum' = as.character(testDate, "%Y-%m-%d"), 
            'Leeftijd op testdatum' = ageString(date, testDate),
            check.names = FALSE
        )
    )
}

analizeData <- function(filteredData, date, task) {
  averageData <- getAverage(filteredData)
  averagePracticeData <- getAveragePractice(filteredData)
  testDate = min(filteredData$created_at)
  confidence = confidenceInterval(date, testDate, averageData$meanprop, task)
  tscore = tScore(date, testDate, averageData$meanprop, task)
  percentiel = percentielScoreAge(date, testDate, averageData$meanprop, task)
  description <- data.frame(
    min = c(0,5,10,25,75,90,95), 
    max = c(5,10,25,75,90,95,100), 
    description = c('Zeer laag', 'Laag', 'Beneden gemiddeld',
      'Gemiddeld', 'Boven gemiddeld', 'Hoog','Zeer hoog'))
  
  description = description[description$min < percentiel, ]
  description = description[description$max > percentiel, ]

  return(data.frame(
    'Oefen score' = averagePracticeData$meanprop_practice * 100,	
    'Ruwe score' = averageData$meanprop * 100,
    'Bi' = paste(confidence, collapse=' - '),
    'Percentiel score' = percentiel,
    'T-score' = tscore[1],
    'T-score interval' = paste(tscore[2],tscore[3], sep=' - '),
    'kwalitatieve beschrijving' = description$description, check.names=FALSE)
  )
}
