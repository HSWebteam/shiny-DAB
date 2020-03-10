#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library("bitops")
library("devtools")
# install_github("HSWebteam/rDAB")
# library(rDAB, lib.loc = "/home/dasscheman/Surfdrive/Development/rdab")
library("rDAB")

library("knitr")
library("rjson")
library("jsonlite")
library("httr")
library("rmarkdown")

library("R.utils", quietly = TRUE); # function getAbsolutePath
library("tools", quietly = TRUE); # function file_path_sans_ext
library("gdata", quietly = TRUE);
library("dataframes2xls", quietly = TRUE);

source("dataPrep.R")
source("plots.R")

function(input, output, session) {
  fetchedData <- reactive({
    getData(session$clientData$url_search)
  })

  search <- reactive({
    session$clientData$url_search
  })
  
  filteredData <- function() {
    data = fetchedData()
    if(is.null(dim(data)) ||
       NROW(data) == 0) {
        return(FALSE)
    }
    data = data[data$taak == input$task, ]
    data = data[data$task_id == input$task_id, ]
    return(data[order(
      data$task_id,
      data$participant_id),])
  }
  
  output$tempText <- renderText({
    averageData <- getAverage(filteredData())
  })
  
  output$taskIdSelector <- renderUI({
    if(is.recursive(fetchedData())) {
      data = fetchedData()
      data = data[data$taak == input$task, ]
      if(NROW(data) > 0 ) {
        if(length(unique(data$task_id)) > 1 ) {
          radioButtons("task_id", "selecteer taak id:", unique(data$task_id)) 
        }
      }
    }
  })
  
  output$taskSelector <- renderUI({
    radioButtons(
      "task", 
      "selecteer taak:", 
      tasksArray())
  })
  
  output$subtitle <- renderText({
    if(is.null(dim(filteredData())) ||
       NROW(filteredData()) == 0) {
      return()
    }
    groupname <- unique(filteredData()$group_name)
    taskname <- names(tasksArray()[as.numeric(input$task)])
    title <- paste(groupname, taskname, sep = '-')
    paste("<font color=\"#000000\"><h3>", title, "</h3></font>", sep = '')
  })
  
  output$warning <- renderText({
    if(is.null(dim(filteredData())) ||
       NROW(filteredData()) == 0) {
      return()
    }
    status <- subset(filterTaskStatus(), task_id == unique(filteredData()$task_id))
    if(status$finished != 1) {
      "<font color=\"#FF0000\"><b>Let op! participant heeft deze taak nog niet afgerond</b></font>"
    }
  })
  
  output$tableTestInfo <- renderTable({
    if(is.null(dim(filteredData())) ||
       NROW(filteredData()) == 0) {
      return('Geen data gevonden.')
    }
    testData(filteredData(), input$date)
  })
  
  
  output$tableScore <- renderTable({
    if(is.null(dim(filteredData())) ||
       NROW(filteredData()) == 0) {
      return('Geen data gevonden.')
    }
  
    averageData <- getAverage(filteredData())
    analizeData(filteredData(), input$date, input$task)
  }, width = "100%")
    
  output$plotPercentiel <- renderPlot({
    if(is.null(dim(filteredData())) ||
      NROW(filteredData()) == 0) {
        return('Geen data gevonden.')
    }
    if(input$task == '1') {
      storedData <- readRDS("models/lionData.rds")
    }
    if(input$task == '2' || input$task == '3') {
      storedData <- readRDS("models/monkeyData.rds")
    }
    averageData <- getAverage(filteredData())
    testDate = min(filteredData()$created_at)
    percetielPlot(storedData, percentielScoreAge(input$date, testDate, averageData$meanprop))
  })
    
  
  output$plotGrowth <- renderPlot({
    if(is.null(dim(filteredData())) ||
       NROW(filteredData()) == 0) {
      return('Geen data gevonden.')
    }
    if(input$task == '1') {
      storedData <- readRDS("models/lionData.rds")
      storedModel <- readRDS("models/lionModel.rds")
    }
    if(input$task == '2' || input$task == '3') {
      storedData <- readRDS("models/monkeyData.rds")
      storedModel <- readRDS("models/monkeyModel.rds")
    }
    averageData <- getAverage(filteredData())
    testDate = min(filteredData()$created_at)
    ageDays = ageAtTestDay(input$date, testDate)
    
    growthPlot(storedModel, storedData, ageDays, averageData$meanprop)
    # abline(v=percentielScoreAge(input$date, testDate, averageData$meanprop)/100,col="red")
  })
  
  output$plotDensity <- renderPlot({
    if(is.null(dim(filteredData())) ||
       NROW(filteredData()) == 0 ) {
        return('Geen data gevonden.')
    }
    if(input$task == '1') {
      storedData <- readRDS("models/lionData.rds")
    }
    if(input$task == '2' || input$task == '3') {
      storedData <- readRDS("models/monkeyData.rds")
    }
    averageData <- getAverage(filteredData())
    testDate = min(filteredData()$created_at)
    
    ageDays = ageAtTestDay(input$date, testDate)
    data = density(ageDays, averageData$meanprop)
    densityScoreAgePlot(data)
  })
  

  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    if(is.null(dim(filteredData())) ||
       NROW(filteredData()) == 0) {
      return(data.frame(c('error'), c('Geen data gevonden.')))
    }
    filteredData()
  }))
  
  output$urlText <- renderText({
    if(is.null(dim(filteredData())) ||
       NROW(filteredData()) == 0) {
      return('Geen data gevonden.')
    }
    averageData <- getAverage(filteredData())
    paste(sep = "",
      "protocol: ", session$clientData$url_protocol, "\n",
      "hostname: ", session$clientData$url_hostname, "\n",
      "pathname: ", session$clientData$url_pathname, "\n",
      "port: ",     session$clientData$url_port,     "\n",
      "search: ",   session$clientData$url_search,   "\n",
      "test: ", averageData$meanprop
    ) 
  })

  output$plot1 <- renderPlot({
    if(is.empty(filteredData()) ||
       NROW(filteredData()) == 0) {
      return('Geen data gevonden.')
    }
    
    storedData <- readRDS("models/lionData.rds")
    averageData <- getAverage(filteredData())
    testDate = min(filteredData()$created_at)
    hist(as.numeric(storedData[,2]))
    abline(v=percentielScoreAge(input$date, testDate, averageData$meanprop),col="red")
  })
  output$report <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html'
      ))
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      averageData <- getAverage(filteredData())
      testDate = min(filteredData()$created_at)
      
      if(input$task == '1') {
        storedModel <- readRDS("models/lionModel.rds")
        storedData <- readRDS("models/lionData.rds")
      }
      if(input$task == '2' || input$task == '3') {
        storedModel <- readRDS("models/monkeyModel.rds")
        storedData <- readRDS("models/monkeyData.rds")
      }
      ageDays = ageAtTestDay(input$date, testDate)
      # Set up parameters to pass to Rmd document
      params <- list(
        path = getwd(),
        storedModel = storedModel,
        storedData = storedData,
        ageDays = ageDays,
        meanproportion = averageData$meanprop,
        percentielScore = percentielScoreAge(
          input$date, 
          testDate, 
          averageData$meanprop, 
          input$task),
        participantData = participantData(
          caseNumber, 
          input$gender, 
          input$date,
          unique(filteredData()$participant_name)),
        testData = testData(filteredData(), input$date),
        analizeData = analizeData(filteredData(), input$date, input$task),
        densityData = density(ageDays, averageData$meanprop)
      )
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      library(rmarkdown)
      out <- render(
        tempReport, 
        switch(
          input$format,
          PDF = pdf_document(), HTML = html_document()
        ),
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv()),
        quiet = FALSE

      )
      
      file.rename(out, file)
    }
  )
}