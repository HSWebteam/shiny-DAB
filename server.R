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
install_github("HSWebteam/rDAB")
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

source("dataModel.R")
source("helperMethods.R")
source("plots.R")

function(input, output, session) {
  fetchedData <- reactive({
    getData(session$clientData$url_search)
  })

  fetchedTaskStatus <- reactive({
    getTaskStatus(session$clientData$url_search)
  })

  search <- reactive({
    session$clientData$url_search
  })

  digits <- reactive({
    # This function is used to detemine de number of digets 
    # the anaylises should have. On production it should be 0.
    query <- parseQueryString(session$clientData$url_search)
    if(query$returnurl != 'https://dab.app.uu.nl') {
      return(5)
    }
    return(0)
  })

  filteredData <- function(task, taskId) {
    # variable task id is only used when for a task multiple resutls exists.
    data = fetchedData()
    if(is.null(dim(data)) ||
       NROW(data) == 0) {
        return(FALSE)
    }
    if(length(task) == 0) {
      return(FALSE)
    }
    
    if(task == '1') {
      #Lion game
      data = data[data$taak == '1', ]
    }

    if(task == '2') {
      # Monkey game
      data = data[data$taak == '2', ]
      data = data[data$theme == 'text_theme', ]
    }

    if(task == '3') {
      # monkey game
      data = data[data$taak == '2', ]
      data = data[data$theme == 'picture_theme', ]
    }

    if(length(unique(data$task_id)) > 1 ) {
      # for the taak/theme combination multiple task_id's exists.
      # therefore use the task_id selector.
      if(length(taskId) == 0) {
        return(FALSE)
      }
      data = data[data$task_id == taskId, ]
    }
    
    return(data[order(
      data$task_id,
      data$participant_id),])
  }

  filterTaskStatus <- function(){
    data = fetchedTaskStatus()
    if(is.null(dim(data)) ||
       NROW(data) == 0) {
      return(FALSE)
    }
    if(length(input$task) == 0) {
      return(FALSE)
    }

    return(data)
  }
  
  output$analyzeSelector <- renderUI({
    radioButtons(
      "analyze_type", 
      "selecteer Analyze:", 
      list("Norm analyze" = "norm", "Discrepantie analyze" = "discrepantie"),
      "norm"
    )
  })
  
  output$taskSelector <- renderUI({
    tasks <- tasksArray()
    if(length(input$analyze_type) != 0 &&
       input$analyze_type == 'discrepantie') {
      tasks = tasks[1]
    }
    radioButtons(
      "task",
      "selecteer taak:",
      tasks)
  })
  
  output$taskSelectorDiscrepantie <- renderUI({
    if(length(input$analyze_type) != 0 &&
       input$analyze_type == 'discrepantie') {
      tasks <- tasksArray()
      radioButtons(
        "task_discrepantie",
        "selecteer taak:",
        tasks[2:3])
    }
  })
  
  output$taskIdSelector <- renderUI({
    if(is.recursive(fetchedData())) {
      data = fetchedData()
      data = data[data$taak == input$task, ]
      # when for given task multiple id's exist, display id selector.
      if(NROW(data) > 0 ) {
        if(length(unique(data$task_id)) > 1 ) {
          radioButtons("task_id", "selecteer taak id:", unique(data$task_id))
        }
      }
    }
  })
  
  output$taskIdSelectorDiscrepantie <- renderUI({
    if(length(input$analyze_type) != 0 &&
       input$analyze_type == 'discrepantie') {
      if(is.recursive(fetchedData())) {
        data = fetchedData()
        data = data[data$taak == input$task_discrepantie, ]
        # when for given task_discrepantie multiple id's exist, display id selector.
        if(NROW(data) > 0 ) {
          if(length(unique(data$task_id)) > 1 ) {
            radioButtons("task_id_discrepantie", "selecteer taak id:", unique(data$task_id))
          }
        }
      }
    }
  })
  
  output$average <- renderUI({
    # This is only used when no data is fetched from the dap.app.
    # Mostly when this shiny app is used as standalone.
    if(!is.recursive(fetchedData())) {
      sliderInput("meanprop", "proportie correct", .01, 1, .5, step = .005)
    }
  })

  output$averageDiscrepantie <- renderUI({
    # This is only used when no data is fetched from the dap.app.
    # Mostly when this shiny app is used as standalone.
    if(length(input$analyze_type) != 0 &&
       input$analyze_type == 'discrepantie') {
      if(!is.recursive(fetchedData())) {
        sliderInput("meanprop_discrepantie", "proportie correct", .01, 1, .5, step = .005)
      }
    }
  })
  
  output$testdate <- renderUI({
    # This is only used when no data is fetched from the dap.app.
    # Mostly when this shiny app is used as standalone.
    if(!is.recursive(fetchedData())) {
      dateInput('testdate',
                label = 'TestDate: jjjj-mm-dd',
                value = Sys.Date() - years(6)
      )
    }
  })
  
  output$testdateDiscrepantie <- renderUI({
    # This is only used when no data is fetched from the dap.app.
    # Mostly when this shiny app is used as standalone.
    if(length(input$analyze_type) != 0 &&
       input$analyze_type == 'discrepantie') {
      if(!is.recursive(fetchedData())) {
        dateInput(
          'testdate_discrepantie',
          label = 'TestDate: jjjj-mm-dd',
          value = Sys.Date() - years(6)
        )
      }
    }
  })
  
  output$warning <- renderText({
    if(is.null(dim(filteredData(input$task, input$task_id))) ||
       NROW(filteredData(input$task, input$task_id)) == 0) {
      return()
    }
    status <- subset(filterTaskStatus(), task_id == unique(filteredData(input$task, input$task_id)$task_id))
    if(status$finished != 1) {
      "<font color=\"#FF0000\"><b>Let op! participant heeft deze taak nog niet afgerond</b></font>"
    }
    
    if(is.null(dim(filteredData(input$task_discrepantie, input$task_id_discrepantie))) ||
       NROW(filteredData(input$task_discrepantie, input$task_id_discrepantie)) == 0) {
      return()
    }
    
    statusDiscrepantie <- subset(
        filterTaskStatus(), 
        task_id == unique(filteredData(input$task_discrepantie, input$task_id_discrepantie)$task_id))
    if(statusDiscrepantie$finished != 1) {
      "<font color=\"#FF0000\"><b>Let op! participant heeft deze taak nog niet afgerond</b></font>"
    }
  })
  
  output$tableGeneral <- renderTable({
    if(is.null(dim(filteredData(input$task, input$task_id))) ||
       NROW(filteredData(input$task, input$task_id)) == 0) {
      return('Geen data gevonden.')
    }
    
    participantData(
      unique(filteredData(input$task, input$task_id)$participant_case_number),
      input$gender,
      as.character(input$date, "%Y-%m-%d"),
      unique(filteredData(input$task, input$task_id)$participant_name)
    )
  })
  
  output$tableDiscrepantieAnalyse <- renderTable({
    if(length(input$analyze_type) == 0 ||
       input$analyze_type == 'norm') {
      return()
    }
    
    
  })
  
  output$subtitle <- renderText({
    if(is.null(dim(filteredData(input$task, input$task_id))) ||
       NROW(filteredData(input$task, input$task_id)) == 0) {
      return()
    }
    groupname <- unique(filteredData(input$task, input$task_id)$group_name)
    taskname <- names(tasksArray()[as.numeric(input$task)])
    title <- paste(groupname, taskname, sep = '-')
    paste("<font color=\"#000000\"><h3>", title, "</h3></font>", sep = '')
  })

 
  output$tableTestInfo <- renderTable({
    if(is.null(dim(filteredData(input$task, input$task_id))) ||
       NROW(filteredData(input$task, input$task_id)) == 0) {
      return('Geen data gevonden.')
    }
    taskData(filteredData(input$task, input$task_id), input$date)
  })
  
  output$tableScore <- renderTable({
    if(is.null(dim(filteredData(input$task, input$task_id))) ||
       NROW(filteredData(input$task, input$task_id)) == 0) {
      return(analizeDataManual(
        input$meanprop,
        input$testdate,
        input$date,
        input$task,
        input$gender,
        4))
    }
    
    analizeData(filteredData(input$task, input$task_id), input$date, input$task, input$gender, digits())
  }, width = "100%", bordered = FALSE, align = 'c', digits = 4)
  
  output$plotGrowth <- renderPlot({
    if(is.null(dim(filteredData(input$task, input$task_id))) ||
       NROW(filteredData(input$task, input$task_id)) == 0) {
      return('Geen data gevonden.')
    }
    storedData = getStoredData(input$task, input$gender)
    storedModel = getStoredModel(input$task, input$gender)
    
    averageData <- getAverage(filteredData(input$task, input$task_id))
    testDate = min(filteredData(input$task, input$task_id)$created_at)
    ageDays = ageAtTestDay(input$date, testDate)
    
    growthPlot(storedModel, storedData, ageDays, averageData$meanprop)
  })
  
  output$subtitleDiscrepantie <- renderText({
    if(length(input$analyze_type) == 0 ||
       input$analyze_type == 'norm') {
      return()
    }
    if(is.null(dim(filteredData(input$task_discrepantie, input$task_id_discrepantie))) ||
       NROW(filteredData(input$task_discrepantie, input$task_id_discrepantie)) == 0) {
      return()
    }
    groupname <- unique(filteredData(input$task_discrepantie, input$task_id_discrepantie)$group_name)
    taskname <- names(tasksArray()[as.numeric(input$task_discrepantie)])
    title <- paste(groupname, taskname, sep = '-')
    paste("<font color=\"#000000\"><h3>", title, "</h3></font>", sep = '')
  })
  
  output$tableTestInfoDiscrepantie <- renderTable({
    if(length(input$analyze_type) == 0 ||
       input$analyze_type == 'norm') {
      return()
    }
    if(is.null(dim(filteredData(input$task_discrepantie, input$task_id_discrepantie))) ||
       NROW(filteredData(input$task_discrepantie, input$task_id_discrepantie)) == 0) {
      return('Geen data gevonden.')
    }
    taskData(filteredData(input$task_discrepantie, input$task_id_discrepantie), input$date)
  })
  
  output$tableScoreDiscrepantie <- renderTable({
    if(length(input$analyze_type) == 0 ||
       input$analyze_type == 'norm') {
      return()
    }
    if(is.null(dim(filteredData(input$task_discrepantie, input$task_id_discrepantie))) ||
       NROW(filteredData(input$task_discrepantie, input$task_id_discrepantie)) == 0) {
      return(analizeDataManual(
        input$meanprop_discrepantie,
        input$testdate_discrepantie,
        input$date,
        input$task_discprepantie,
        input$gender,
        4))
    }
    
    analizeData(filteredData(input$task_discrepantie, input$task_id_discrepantie), input$date, input$task, input$gender, digits())
  }, width = "100%", bordered = FALSE, align = 'c', digits = 4)
  
  output$plotGrowthDiscrepantie <- renderPlot({
    if(length(input$analyze_type) == 0 ||
       input$analyze_type == 'norm') {
      return()
    }
    if(is.null(dim(filteredData(input$task_discrepantie, input$task_id_discrepantie))) ||
       NROW(filteredData(input$task_discrepantie, input$task_id_discrepantie)) == 0) {
      return('Geen data gevonden.')
    }
    storedData = getStoredData(input$task_discrepantie, input$gender)
    storedModel = getStoredModel(input$task_discrepantie, input$gender)
    
    averageData <- getAverage(filteredData(input$task_discrepantie, input$task_id_discrepantie))
    testDate = min(filteredData(input$task_discrepantie, input$task_id_discrepantie)$created_at)
    ageDays = ageAtTestDay(input$date, testDate)
    
    growthPlot(storedModel, storedData, ageDays, averageData$meanprop)
  })
  
  
  # output$plotPercentiel <- renderPlot({
  #   if(is.null(dim(filteredData(input$task, input$task_id))) ||
  #     NROW(filteredData(input$task, input$task_id)) == 0) {
  #       return('Geen data gevonden.')
  #   }
  #   storedData = getStoredData(input$task, input$gender)
  # 
  #   averageData <- getAverage(filteredData(input$task, input$task_id))
  #   testDate = min(filteredData(input$task, input$task_id)$created_at)
  #   percetielPlot(storedData, percentielScoreAge(input$date, testDate, averageData$meanprop))
  # })
  # 
  # 
  # output$plotDensity <- renderPlot({
  #   if(is.null(dim(filteredData(input$task, input$task_id))) ||
  #      NROW(filteredData(input$task, input$task_id)) == 0 ) {
  #       return('Geen data gevonden.')
  #   }
  # 
  #   averageData <- getAverage(filteredData(input$task, input$task_id))
  #   testDate = min(filteredData(input$task, input$task_id)$created_at)
  # 
  #   ageDays = ageAtTestDay(input$date, testDate)
  #   data = density(ageDays, averageData$meanprop, input$task, input$gender)
  #   densityScoreAgePlot(data)
  # })


  # Filter data based on selections
  # output$table <- DT::renderDataTable(DT::datatable({
  #   if(is.null(dim(filteredData(input$task, input$task_id))) ||
  #      NROW(filteredData(input$task, input$task_id)) == 0) {
  #     return(data.frame(c('error'), c('Geen data gevonden.')))
  #   }
  #   filteredData(input$task, input$task_id)
  # }))

  # output$urlText <- renderText({
  #   if(is.null(dim(filteredData(input$task, input$task_id))) ||
  #      NROW(filteredData(input$task, input$task_id)) == 0) {
  #     return('Geen data gevonden.')
  #   }
  #   averageData <- getAverage(filteredData(input$task, input$task_id))
  #   paste(sep = "",
  #     "protocol: ", session$clientData$url_protocol, "\n",
  #     "hostname: ", session$clientData$url_hostname, "\n",
  #     "pathname: ", session$clientData$url_pathname, "\n",
  #     "port: ",     session$clientData$url_port,     "\n",
  #     "search: ",   session$clientData$url_search,   "\n",
  #     "test: ", averageData$meanprop
  #   )
  # })

  output$report <- downloadHandler(
    filename = function() {
      groupname <- unique(filteredData(input$task, input$task_id)$group_name)
      taskname <- names(tasksArray()[as.numeric(input$task)])
      caseNumber <- unique(filteredData(input$task, input$task_id)$participant_case_number)
      tempFilename <- paste('report', caseNumber, groupname, taskname, sep = '-')
      tempFilename <- gsub("[[:punct:]]\\s+","_", tempFilename)
      paste(tempFilename, sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html'
      ))
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).

      groupname <- unique(filteredData(input$task, input$task_id)$group_name)
      taskname <- names(tasksArray()[as.numeric(input$task)])
      caseNumber <- unique(filteredData(input$task, input$task_id)$participant_case_number)

      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      storedModel = getStoredModel(input$task, input$gender)
      storedData = getStoredData(input$task, input$gender)

      averageData <- getAverage(filteredData(input$task, input$task_id))
      testDate = min(filteredData(input$task, input$task_id)$created_at)

      ageDays = ageAtTestDay(input$date, testDate)
      # Set up parameters to pass to Rmd document
      params <- list(
        groupname = groupname,
        taskname = taskname,
        path = getwd(),
        storedModel = storedModel,
        storedData = storedData,
        ageDays = ageDays,
        meanproportion = averageData$meanprop,
        percentielScore = percentielScoreAge(
          input$date,
          testDate,
          averageData$meanprop,
          input$task,
          input$gender),
        participantData = participantData(
          caseNumber,
          input$gender,
          input$date,
          unique(filteredData(input$task, input$task_id)$participant_name)),
        taskData = taskData(filteredData(input$task, input$task_id), input$date),
        analizeData = analizeData(
          filteredData(input$task, input$task_id),
          input$date,
          input$task,
          input$gender),
        densityData = density(
          ageDays,
          averageData$meanprop,
          input$task,
          input$gender)
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
