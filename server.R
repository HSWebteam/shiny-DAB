#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# options(shiny.port = 5732)

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
    if (query$returnurl != 'https://dab.app.uu.nl') {
      return(5)
    }
    return(0)
  })

  filteredData <- function(task, taskId) {
    # variable task id is only used when for a task multiple resutls exists.
    data = fetchedData()
    if (is.null(dim(data)) ||
       NROW(data) == 0) {
        return(FALSE)
    }
    if (length(task) == 0) {
      return(FALSE)
    }

    if (task == '1') {
      #Lion game
      data = data[data$taak == '1', ]
    }

    if (task == '2') {
      # Monkey game
      data = data[data$taak == '2', ]
      data = data[data$theme == 'text_theme', ]
    }

    if (task == '3') {
      # monkey game
      data = data[data$taak == '2', ]
      data = data[data$theme == 'picture_theme', ]
    }

    if (length(unique(data$task_id)) > 1 ) {
      # for the taak/theme combination multiple task_id's exists.
      # therefore use the task_id selector.
      if (length(taskId) == 0) {
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
    if (is.null(dim(data)) ||
       NROW(data) == 0) {
      return(FALSE)
    }
    if (length(input$task) == 0) {
      return(FALSE)
    }

    return(data)
  }

  output$analyzeSelector <- renderUI({
    radioButtons(
      "analyze_type",
      "selecteer Analyse:",
      list("Norm analyse" = "norm", "Discrepantie analyse" = "discrepantie"),
      "norm"
    )
  })

  output$taskSelector <- renderUI({
    tasks <- tasksArray()
    if (length(input$analyze_type) != 0 &&
       input$analyze_type == 'discrepantie') {
      tasks = tasks[1]
    }
    radioButtons(
      "task",
      "selecteer taak:",
      tasks)
  })

  output$taskSelectorCounterpart <- renderUI({
    if (length(input$analyze_type) != 0 &&
       input$analyze_type == 'discrepantie') {
      tasks <- tasksArray()
      radioButtons(
        "task_counterpart",
        "selecteer taak:",
        tasks[2:3])
    }
  })

  output$taskIdSelector <- renderUI({
    if (is.recursive(fetchedData())) {
      data = fetchedData()
      data = data[data$taak == input$task, ]
      # when for given task multiple id's exist, display id selector.
      if (NROW(data) > 0 ) {
        if (length(unique(data$task_id)) > 1 ) {
          radioButtons("task_id", "selecteer taak id:", unique(data$task_id))
        }
      }
    }
  })

  output$taskIdSelectorCounterpart <- renderUI({
    if (length(input$analyze_type) != 0 &&
       input$analyze_type == 'discrepantie') {
      if (is.recursive(fetchedData())) {
        data = fetchedData()
        data = data[data$taak == input$task_counterpart, ]
        # when for given task_discrepantie multiple id's exist, display id selector.
        if (NROW(data) > 0 ) {
          if (length(unique(data$task_id)) > 1 ) {
            radioButtons("task_id_counterpart", "selecteer taak id:", unique(data$task_id))
          }
        }
      }
    }
  })

  output$average <- renderUI({
    # This is only used when no data is fetched from the dap.app.
    # Mostly when this shiny app is used as standalone.
    if (!is.recursive(fetchedData())) {
      sliderInput("meanprop", "proportie correct", .01, 1, .5, step = .005)
    }
  })

  output$averageCounterpart <- renderUI({
    # This is only used when no data is fetched from the dap.app.
    # Mostly when this shiny app is used as standalone.
    if (length(input$analyze_type) != 0 &&
       input$analyze_type == 'discrepantie') {
      if (!is.recursive(fetchedData())) {
        sliderInput("meanprop_monkey", "proportie correct", .01, 1, .5, step = .005)
      }
    }
  })

  output$testdate <- renderUI({
    # This is only used when no data is fetched from the dap.app.
    # Mostly when this shiny app is used as standalone.
    if (!is.recursive(fetchedData())) {
      dateInput('testdate',
                label = 'TestDate: jjjj-mm-dd',
                value = Sys.Date() - years(6)
      )
    }
  })

  output$testdateCounterpart <- renderUI({
    # This is only used when no data is fetched from the dap.app.
    # Mostly when this shiny app is used as standalone.
    if (length(input$analyze_type) != 0 &&
       input$analyze_type == 'discrepantie') {
      if (!is.recursive(fetchedData())) {
        dateInput(
          'testdate_monkey',
          label = 'TestDate: jjjj-mm-dd',
          value = Sys.Date() - years(6)
        )
      }
    }
  })

  output$warning <- renderText({
    if (is.null(dim(filteredData(input$task, input$task_id))) ||
       NROW(filteredData(input$task, input$task_id)) == 0) {
      return()
    }
    status <- subset(filterTaskStatus(), task_id == unique(filteredData(input$task, input$task_id)$task_id))
    if (status$finished != 1) {
      return("<font color=\"#FF0000\"><b>Let op! participant heeft deze taak nog niet afgerond</b></font>")
    }

    storedData = getStoredData(input$task, input$gender)
    testDate = min(filteredData(input$task, input$task_id)$created_at)
    ageDays = ageAtTestDay(input$date, testDate)

    if (max(storedData$lftd) < ageDays ||
        min(storedData$lftd) > ageDays ) {
      return("<font color=\"#FF0000\"><b>Let op! Leeftijd ligt buiten de normgroep</b></font>")
    }
    
    if (is.null(dim(filteredData(input$task_counterpart, input$task_id_counterpart))) ||
       NROW(filteredData(input$task_monkey, input$task_id_monkey)) == 0) {
      return()
    }
    
    statusCounterpart <- subset(
        filterTaskStatus(),
        task_id == unique(filteredData(input$task_counterpart, input$task_id_counterpart)$task_id))
    if (statusCounterpart$finished != 1) {
      return("<font color=\"#FF0000\"><b>Let op! participant heeft deze taak nog niet afgerond</b></font>")
    }
  })

  output$tableGeneral <- renderTable({
    if (is.null(dim(filteredData(input$task, input$task_id))) ||
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

  output$subtitleDiscrepantieAnalyse <- renderText({
    if (length(input$analyze_type) == 0 ||
       input$analyze_type == 'norm') {
      return()
    }
    paste("<font color=\"#000000\"><h3>Discrepantie analyse</h3></font>", sep = '')
  })

  output$tableDiscrepantieAnalyse <- renderTable({
    if (length(input$analyze_type) == 0 ||
       input$analyze_type == 'norm') {
      return()
    }

    percentage = 10
    if (is.null(dim(filteredData(input$task_counterpart, input$task_id_counterpart))) ||
       NROW(filteredData(input$task_counterpart, input$task_id_counterpart)) == 0) {
      # When no data is found, the manual input is anabled.
      # This is only used when this shiny app is used as a standalone.
      discrepantiecore = discrepantie(
        input$date,
        input$testdate,
        input$testdate_counterpart,
        input$meanprop,
        input$meanprop_counterpart,
        input$gender,
        percentage)

      if (discrepantiecore > 0 ||
         discrepantiecore == FALSE) {
        return(data.frame(
          'Discrepantie' = 'Geen data gevonden'
        ))
      }

      return(data.frame(
        'Discrepantie' = as.character(round(discrepantiecore[1], 5)),
        'Kritieke waarde' = as.character(round(discrepantiecore[2], 5)),
        'Nominale sign. niveau in %' = as.character(round(percentage, 5)),
        check.names = FALSE
      ))
    }

    filteredDataLion <- filteredData(input$task, input$task_id)
    averageDataLion <- getAverage(filteredDataLion)
    testDateLion = min(filteredDataLion$created_at)

    filteredDataMonkey <- filteredData(input$task_counterpart, input$task_id_counterpart)
    averageDataMonkey <- getAverage(filteredDataMonkey)
    testDateMonkey = min(filteredDataMonkey$created_at)

    discrepantieAnalyze(input$date,
                        testDateLion,
                        testDateMonkey,
                        averageDataLion$meanprop,
                        averageDataMonkey$meanprop,
                        input$gender,
                        digits())

  }, width = "75%", bordered = FALSE, align = 'c', digits = 4)

  output$subtitle <- renderText({
    if (is.null(dim(filteredData(input$task, input$task_id))) ||
       NROW(filteredData(input$task, input$task_id)) == 0) {
      return()
    }
    groupname <- unique(filteredData(input$task, input$task_id)$group_name)
    taskname <- names(tasksArray()[as.numeric(input$task)])
    title <- paste(groupname, taskname, sep = '-')
    paste("<font color=\"#000000\"><h3>", title, "</h3></font>", sep = '')
  })


  output$tableTestInfo <- renderTable({
    if (is.null(dim(filteredData(input$task, input$task_id))) ||
       NROW(filteredData(input$task, input$task_id)) == 0) {
      return('Geen data gevonden.')
    }
    taskData(filteredData(input$task, input$task_id), input$date)
  })

  output$tableScore <- renderTable({
    if (is.null(dim(filteredData(input$task, input$task_id))) ||
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
    if (is.null(dim(filteredData(input$task, input$task_id))) ||
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

  output$subtitleMonkey <- renderText({
    if (length(input$analyze_type) == 0 ||
       input$analyze_type == 'norm') {
      return()
    }
    if (is.null(dim(filteredData(input$task_counterpart, input$task_id_counterpart))) ||
       NROW(filteredData(input$task_counterpart, input$task_id_counterpart)) == 0) {
      return()
    }
    groupname <- unique(filteredData(input$task_counterpart, input$task_id_counterpart)$group_name)
    taskname <- names(tasksArray()[as.numeric(input$task_counterpart)])
    title <- paste(groupname, taskname, sep = '-')
    paste("<font color=\"#000000\"><h3>", title, "</h3></font>", sep = '')
  })

  output$tableTestInfoCounterpart <- renderTable({
    if (length(input$analyze_type) == 0 ||
       input$analyze_type == 'norm') {
      return()
    }
    if (is.null(dim(filteredData(input$task_counterpart, input$task_id_counterpart))) ||
       NROW(filteredData(input$task_counterpart, input$task_id_counterpart)) == 0) {
      return('Geen data gevonden.')
    }
    taskData(filteredData(input$task_counterpart, input$task_id_counterpart), input$date)
  })

  output$tableScoreCounterpart <- renderTable({
    if (length(input$analyze_type) == 0 ||
       input$analyze_type == 'norm') {
      return()
    }

    if (is.null(dim(filteredData(input$task_counterpart, input$task_id_counterpart))) ||
       NROW(filteredData(input$task_counterpart, input$task_id_counterpart)) == 0) {

      return(analizeDataManual(input$meanprop_counterpart,
                               input$testdate_counterpart,
                               input$date,
                               input$task_counterpart,
                               input$gender,
                               4))
    }

    analizeData(filteredData(input$task_counterpart, input$task_id_counterpart),
                input$date,
                input$task_counterpart,
                input$gender,
                digits())
  }, width = "100%", bordered = FALSE, align = 'c', digits = 4)

  output$plotGrowthCounterpart <- renderPlot({
    if (length(input$analyze_type) == 0 ||
       input$analyze_type == 'norm') {
      return()
    }
    if (is.null(dim(filteredData(input$task_counterpart, input$task_id_counterpart))) ||
       NROW(filteredData(input$task_counterpart, input$task_id_counterpart)) == 0) {
      return('Geen data gevonden.')
    }
    storedData = getStoredData(input$task_counterpart, input$gender)
    storedModel = getStoredModel(input$task_counterpart, input$gender)

    averageData <- getAverage(filteredData(input$task_counterpart, input$task_id_counterpart))
    testDate = min(filteredData(input$task_counterpart, input$task_id_counterpart)$created_at)
    ageDays = ageAtTestDay(input$date, testDate)

    growthPlot(storedModel, storedData, ageDays, averageData$meanprop)
  })



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

      if (input$analyze_type == 'norm') {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        params <- listNormData(
            filteredData(input$task, input$task_id),
            input$task,
            input$task_id,
            input$gender,
            input$date)
      }

      if (input$analyze_type == 'discrepantie') {
        tempReport <- file.path(tempdir(), "reportCounterpart.Rmd")
        file.copy("reportCounterpart.Rmd", tempReport, overwrite = TRUE)
        params <- listCounterpartData(
          filteredData(input$task, input$task_id),
          filteredData(input$task_counterpart, input$task_id_counterpart),
          input$task,
          input$task_counterpart,
          input$task_id,
          input$task_id_counterpart,
          input$gender,
          input$date)
      }

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
