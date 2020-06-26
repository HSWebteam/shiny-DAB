library('lubridate')
fluidPage(
    titlePanel("Utrecht University Developmental Assessment Battery"),
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
            uiOutput("analyzeSelector"),
            uiOutput("taskSelector"),
            uiOutput("taskIdSelector"),
            uiOutput("average"),
            uiOutput("testdate"),
            
            uiOutput("taskSelectorDiscrepantie"),
            uiOutput("taskIdSelectorDiscrepantie"),
            uiOutput("averageDiscrepantie"),
            uiOutput("testdateDiscrepantie"),
            
            # verbatimTextOutput("tempText"),
            dateInput('date',
                label = 'Geboortedatum: jjjj-mm-dd',
                min = Sys.Date() - years(15), max = Sys.Date() - years(5),
                value = Sys.Date() - years(6)
            ),
            radioButtons('gender', 'Geslacht:',
                         c('n.v.t' = 'na',
                           'Mannelijk' = 'man',
                           'Vrouwelijk' = 'vrouw')
            ),
            radioButtons('format', 
                         'Selecteer bestandsformat:', 
                         c('PDF', 'HTML'),
                         inline = TRUE),
            downloadButton('report', 'Generate report')
        ),
        mainPanel(
            h1("Report"),
            htmlOutput("warning"),
            tableOutput("tableGeneral"),
            
            htmlOutput("subtitleDiscrepantieAnalyse"),
            tableOutput("tableDiscrepantieAnalyse"),
            
            htmlOutput("subtitle"),
            tableOutput("tableTestInfo"),
            tableOutput("tableScore"),
            plotOutput("plotGrowth", width = 500, height = 400),
            
            ## these items are displayed when the discrepantie analysis is checked.
            htmlOutput("subtitleDiscrepantie"),
            tableOutput("tableTestInfoDiscrepantie"),
            tableOutput("tableScoreDiscrepantie"),
            plotOutput("plotGrowthDiscrepantie", width = 500, height = 400)
        )
    )
)