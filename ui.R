library('lubridate')
fluidPage(
    titlePanel("Utrecht University Developmental Assessment Battery"),
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
            uiOutput("taskIdSelector"),
            uiOutput("taskSelector"),
            # verbatimTextOutput("tempText"),
            dateInput('date',
                label = 'Geboortedatum: jjjj-mm-dd',
                min = Sys.Date() - years(15), max = Sys.Date() - years(5),
                value = Sys.Date() - years(6)
            ),
            radioButtons('gender', 'Sekse:',
                         c('n.v.t' = 'na',
                           'Mannelijk' = 'male',
                           'Vrouwelijk' = 'female')
            ),
            radioButtons('region', 'Regio:',
                         c('n.v.t' = 'na',
                           'Noord' = 'noord',
                           'Oost' = 'oost',
                           'Zuid' = 'zuid',
                           'West' = 'west')
            ),
            radioButtons('format', 'Selecteer bestandsformat:', c('PDF', 'HTML'),
                         inline = TRUE),
            downloadButton('report', 'Generate report')
        ),
        mainPanel(
            h1("Report"),
            tableOutput("tableGeneral"),
            tableOutput("tableTestInfo"),
            tableOutput("tableScore"),
            plotOutput("plotGrowth", width = 500, height = 400),
            plotOutput("plotDensity", width = 500, height = 400)
        )
    )
    # DT::dataTableOutput("table")
)