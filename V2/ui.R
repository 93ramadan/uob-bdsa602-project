#************************************************************#
#*  Define UI App
#************************************************************#

ui = fluidPage(
  # Header
  fluidRow(
    column(4, align="center"),
    column(4, align="center", 
           h2('Modeling the Dynamics of COVID-19'),
           span('Supervised by Dr. Sawsan Hilal'), br(),
           span('Built by Aisha Khalid and Ahmed Khedr'),
           ),
    column(4, align="right",
           imageOutput('UOB_LOGO', height = 120)
           )
    ),
  # Sidebar and Overview
  fluidRow(
    column(4, 
           wellPanel(
             radioButtons("selectedDataSource", 'Data Source', choices = list('Local'='local','Our World In Data'='owid'), selected = 'local'),
             selectInput('selectedContinent', 'Continent', choices = NULL),
             selectInput('selectedCountryCode', 'Country',  choices = NULL),
             dateRangeInput('selectedDates', 'Modeling Period'),
             sliderInput("selectedTrainingTestingSplit", "Percentage of Training Dataset (Training vs. Testing Split):", min = 20, max = 100, value = 80, step = 5),
             radioButtons("selectedVariableY", 'Variable of Interest', choices = list('New Cases'='new_cases','New Deaths'='new_deaths'), selected = 'new_cases')
             )),
    column(8, 
           wellPanel(
             h2(textOutput("Text_CountryName")),
             htmlOutput("Text_InitialAnalysis"),
             hr(),
             h2(textOutput("Plot_InitialAnalysisTitle")),
             plotOutput("Plot_InitialAnalysis"),
             )
           )
    ),
  # Analysis Output
  fluidRow(
    column(12, align="center", span(htmlOutput("Text_PlotHeaderWarnings"), style="color:red")),
    column(3, align="center",
           plotOutput("Plot_LR", height = "400px"),
           htmlOutput("Text_Plot_LR")
           ),
    column(3, align="center",
           plotOutput("Plot_SplineCubic", height = "400px"),
           htmlOutput("Text_Plot_SplineCubic")
           ),
    column(3, align="center",
           plotOutput("Plot_SplineNatural", height = "400px"),
           htmlOutput("Text_Plot_SplineNatural")
           ),
    column(3, align="center",
           plotOutput("Plot_SplineSmooth", height = "400px"),
           htmlOutput("Text_Plot_SplineSmooth")
           ),
    ),
  # Footer
  fluidRow(
    column(12, align="left",
           hr(),
           a("Source: Our World In Data", href = "https://ourworldindata.org/covid-cases"), br(),
           hr(),
    )
  ),
)
