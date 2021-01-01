#************************************************************#
#*  Define UI App
#************************************************************#

ui = fluidPage(
  # Header
  headerPanel('COVID-19 Modeling Project'),
  fluidRow(
    column(4, 
      wellPanel(
        radioButtons("selectedDataSource", 'Data Source', choices = list('Local'='local','Our World In Date'='owid'), selected = 'local'),
        selectInput('selectedContinent', 'Continent', choices = NULL),
        selectInput('selectedCountryCode', 'Country',  choices = NULL),
        dateRangeInput('selectedDates', 'Modeling Period'),
        sliderInput("selectedTrainingTestingSplit", "Percentage of Training Dataset (Training vs. Testing Split):", min = 20, max = 90, value = 70, step = 5),
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
  fluidRow(
    column(12, align="center", span(htmlOutput("Text_PlotHeaderWarnings"), style="color:red")),
    column(3, plotOutput("Plot_LR", height = "400px")),
    column(3, plotOutput("Plot_SplineCubic", height = "400px")),
    column(3, plotOutput("Plot_SplineNatural", height = "400px")),
    column(3, plotOutput("Plot_SplineSmooth", height = "400px")),
  )
)
