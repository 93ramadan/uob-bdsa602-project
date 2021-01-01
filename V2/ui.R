#************************************************************#
#*  Define UI App
#************************************************************#

ui = fluidPage(
  # Header
  headerPanel('COVID-19 Modeling Project'),
  # Side-bar Layout
  sidebarLayout(
    # Side-bar Panel
    sidebarPanel(
      #width = 3,
      radioButtons("selectedDataSource", 'Data Source', choices = list('Local'='local','Our World In Date'='owid'), selected = 'local'),
      selectInput('selectedContinent', 'Continent', c("")),
      selectInput('selectedCountryCode', 'Country', c("")),
      #uiOutput("continentSelection"),
      #uiOutput("countrySelection"),
      uiOutput("dateRangeSelection"),
      radioButtons("selectedVariableY", 'Variable of Interest', choices = list('Total Cases'='total_cases','New Cases'='new_cases','New Deaths'='new_deaths'), selected = 'total_cases'),
    ),
    # Main Panel
    mainPanel(
      h2(textOutput("Output_Header")),
      htmlOutput("Output_InitialAnalysis"),
      hr(),
      h2('Cases vs. Deaths Overview'),
      plotOutput("Plot_InitialAnalysis"),
      hr(),
    )
  )
)
