#************************************************************#
#*  Define Server App
#************************************************************#

server = function(input, output, session) {
  
  #************************************************************#
  #*  Dynamic UI Updates - Sidebar
  #************************************************************#
  
  #* Continent Selection
  output$continentSelection <- renderUI({
    selectInput('selectedContinent', 'Continent', choices = get_ListOfContinents(input$selectedDataSource))
  })
  
  #* Country Selection
  output$countrySelection <- renderUI({
    if (!is.null(input$selectedContinent)){
      listOfCountries = get_ListOfCountries_ByContinent(input$selectedDataSource, input$selectedContinent)
      listOfCountries = with(listOfCountries, split(CountryCode, CountryName))
      selectInput('selectedCountryCode', 'Country', choices = listOfCountries)
    }
  })
  
  #* Date Range Selection
  output$dateRangeSelection <- renderUI({
    if (!is.null(input$selectedCountryCode)){
      minDate = min(countryData()$date_asdate)
      maxDate = max(countryData()$date_asdate)
      dateRangeInput("modelingDates", label = 'Modeling Period', start = minDate, end = maxDate, min = minDate, max = maxDate)
    }
  })
  
  #************************************************************#
  #*  App Variables
  #************************************************************#
  
  #* Full COVID-19 Data for Selected Country
  countryData = reactive({
    if (!is.null(input$selectedCountryCode)){
      get_Data_ByCountryCode(input$selectedDataSource, input$selectedCountryCode)
    }
  })
  
  #* Model Data for Selected Country
  ModelingData = reactive({
    if (!is.null(input$selectedCountryCode) & !is.null(input$modelingDates)){
      get_ModelingData_ByCountryCodeAndDates(input$selectedDataSource, input$selectedCountryCode, input$modelingDates[1], input$modelingDates[2], input$selectedVariableY)
    }
  })
  
  #* Country Name
  countryName = reactive({
    get_CountryName_ByCountryCode(input$selectedDataSource, input$selectedCountryCode)
  })
  
  #************************************************************#
  #*  Output Area
  #************************************************************#

  #* Update Output Header
  output$Output_Header = countryName
  
  #* Update Output Initial Analysis
  output$Output_InitialAnalysis = renderText({
    if (!is.null(input$selectedCountryCode) & !is.null(input$modelingDates)){
      Render_InitialAnalysisV2(countryData(), input$modelingDates[1], input$modelingDates[2])
    } else {
      "Make some selections please ..."
    }
  })
  
  #************************************************************#
  #*  Output Plots
  #************************************************************#
  
  #* Plot Initial Analysis
  output$Plot_InitialAnalysis = renderPlot({
    print(input$selectedCountryCode)
    if (!is.null(input$selectedCountryCode) & !is.null(input$modelingDates)){
      localData = countryData()
      localCountryName = countryName()
      
      # Visualize Total Cases
      InitialPlots.TotalCases = ggplot(localData, aes(x = date_asdate)) + ggtitle(paste('Total cases in', localCountryName)) +
        geom_area(aes(y = total_cases), fill = "blue", alpha = 0.2) +
        geom_line(aes(x = date_asdate, y = total_cases), color = "blue") +
        xlab("Date") +
        ylab("Total Cases")
      
      # Visualize New Cases
      InitialPlots.NewCases = ggplot(localData, aes(x = date_asdate)) + ggtitle(paste('New cases in', localCountryName)) +
        geom_area(aes(y = new_cases), fill = "blue", alpha = 0.2) +
        geom_line(aes(x = date_asdate, y = new_cases), color = "blue") +
        xlab("Date") +
        ylab("New Cases")
      
      # Visualize Total Deaths
      InitialPlots.TotalDeaths = ggplot(localData, aes(x = date_asdate)) + ggtitle(paste('Total deaths in', localCountryName)) +
        geom_area(aes(y = total_deaths), fill = "black", alpha = 0.2) +
        geom_line(aes(x = date_asdate, y = total_deaths), color = "black") +
        xlab("Date") +
        ylab("Total Deaths")
      
      # Visualize New Deaths
      InitialPlots.NewDeaths = ggplot(localData, aes(x = date_asdate)) + ggtitle(paste('New cases in', localCountryName)) +
        geom_area(aes(y = new_deaths), fill = "black", alpha = 0.2) +
        geom_line(aes(x = date_asdate, y = new_deaths), color = "black") +
        xlab("Date") +
        ylab("New Deaths")
      
      # Display Plots
      grid.arrange(InitialPlots.TotalCases, InitialPlots.NewCases,
                   InitialPlots.TotalDeaths, InitialPlots.NewDeaths, nrow=2, ncol=2)
    }
  })


}
