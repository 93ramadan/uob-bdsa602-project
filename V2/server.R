#************************************************************#
#*  Define Server App
#************************************************************#

server = function(input, output, session) {
  
  #************************************************************#
  #*  Dynamic UI Updates - Sidebar
  #************************************************************#
  
  #* Data Source Selection
  listOfContinents = reactive({
    req(input$selectedDataSource)
    print(paste('data source changed to = ', input$selectedDataSource))
    
    if (input$selectedDataSource == 'local'){
      get_ListOfContinents(input$selectedDataSource)
    } else if (input$selectedDataSource == 'owid'){
      get_ListOfContinents(input$selectedDataSource)
    }
  })
  observeEvent(listOfContinents(), {
    updateSelectInput(session, "selectedContinent", choices = listOfContinents(), selected = "")
  })
  
  #* Continent Selection
  listOfCountries = reactive({
    req(input$selectedContinent)
    print(paste('continent changed to = ', input$selectedContinent))
    
    listOfCountries = get_ListOfCountries_ByContinent(input$selectedDataSource, input$selectedContinent)
    listOfCountries
  })
  observeEvent(listOfCountries(), {
    choices = listOfCountries()
    choices = setNames(choices$CountryCode, choices$CountryName)
    updateSelectInput(session, "selectedCountryCode", choices = choices, selected = "")
  })
  
  countrySelectedHasChanged <- reactiveVal(FALSE)
  #* Country Selection
  countryFullData = reactive({
    req(input$selectedDataSource)
    req(input$selectedCountryCode)
    req(listOfCountries()[which(listOfCountries()$CountryCode == input$selectedCountryCode), ]$CountryCode)
    print(paste('country changed to = ', input$selectedCountryCode))

    get_Data_ByCountryCode(input$selectedDataSource, input$selectedCountryCode)
  })
  observeEvent(countryFullData(), {
    minDate = min(countryFullData()$date_asdate)
    maxDate = max(countryFullData()$date_asdate)
    updateDateRangeInput(session, 'selectedDates', start = minDate, end = maxDate, min = minDate, max = maxDate)
    # Do not filter data on initial country load - delay to get latest date range values reflected
    countrySelectedHasChanged(TRUE)
    if (minDate == input$selectedDates[1] & maxDate == input$selectedDates[2]){
      # Date has not changed
      dateRangeHasChanged(FALSE)
    } else {
      dateRangeHasChanged(TRUE)
    }
  })
  #* Country Selection - Update Country Name
  output$Text_CountryName = renderText({
    req(input$selectedCountryCode)
    print("Update country name header")
    listOfCountries()[which(listOfCountries()$CountryCode == input$selectedCountryCode), ]$CountryName
  })
  #* Country Selection - Update Initial Analysis (Text)
  output$Text_InitialAnalysis = renderText({
    req(countryFilteredData())
    req((countrySelectedHasChanged() == TRUE & dateRangeHasChanged() == FALSE) |
          (countrySelectedHasChanged() == FALSE & dateRangeHasChanged() == TRUE))
    print("Update initial analysis output")
    
    Render_InitialAnalysisV2(countryFilteredData(), input$selectedDates[1], input$selectedDates[2])
  })
  
  dateRangeHasChanged <- reactiveVal(FALSE)
  #* Country and Date Range Selection
  countryFilteredData = reactive({
    req(countryFullData())
    req(input$selectedDates)
    req((countrySelectedHasChanged() == TRUE & dateRangeHasChanged() == FALSE) |
          (countrySelectedHasChanged() == FALSE & dateRangeHasChanged() == TRUE))
    print(paste('Updating filtered data for = ', input$selectedDates))
    
    countryFullData()[(countryFullData()$date_asdate >= input$selectedDates[1] & countryFullData()$date_asdate <= input$selectedDates[2]), ]
  })
  observeEvent(countryFilteredData(), {
    print('in event')
    
  })
  observeEvent(input$selectedDates, {
    # To allow data to be filtered
    dateRangeHasChanged(TRUE)
    countrySelectedHasChanged(FALSE)
  })
  
  
  #* Continent Selection
  #observeEvent(input$selectedContinent, {
  #  selectedContinent = input$selectedContinent
  #  
  #  print(paste('continent changed to = ', selectedContinent))
  #
  #  if (!is.null(session$userData$modelingDatesEvent)){
  #    session$userData$modelingDatesEvent$destroy()
  #  }
  #  
  #  output$dateRangeSelection <- renderUI({ })
  #  output$Output_Header = renderText({ })
  #  output$Output_InitialAnalysis = renderText({ })
  #  output$Plot_InitialAnalysis = renderPlot({ })
  #  
  #  if (!is.null(selectedContinent) & selectedContinent != ''){
  #    listOfCountries = get_ListOfCountries_ByContinent(input$selectedDataSource, selectedContinent)
  #    listOfCountries = with(listOfCountries, split(CountryCode, CountryName))
  #    updateSelectInput(session, "selectedCountryCode", choices = listOfCountries, selected = "")
  #  }
  #})
  
  #updating_type_inprogress <- reactiveVal(1)
  
  #* Country Selection
  #observeEvent(input$selectedCountryCode, {
  #  selectedCountryCode = input$selectedCountryCode
  #  updating_type_inprogress(1)
  #  
  #  print(paste('country changed to = ', selectedCountryCode))
  #  
  #  if (!is.null(session$userData$modelingDatesEvent)){
  #    session$userData$modelingDatesEvent$destroy()
  #  }
  #  removeUI("dateRangeSelection")
  #  removeUI("modelingDates")
  #  output$dateRangeSelection <- renderUI({ })
  #  output$Output_Header = renderText({ })
  #  output$Output_InitialAnalysis = renderText({ })
  #  output$Plot_InitialAnalysis = renderPlot({ })
  # 
  #  if (!is.null(selectedCountryCode) & selectedCountryCode != ''){
  #    selectedCountryName = get_CountryName_ByCountryCode(input$selectedDataSource, input$selectedCountryCode)
  #    
  #    #* Update Output Header
  #    output$Output_Header = renderText({
  #      print("Update country name header")
  #      selectedCountryName
  #    })
  #    
  #    minDate = min(countryData()$date_asdate)
  #    maxDate = max(countryData()$date_asdate)
  #    output$dateRangeSelection <- renderUI({
  #      dateRangeInput(inputId = "modelingDates", 'Modeling Period', start = minDate, end = maxDate, min = minDate, max = maxDate)
  #    })
  #    
  #    update_InitialAnalysis(output, selectedCountryName, countryData = countryData(), minDate = minDate, maxDate = maxDate)
  #   
  #    #* Date Range Selection
  #    session$userData$modelingDatesEvent = observeEvent(input$modelingDates, {
  #      modelingDates = input$modelingDates
  #      print(paste('modeling dates changed to = ', modelingDates[1], "until", modelingDates[2]))
  #      
  #      #req(!updating_type_inprogress())
  #      #* Update Output Initial Analysis
  #      output$Output_InitialAnalysis = renderText({
  #        print("From Date - Update initial analysis output")
  #        if (!is.null(input$selectedCountryCode) & input$selectedCountryCode != '' & !is.null(input$modelingDates)){
  #          Render_InitialAnalysisV2(countryData(), input$modelingDates[1], input$modelingDates[2])
  #        } else {
  #          "Make some selections please ..."
  #        }
  #      })
#
  #      #* Plot Initial Analysis
  #      output$Plot_InitialAnalysis = renderPlot({
  #        print("Update initial analysis plot")
  #        if (!is.null(input$selectedCountryCode) & input$selectedCountryCode != '' & !is.null(input$modelingDates)){
  #          localData = countryData()
  #          localCountryName = 'countryName()'
  #          
  #          # Visualize Total Cases
  #          InitialPlots.TotalCases = ggplot(localData, aes(x = date_asdate)) + ggtitle(paste('Total cases in', localCountryName)) +
  #            geom_area(aes(y = total_cases), fill = "blue", alpha = 0.2) +
  #            geom_line(aes(x = date_asdate, y = total_cases), color = "blue") +
  #            xlab("Date") +
  #            ylab("Total Cases")
  #          
  #          # Visualize New Cases
  #          InitialPlots.NewCases = ggplot(localData, aes(x = date_asdate)) + ggtitle(paste('New cases in', localCountryName)) +
  #            geom_area(aes(y = new_cases), fill = "blue", alpha = 0.2) +
  #            geom_line(aes(x = date_asdate, y = new_cases), color = "blue") +
  #            xlab("Date") +
  #            ylab("New Cases")
  #          
  #          # Visualize Total Deaths
  #          InitialPlots.TotalDeaths = ggplot(localData, aes(x = date_asdate)) + ggtitle(paste('Total deaths in', localCountryName)) +
  #            geom_area(aes(y = total_deaths), fill = "black", alpha = 0.2) +
  #            geom_line(aes(x = date_asdate, y = total_deaths), color = "black") +
  #            xlab("Date") +
  #            ylab("Total Deaths")
  #          
  #          # Visualize New Deaths
  #          InitialPlots.NewDeaths = ggplot(localData, aes(x = date_asdate)) + ggtitle(paste('New cases in', localCountryName)) +
  #            geom_area(aes(y = new_deaths), fill = "black", alpha = 0.2) +
  #            geom_line(aes(x = date_asdate, y = new_deaths), color = "black") +
  #            xlab("Date") +
  #            ylab("New Deaths")
  #          
  #          # Display Plots
  #          grid.arrange(InitialPlots.TotalCases, InitialPlots.NewCases,
  #                       InitialPlots.TotalDeaths, InitialPlots.NewDeaths, nrow=2, ncol=2)
  #        }
  #      })
  #      
  #      
  #    }, ignoreInit = TRUE, autoDestroy = TRUE)
  #    
  #  }
  #})
  
 
  
  #************************************************************#
  #*  App Variables
  #************************************************************#
  
  #* Full COVID-19 Data for Selected Country
  #countryData = reactive({
  #  print("loading country data")
  #  if (!is.null(input$selectedCountryCode) & input$selectedCountryCode != ''){
  #    get_Data_ByCountryCode(input$selectedDataSource, input$selectedCountryCode)
  #  }
  #})
  #
  ##* Model Data for Selected Country
  #ModelingData = reactive({
  #  print("or here?")
  #  if (!is.null(input$selectedCountryCode) & !is.null(input$modelingDates)){
  #    get_ModelingData_ByCountryCodeAndDates(input$selectedDataSource, input$selectedCountryCode, input$modelingDates[1], input$modelingDates[2], input$selectedVariableY)
  #  }
  #})
  
  #************************************************************#
  #*  Output Area
  #************************************************************#

  
  
  
  
  #************************************************************#
  #*  Output Plots
  #************************************************************#
  
}

update_InitialAnalysis = function(output, countryName, countryData, minDate, maxDate){
  
  #* Update Output Initial Analysis
  output$Output_InitialAnalysis = renderText({
    print("From Country - Update initial analysis output")
    if (!is.null(countryData) & !is.null(minDate) & !is.null(maxDate)){
      Render_InitialAnalysisV2(countryData, minDate, maxDate)
    } else {
      "Make some selections please ..."
    }
  })
  
  #* Plot Initial Analysis
  output$Plot_InitialAnalysis = renderPlot({
    print("From Country - Update initial analysis plot")
      
    # Visualize Total Cases
    InitialPlots.TotalCases = ggplot(countryData, aes(x = date_asdate)) + ggtitle(paste('Total cases in', countryName)) +
      geom_area(aes(y = total_cases), fill = "blue", alpha = 0.2) +
      geom_line(aes(x = date_asdate, y = total_cases), color = "blue") +
      xlab("Date") +
      ylab("Total Cases")
      
    # Visualize New Cases
    InitialPlots.NewCases = ggplot(countryData, aes(x = date_asdate)) + ggtitle(paste('New cases in', countryName)) +
      geom_area(aes(y = new_cases), fill = "blue", alpha = 0.2) +
      geom_line(aes(x = date_asdate, y = new_cases), color = "blue") +
      xlab("Date") +
      ylab("New Cases")
      
    # Visualize Total Deaths
    InitialPlots.TotalDeaths = ggplot(countryData, aes(x = date_asdate)) + ggtitle(paste('Total deaths in', countryName)) +
      geom_area(aes(y = total_deaths), fill = "black", alpha = 0.2) +
      geom_line(aes(x = date_asdate, y = total_deaths), color = "black") +
      xlab("Date") +
      ylab("Total Deaths")
      
    # Visualize New Deaths
    InitialPlots.NewDeaths = ggplot(countryData, aes(x = date_asdate)) + ggtitle(paste('New cases in', countryName)) +
      geom_area(aes(y = new_deaths), fill = "black", alpha = 0.2) +
      geom_line(aes(x = date_asdate, y = new_deaths), color = "black") +
      xlab("Date") +
      ylab("New Deaths")
      
    # Display Plots
    grid.arrange(InitialPlots.TotalCases, InitialPlots.NewCases,
                 InitialPlots.TotalDeaths, InitialPlots.NewDeaths, nrow=2, ncol=2)
  })
  
}
