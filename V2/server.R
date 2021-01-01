#************************************************************#
#*  Define Server App
#************************************************************#

server = function(input, output, session) {
  
  #************************************************************#
  #*  Dynamic UI Updates
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
  
  #* Country or Date Range Selection - Update Initial Analysis (Text)
  output$Text_InitialAnalysis = renderText({
    req(countryFilteredData())
    req((countrySelectedHasChanged() == TRUE & dateRangeHasChanged() == FALSE) |
          (countrySelectedHasChanged() == FALSE & dateRangeHasChanged() == TRUE))
    print("Update initial analysis output text")
    
    Text_InitialAnalysis(countryFilteredData(), input$selectedDates[1], input$selectedDates[2])
  })
  
  #* Country or Date Range Selection - Update Initial Analysis (Plots)
  output$Plot_InitialAnalysis = renderPlot({
    req(countryFilteredData())
    req((countrySelectedHasChanged() == TRUE & dateRangeHasChanged() == FALSE) |
          (countrySelectedHasChanged() == FALSE & dateRangeHasChanged() == TRUE))
    print("Update initial analysis output plots")
    
    output$Plot_InitialAnalysisTitle = renderText({
      "Cases vs. Deaths Overview"
    })
    countryName = listOfCountries()[which(listOfCountries()$CountryCode == input$selectedCountryCode), ]$CountryName
    Plot_InitialAnalysis(countryName, countryFilteredData())
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
    print('country filtered data has been updated')
  })
  observeEvent(input$selectedDates, {
    # To allow data to be filtered
    dateRangeHasChanged(TRUE)
    countrySelectedHasChanged(FALSE)
  })
  
  #* Country Filtered Data Changed
  countryModelingData = reactive({
    req(input$selectedVariableY)
    req(countryFilteredData())
    print(paste('Updating modeling data'))
    
    get_ModelingData_ByCountryData(countryFilteredData(), input$selectedVariableY)
  })
  observeEvent(countryModelingData(),{
    print('country modeling data has been updated')
  })
  
  #* Plots Header Warning Messages
  output$Text_PlotHeaderWarnings = renderText({
    req(input$selectedVariableY)
    req(countryFilteredData())
    print(paste('Updating plots section warning header'))
    
    Text_PlotHeaderWarnings_ByModelingData(countryFilteredData(), input$selectedVariableY)
  })
  
  #* Linear Regression Model Changed
  CountryModel_LR = reactive({
    req(countryModelingData())
    print('(LR) model updating')
    
    build_LinearRegressionModel(modelData = countryModelingData())
  })
  observeEvent(CountryModel_LR(),{
    print('(LR) country model has been updated')
  })
  #* Linear Regression Model Changed
  output$Plot_LR = renderPlot({
    req(CountryModel_LR())
    print('(LR) plot updating')
    
    # Prepare Data vs Prediction
    modelingData = countryModelingData()
    predictionData = predict(object = CountryModel_LR(), newdata = modelingData)
    
    # Plots
    plot(modelingData$X, modelingData$Y, pch=20, col="darkgrey",
         main=paste('New',get_YTitle(),'(Actual vs. LR)'), xlab = 'Day', ylab = paste('Number of',get_YTitle()))
    lines(modelingData$X, modelingData$Y, lty=1, lwd=2, col="darkgrey")
    lines(modelingData$X, predictionData, lty=1, lwd=2, col="blue")
    grid()
  })
  
  #* Spline - Cubic - Model Changed
  CountryModel_SplineCubic = reactive({
    req(countryModelingData())
    print('(Spline - Cubic) model updating')
    
    build_CubicSplineModel(modelData = countryModelingData())
  })
  observeEvent(CountryModel_SplineCubic(),{
    print('(Spline - Cubic) country model has been updated')
  })
  #* Spline - Cubic - Model Changed
  output$Plot_SplineCubic = renderPlot({
    req(CountryModel_SplineCubic())
    print('(Spline - Cubic) plot updating')
    
    # Prepare Data vs Prediction
    modelingData = countryModelingData()
    predictionData = predict(object=CountryModel_SplineCubic(), newdata=list(X = modelingData$X), se=TRUE)
    predictionData.Y = predictionData$fit
    predictionData.SE = predictionData$se.fit
    predictionData.CI = cbind(predictionData.Y-2*predictionData.SE, predictionData.Y+2*predictionData.SE)
    
    # Plots
    plot(modelingData$X, modelingData$Y, pch=20, col="darkgrey",
         main=paste('New',get_YTitle(),'(Actual vs. Cubic Spline)'), xlab = 'Day', ylab = paste('Number of',get_YTitle()))
    lines(modelingData$X, modelingData$Y, lty=1, lwd=2, col="darkgrey")
    lines(modelingData$X, predictionData.Y, lty=1, lwd=2, col="blue")
    matlines(modelingData$X, predictionData.CI, lty=2, lwd=2, col="red")
    grid()
    
    # Plot Knot Locations
    modelKnotLocations = attr(bs(modelingData$X, df=6), "knots")
    abline(v = as.numeric(c(modelKnotLocations)), lty = 2)
  })
  
  #* Spline - Natural - Model Changed
  CountryModel_SplineNatural = reactive({
    req(countryModelingData())
    print('(Spline - Natural) model updating')
    
    build_NaturalSplineModel(modelData = countryModelingData())
  })
  observeEvent(CountryModel_SplineNatural(),{
    print('(Spline - Natural) country model has been updated')
  })
  #* Spline - Natural - Model Changed
  output$Plot_SplineNatural = renderPlot({
    req(CountryModel_SplineNatural())
    print('(Spline - Natural) plot updating')
    
    # Prepare Data vs Prediction
    modelingData = countryModelingData()
    predictionData = predict(object=CountryModel_SplineNatural(), newdata=list(X = modelingData$X), se=TRUE)
    predictionData.Y = predictionData$fit
    predictionData.SE = predictionData$se.fit
    predictionData.CI = cbind(predictionData.Y-2*predictionData.SE, predictionData.Y+2*predictionData.SE)
    
    # Plots
    plot(modelingData$X, modelingData$Y, pch=20, col="darkgrey",
         main=paste('New',get_YTitle(),'(Actual vs. Natural Spline)'), xlab = 'Day', ylab = paste('Number of',get_YTitle()))
    lines(modelingData$X, modelingData$Y, lty=1, lwd=2, col="darkgrey")
    lines(modelingData$X, predictionData.Y, lty=1, lwd=2, col="blue")
    matlines(modelingData$X, predictionData.CI, lty=2, lwd=2, col="red")
    grid()
    
    # Plot Knot Locations
    modelKnotLocations = attr(ns(modelingData$X, df=4), "knots")
    abline(v = as.numeric(c(modelKnotLocations)), lty = 2)
  })
  
  #* Spline - Smooth - Model Changed
  CountryModel_SplineSmooth = reactive({
    req(countryModelingData())
    print('(Spline - Smooth) model updating')
    
    build_SmoothSplineModel(modelData = countryModelingData())
  })
  observeEvent(CountryModel_SplineSmooth(),{
    print('(Spline - Smooth) country model has been updated')
  })
  #* Spline - Smooth - Model Changed
  output$Plot_SplineSmooth = renderPlot({
    req(CountryModel_SplineSmooth())
    print('(Spline - Smooth) plot updating')
    
    # Prepare Data vs Prediction
    modelingData = countryModelingData()
    predictionData = predict(object=CountryModel_SplineSmooth(), newdata=list(X = modelingData$X))
    predictionData.Y = predictionData$y
    
    # Plots
    plot(modelingData$X, modelingData$Y, pch=20, col="darkgrey",
         main=paste('New',get_YTitle(),'(Actual vs. Smooth Spline)'), xlab = 'Day', ylab = paste('Number of',get_YTitle()))
    lines(modelingData$X, modelingData$Y, lty=1, lwd=2, col="darkgrey")
    lines(modelingData$X, predictionData.Y, lty=1, lwd=2, col="blue")
    grid()
  })
  
  #************************************************************#
  #*  Common Functions
  #************************************************************#
  
  get_YTitle = function(){
    title = unlist(strsplit(input$selectedVariableY, '_'))[2]
    title = paste(toupper(substring(title, 1,1)), substring(title, 2), sep = '')
    return(title)
  }
}