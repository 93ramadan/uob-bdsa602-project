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
    
    get_countryName()
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
    countryName = get_countryName()
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
  
  trainingVStestingSplitHasChanged <- reactiveVal(FALSE)
  #* Training and Testing Split Selection
  observeEvent(input$selectedTrainingTestingSplit, {
    print('training-testing split has been updated')
    # To allow data to be filtered
    trainingVStestingSplitHasChanged(TRUE)
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
    req(input$selectedTrainingTestingSplit)
    print('(LR) model updating')
    
    build_LinearRegressionModel(modelData = countryModelingData(), input$selectedTrainingTestingSplit)
  })
  observeEvent(CountryModel_LR(),{
    print('(LR) country model has been updated')
  })
  #* Linear Regression Model Changed
  output$Plot_LR = renderPlot({
    req(CountryModel_LR())
    print('(LR) plot updating')
    
    # Get Country Name
    countryName = get_countryName()
    
    # Collect Data (Actual vs Training vs. Testing)
    modelingData = countryModelingData()
    modelingData_Training = get_trainingSplit(modelingData)
    modelingData_Testing = get_testingSplit(modelingData)
    
    # Predict Data (Training vs. Testing)
    predictionData_Training = predict(object = CountryModel_LR(), newdata = modelingData_Training)
    predictionData_Testing = predict(object = CountryModel_LR(), newdata = modelingData_Testing)
    
    # Plot - Actual
    plot(modelingData$X, modelingData$Y, pch=20, col="darkgrey",
         main=paste('New',get_YTitle(),'in',countryName,'(Actual vs. LR)'), xlab = 'Day', ylab = paste('Number of',get_YTitle()))
    lines(modelingData$X, modelingData$Y, lty=1, lwd=2, col="darkgrey")
    # Plot - Training
    lines(modelingData_Training$X, predictionData_Training, lty=1, lwd=2, col="blue")
    # Plot - Testing
    lines(modelingData_Testing$X, predictionData_Testing, lty=1, lwd=2, col="darkorange1")
    
    grid()
    
    #* Model Evaluations
    #* Text - LR Message
    output$Text_Plot_LR = renderText({
      req(CountryModel_LR())
      conclusionMessage = ''
      trainingMSE = mean((modelingData_Training$Y-predictionData_Training)^2)
      conclusionMessage = paste(conclusionMessage,'<b> Training MSE </b> =', round(trainingMSE,2) )
      testingMSE = mean((modelingData_Testing$Y-predictionData_Testing)^2)
      conclusionMessage = paste(conclusionMessage,'<br/><b> Testing MSE </b> =', round(testingMSE,2) )
      conclusionMessage
    })
  })
  
  #* Spline - Cubic - Model Changed
  CountryModel_SplineCubic = reactive({
    req(countryModelingData())
    req(input$selectedTrainingTestingSplit)
    print('(Spline - Cubic) model updating')
    
    build_CubicSplineModel(modelData = countryModelingData(), input$selectedTrainingTestingSplit)
  })
  observeEvent(CountryModel_SplineCubic(),{
    print('(Spline - Cubic) country model has been updated')
  })
  #* Spline - Cubic - Model Changed
  output$Plot_SplineCubic = renderPlot({
    req(CountryModel_SplineCubic())
    print('(Spline - Cubic) plot updating')
    
    # Get Country Name
    countryName = get_countryName()
    
    # Collect Data (Actual vs Training vs. Testing)
    modelingData = countryModelingData()
    modelingData_Training = get_trainingSplit(modelingData)
    modelingData_Testing = get_testingSplit(modelingData)
    
    # Predict Data (Training vs. Testing)
    predictionData_Training = predict(object=CountryModel_SplineCubic(), newdata=list(X = modelingData_Training$X), se=TRUE)
    predictionData_Training.Y = predictionData_Training$fit
    predictionData_Training.SE = predictionData_Training$se.fit
    predictionData_Training.CI = cbind(predictionData_Training.Y-2*predictionData_Training.SE, predictionData_Training.Y+2*predictionData_Training.SE)
    
    if (length(modelingData_Testing$X) > 0){
      predictionData_Testing = predict(object=CountryModel_SplineCubic(), newdata=list(X = modelingData_Testing$X), se=TRUE)
      predictionData_Testing.Y = predictionData_Testing$fit
      predictionData_Testing.SE = predictionData_Testing$se.fit
      predictionData_Testing.CI = cbind(predictionData_Testing.Y-2*predictionData_Testing.SE, predictionData_Testing.Y+2*predictionData_Testing.SE)
    }
    
    # Plot - Actual
    plot(modelingData$X, modelingData$Y, pch=20, col="darkgrey",
         main=paste('New',get_YTitle(),'in',countryName,'(Actual vs. Cubic Spline)'), xlab = 'Day', ylab = paste('Number of',get_YTitle()))
    lines(modelingData$X, modelingData$Y, lty=1, lwd=2, col="darkgrey")
    
    # Plot - Training
    lines(modelingData_Training$X, predictionData_Training.Y, lty=1, lwd=2, col="blue")
    matlines(modelingData_Training$X, predictionData_Training.CI, lty=2, lwd=2, col="red")
    # Plot Knot Locations
    modelKnotLocations = attr(bs(modelingData_Training$X, df=6), "knots")
    abline(v = as.numeric(c(modelKnotLocations)), lty = 2)
    
    if (length(modelingData_Testing$X) > 0){
      # Plot - Testing
      lines(modelingData_Testing$X, predictionData_Testing.Y, lty=1, lwd=2, col="darkorange1")
      matlines(modelingData_Testing$X, predictionData_Testing.CI, lty=2, lwd=2, col="darkorchid2")
    }
    
    grid()
    
    #* Model Evaluations
    #* Text - Spline-Cubic Message
    output$Text_Plot_SplineCubic = renderText({
      req(CountryModel_SplineCubic())
      conclusionMessage = ''
      trainingMSE = mean((modelingData_Training$Y-predictionData_Training.Y)^2)
      conclusionMessage = paste(conclusionMessage,'<b> Training MSE </b> =', round(trainingMSE,2) )
      if (length(modelingData_Testing$X) > 0){
        testingMSE = mean((modelingData_Testing$Y-predictionData_Testing.Y)^2)
        conclusionMessage = paste(conclusionMessage,'<br/><b> Testing MSE </b> =', round(testingMSE,2) )
      }
      conclusionMessage
    })
  })
  
  #* Spline - Natural - Model Changed
  CountryModel_SplineNatural = reactive({
    req(countryModelingData())
    req(input$selectedTrainingTestingSplit)
    print('(Spline - Natural) model updating')
    
    build_NaturalSplineModel(modelData = countryModelingData(), input$selectedTrainingTestingSplit)
  })
  observeEvent(CountryModel_SplineNatural(),{
    print('(Spline - Natural) country model has been updated')
  })
  #* Spline - Natural - Model Changed
  output$Plot_SplineNatural = renderPlot({
    req(CountryModel_SplineNatural())
    print('(Spline - Natural) plot updating')
    
    # Get Country Name
    countryName = get_countryName()
    
    # Collect Data (Actual vs Training vs. Testing)
    modelingData = countryModelingData()
    modelingData_Training = get_trainingSplit(modelingData)
    modelingData_Testing = get_testingSplit(modelingData)
    
    # Predict Data (Training vs. Testing)
    predictionData_Training = predict(object=CountryModel_SplineNatural(), newdata=list(X = modelingData_Training$X), se=TRUE)
    predictionData_Training.Y = predictionData_Training$fit
    predictionData_Training.SE = predictionData_Training$se.fit
    predictionData_Training.CI = cbind(predictionData_Training.Y-2*predictionData_Training.SE, predictionData_Training.Y+2*predictionData_Training.SE)
    
    if (length(modelingData_Testing$X) > 0){
      predictionData_Testing = predict(object=CountryModel_SplineNatural(), newdata=list(X = modelingData_Testing$X), se=TRUE)
      predictionData_Testing.Y = predictionData_Testing$fit
      predictionData_Testing.SE = predictionData_Testing$se.fit
      predictionData_Testing.CI = cbind(predictionData_Testing.Y-2*predictionData_Testing.SE, predictionData_Testing.Y+2*predictionData_Testing.SE)
    }
    
    # Plot - Actual
    plot(modelingData$X, modelingData$Y, pch=20, col="darkgrey",
         main=paste('New',get_YTitle(),'in',countryName,'(Actual vs. Natural Spline)'), xlab = 'Day', ylab = paste('Number of',get_YTitle()))
    lines(modelingData$X, modelingData$Y, lty=1, lwd=2, col="darkgrey")
    
    # Plot - Training
    lines(modelingData_Training$X, predictionData_Training.Y, lty=1, lwd=2, col="blue")
    matlines(modelingData_Training$X, predictionData_Training.CI, lty=2, lwd=2, col="red")
    # Plot Knot Locations
    modelKnotLocations = attr(ns(modelingData_Training$X, df=4), "knots")
    abline(v = as.numeric(c(modelKnotLocations)), lty = 2)
    
    if (length(modelingData_Testing$X) > 0){
      # Plot - Testing
      lines(modelingData_Testing$X, predictionData_Testing.Y, lty=1, lwd=2, col="darkorange1")
      matlines(modelingData_Testing$X, predictionData_Testing.CI, lty=2, lwd=2, col="darkorchid2")
    }
 
    grid()
    
    #* Model Evaluations
    #* Text - Spline-Natural Message
    output$Text_Plot_SplineNatural = renderText({
      req(CountryModel_SplineNatural())
      conclusionMessage = ''
      trainingMSE = mean((modelingData_Training$Y-predictionData_Training.Y)^2)
      conclusionMessage = paste(conclusionMessage,'<b> Training MSE </b> =', round(trainingMSE,2) )
      if (length(modelingData_Testing$X) > 0){
        testingMSE = mean((modelingData_Testing$Y-predictionData_Testing.Y)^2)
        conclusionMessage = paste(conclusionMessage,'<br/><b> Testing MSE </b> =', round(testingMSE,2) )
      }
      conclusionMessage
    })
  })
  
  #* Spline - Smooth - Model Changed
  CountryModel_SplineSmooth = reactive({
    req(countryModelingData())
    req(input$selectedTrainingTestingSplit)
    print('(Spline - Smooth) model updating')
    
    build_SmoothSplineModel(modelData = countryModelingData(), input$selectedTrainingTestingSplit)
  })
  observeEvent(CountryModel_SplineSmooth(),{
    print('(Spline - Smooth) country model has been updated')
  })
  #* Spline - Smooth - Model Changed
  output$Plot_SplineSmooth = renderPlot({
    req(CountryModel_SplineSmooth())
    print('(Spline - Smooth) plot updating')
    
    # Get Country Name
    countryName = get_countryName()
    
    # Collect Data (Actual vs Training vs. Testing)
    modelingData = countryModelingData()
    modelingData_Training = get_trainingSplit(modelingData)
    modelingData_Testing = get_testingSplit(modelingData)
    
    # Predict Data (Training vs. Testing)
    predictionData_Training = predict(object=CountryModel_SplineSmooth(), modelingData_Training$X)
    predictionData_Training.Y = predictionData_Training$y
    
    predictionData_Testing = predict(object=CountryModel_SplineSmooth(), modelingData_Testing$X)
    predictionData_Testing.Y = predictionData_Testing$y
    
    # Plot - Actual
    plot(modelingData$X, modelingData$Y, pch=20, col="darkgrey",
         main=paste('New',get_YTitle(),'in',countryName,'(Actual vs. Smooth Spline)'), xlab = 'Day', ylab = paste('Number of',get_YTitle()))
    lines(modelingData$X, modelingData$Y, lty=1, lwd=2, col="darkgrey")
    
    # Plot - Training
    lines(modelingData_Training$X, predictionData_Training.Y, lty=1, lwd=2, col="blue")
    
    # Plot - Testing
    lines(modelingData_Testing$X, predictionData_Testing.Y, lty=1, lwd=2, col="darkorange1")
    
    grid()
    
    grid()
    
    #* Model Evaluations
    #* Text - Spline-Smooth Message
    output$Text_Plot_SplineSmooth = renderText({
      req(CountryModel_SplineSmooth())
      conclusionMessage = ''
      trainingMSE = mean((modelingData_Training$Y-predictionData_Training.Y)^2)
      conclusionMessage = paste(conclusionMessage,'<b> Training MSE </b> =', round(trainingMSE,2) )
      #if (length(modelingData_Testing$X) > 0){
        testingMSE = mean((modelingData_Testing$Y-predictionData_Testing.Y)^2)
        conclusionMessage = paste(conclusionMessage,'<br/><b> Testing MSE </b> =', round(testingMSE,2) )
      #}
      conclusionMessage
    })
  })
  
  #************************************************************#
  #*  Common Functions
  #************************************************************#
  
  get_YTitle = function(){
    title = unlist(strsplit(input$selectedVariableY, '_'))[2]
    title = paste(toupper(substring(title, 1,1)), substring(title, 2), sep = '')
    return(title)
  }
  
  get_countryName = function(){
    return(listOfCountries()[which(listOfCountries()$CountryCode == input$selectedCountryCode), ]$CountryName)
  }
  
  get_trainingSplit = function(modelData){
    # Training vs Testing Split
    sampleSize = dim(modelData)[1]
    modelingTrainingSet.Count = round((sampleSize * (input$selectedTrainingTestingSplit/100)), 0)
    
    # Create Training Data Set
    modelData_Training = modelData[1:modelingTrainingSet.Count, ]
    return(modelData_Training)
  }
  
  get_testingSplit = function(modelData){
    # Testing Split
    sampleSize = dim(modelData)[1]
    modelingTrainingSet.Count = round((sampleSize * (input$selectedTrainingTestingSplit/100)), 0)
    modelingTestingSet.Count = round((sampleSize * (1-(input$selectedTrainingTestingSplit/100))), 0)
    
    # Create Testing Data Set
    modelData_Testing = tail(modelData, modelingTestingSet.Count)
    return(modelData_Testing)
  }
}