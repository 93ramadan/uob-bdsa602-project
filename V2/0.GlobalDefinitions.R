#************************************************************#
#*  Variables
#************************************************************#

List_Continents = NULL
List_Countries = NULL

Local_List_Continents = NULL
Local_List_Countries = NULL

FullDataSet = NULL
LocalDataSet = NULL

#************************************************************#
#*  Functions
#************************************************************#

get_ListOfContinents = function(selectedDataSource){
  if (selectedDataSource == 'local'){
    return(Local_List_Continents)
  } else if (selectedDataSource == 'owid'){
    return(List_Continents$ContinentName)
  }
}

get_ListOfCountries_ByContinent = function(selectedDataSource, selectedContinent){
  if (selectedDataSource == 'local'){
    return(Local_List_Countries[which(Local_List_Countries$ContinentName == selectedContinent),])
  } else if (selectedDataSource == 'owid'){
    return(List_Countries[which(List_Countries$ContinentName == selectedContinent),])
  }
}

get_CountryName_ByCountryCode = function(selectedDataSource, selectedCountryCode){
  if (selectedDataSource == 'local'){
    return(Local_List_Countries[which(Local_List_Countries$CountryCode == selectedCountryCode),]$CountryName)
  } else if (selectedDataSource == 'owid'){
    return(List_Countries[which(List_Countries$CountryCode == selectedCountryCode),]$CountryName)
  }
}

get_MinDate_ByCountryCode = function(selectedDataSource, selectedCountryCode){
  return(min(as.Date(get_Data_ByCountryCode(selectedDataSource, selectedCountryCode)$date)))
}

get_MaxDate_ByCountryCode = function(selectedDataSource, selectedCountryCode){
  return(max(as.Date(get_Data_ByCountryCode(selectedDataSource, selectedCountryCode)$date)))
}

get_Data_ByCountryCode = function(selectedDataSource, selectedCountryCode){
  if (selectedDataSource == 'local'){
    # Extract Data
    countryData = LocalDataSet[which(LocalDataSet$location==selectedCountryCode),]
  } else if (selectedDataSource == 'owid'){
    # Extract Data
    countryData = FullDataSet[[selectedCountryCode]][['data']]
  }
  # Add Date Field
  countryData$date_asdate = as.Date(countryData$date, "%Y-%m-%d")
  # Order By Date
  countryData[order(countryData$date_asdate),1]
  return(countryData)
}

get_ModelingData_ByCountryCodeAndDates = function(selectedDataSource, selectedCountryCode, selectedStart, selectedEnd, interestedY){
  # Get Data
  countryData = get_Data_ByCountryCode(selectedDataSource, selectedCountryCode)
  
  # Get Total Days * Prepare Dates
  StudyTimeFrame.Start = as.Date(selectedStart)
  StudyTimeFrame.End = as.Date(selectedEnd)
  StudyTimeFrame.TotalDays = as.numeric((StudyTimeFrame.End - StudyTimeFrame.Start), units="days")
  
  # Filter Data as per selected dates
  countryData = countryData[countryData$date_asdate >= StudyTimeFrame.Start & countryData$date_asdate <= StudyTimeFrame.End, ]
  
  # Generate Day Counter
  dayCounter = 1 : StudyTimeFrame.TotalDays
  
  # Generate Modeling Data 
  modelingData = data.frame(cbind("X" = dayCounter, "Y" = countryData[1:StudyTimeFrame.TotalDays,][interestedY]))
  colnames(modelingData)[1] = 'X'
  colnames(modelingData)[2] = 'Y'
  return(modelingData)
}

Text_InitialAnalysis = function(countryData, selectedStart, selectedEnd){
  initialAnalysisOutput = ''
  
  # Study Time-frame (Study Data)
  StudyTimeFrame.Start = as.Date(selectedStart)
  StudyTimeFrame.End = as.Date(selectedEnd)
  StudyTimeFrame.TotalDays = as.numeric((StudyTimeFrame.End - StudyTimeFrame.Start), units="days")
  initialAnalysisOutput = paste(initialAnalysisOutput, 
                                "The study timeframe starts from",
                                "<b>", StudyTimeFrame.Start, "</b>until<b>", StudyTimeFrame.End, "</b>",
                                "covering<b>", StudyTimeFrame.TotalDays ,"</b>days.<br/>")
  
  # First Reporting
  FirstReported.Case = min(countryData[which(countryData$total_cases!=0),]$date_asdate)
  FirstReported.Death = min(countryData[which(countryData$total_deaths!=0),]$date_asdate)
  initialAnalysisOutput = paste(initialAnalysisOutput, "The first case was reported on:<b>", FirstReported.Case, '</b><br/>')
  initialAnalysisOutput = paste(initialAnalysisOutput, "The first death was reported on:<b>", FirstReported.Death, '</b><br/>')
  
  # Last Reporting
  LastReported.TotalCases = countryData[which(countryData$date_asdate == StudyTimeFrame.End),]$total_cases
  LastReported.TotalDeaths = countryData[which(countryData$date_asdate == StudyTimeFrame.End),]$total_deaths
  initialAnalysisOutput = paste(initialAnalysisOutput,
                                "The total cumulative  number of cases by the last day in the selected period is<b>", LastReported.TotalCases, '</b> cases.<br/>')
  initialAnalysisOutput = paste(initialAnalysisOutput,
                                "The total number of deaths by the last day in the selected period is<b>", LastReported.TotalDeaths, '</b> deaths.<br/>')
  # Return Initial Analysis Output
  return(initialAnalysisOutput)
}

Plot_InitialAnalysis = function(countryName, countryData){
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
  InitialPlots.NewDeaths = ggplot(countryData, aes(x = date_asdate)) + ggtitle(paste('New deaths in', countryName)) +
    geom_area(aes(y = new_deaths), fill = "black", alpha = 0.2) +
    geom_line(aes(x = date_asdate, y = new_deaths), color = "black") +
    xlab("Date") +
    ylab("New Deaths")
  
  # Display Plots
  grid.arrange(InitialPlots.TotalCases, InitialPlots.NewCases,
               InitialPlots.TotalDeaths, InitialPlots.NewDeaths, nrow=2, ncol=2)
  
}