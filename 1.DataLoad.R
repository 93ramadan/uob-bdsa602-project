#************************************************************#
#*  Load Full Data Set from Our World In Data (JSON)
#************************************************************#

FullDataSet = fromJSON('https://covid.ourworldindata.org/data/owid-covid-data.json')
LocalDataSet = read.csv('COVID19DATA.csv')

#************************************************************#
#*  Load Continents and Countries
#************************************************************#

for (CountryCode in names(FullDataSet)) {
  ContinentName = FullDataSet[[CountryCode]][['continent']]
  CountryName = FullDataSet[[CountryCode]][['location']]
  if (!is.null(ContinentName) & !is.null(CountryName)){
    # Append Continent
    List_Continents = rbind(List_Continents, data.frame(ContinentName))
    # Ensure Unique
    List_Continents = unique(List_Continents)
    # Append Country
    List_Countries = rbind(List_Countries, data.frame(ContinentName, CountryCode, CountryName))
    # Ensure Unique
    List_Countries = unique(List_Countries)
  }
}

Local_List_Continents = unique(LocalDataSet$continent)
Local_List_Countries = unique(rbind(data.frame("ContinentName" = LocalDataSet$continent, "CountryCode" = LocalDataSet$location, "CountryName" = LocalDataSet$location)))
row.names(Local_List_Continents) = NULL
row.names(Local_List_Countries) = NULL

# Sort
List_Continents = arrange(List_Continents, (ContinentName))
List_Countries = arrange(List_Countries, (CountryName))

Local_List_Continents = sort(Local_List_Continents)
Local_List_Countries = arrange(Local_List_Countries, (CountryName))