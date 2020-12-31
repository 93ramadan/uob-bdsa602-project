#************************************************************#
#*                BDSA 602 - Statistical Analysis
#*                      Project
#*                
#*  Team Members:
#*    1. Aisha Khalid (20092905)
#*    2. Ahmed Khedr (20113798)
#*    
#************************************************************#

source("V2/0.Imports.R")
source("V2/0.GlobalDefinitions.R")
source("V2/1.DataLoad.R")

source("V2/ui.R")
source("V2/server.R")
shinyApp(ui = ui, server = server)


source("2.DataPrep.R")
source("3.BasicInitialAnalysis.R")
source("3.1.BasicInitialAnalysisPlots.R")
source("4.ModelBuilding.R")
source("5.ModelPrediction.R")
source("5.1.ModelPredictionPlots.R")
source("6.ResearchFactors.R")
source("6.1.ResearchFactorsPlots.R")


c(List_Countries$CountryCode = List_Countries$CountryName)

(List_Countries$CountryCode)

my_new_list <- with(List_Countries, split(CountryCode, CountryName))
