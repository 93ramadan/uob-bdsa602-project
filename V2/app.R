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