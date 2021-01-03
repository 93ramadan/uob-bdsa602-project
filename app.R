#************************************************************#
#*                BDSA 602 - Statistical Analysis
#*                      Project
#*                
#*  Supervised By
#*    Dr. Sawsan Hilal
#*    
#*    
#*  Team Members:
#*    1. Aisha Khalid (20092905)
#*    2. Ahmed Khedr (20113798)
#*    
#************************************************************#

source("santaR.R")
source("0.Imports.R")
source("0.GlobalDefinitions.R")
source("1.DataLoad.R")

source("ui.R")
source("server.R")
shinyApp(ui = ui, server = server)