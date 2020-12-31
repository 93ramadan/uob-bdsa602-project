#************************************************************#
#*                BDSA 601 - Research Methods
#*                Assignment 3 - Project
#*                
#*  Team Members:
#*    1. Aisha Khalid (20092905)
#*    2. Ahmed Khedr (20113798)
#*    
#************************************************************#

# Define Research Scope
ResearchScope.NumberofDays = 90

# Define Research Assumptions
ResearchAssumes.Hospital.BedOccupancyRate = 0.8 # Obtained from verbal communication with health care worker
ResearchAssumes.IQC.TotalBeds = (4257+5489) # Obtained from Published Figures (Al Arabiya English, 13-May-2020)
ResearchAssumes.IQC.BedOccupancyRate = (3218+533) / (4257+5489) # Obtained from Published Figures (Al Arabiya English, 13-May-2020)
ResearchAssumes.Current.ActiveCases = 2832 # Obtained from MOH (01-Aug-2020)
ResearchAssumes.Current.CasesUnderTreatment = 81 # Obtained from MOH (01-Aug-2020)

source("0.Imports.R")
source("1.DataLoad.R")
source("2.DataPrep.R")
source("3.BasicInitialAnalysis.R")
source("3.1.BasicInitialAnalysisPlots.R")
source("4.ModelBuilding.R")
source("5.ModelPrediction.R")
source("5.1.ModelPredictionPlots.R")
source("6.ResearchFactors.R")
source("6.1.ResearchFactorsPlots.R")
