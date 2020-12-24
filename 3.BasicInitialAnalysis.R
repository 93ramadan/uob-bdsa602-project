#************************************************************#
#*  Basic Initial Analysis of Data Figures (Cases & Deaths)
#************************************************************#

# Study Time-frame (Study Data)
StudyTimeFrame.Start = min(bahrain$date_asdate)
StudyTimeFrame.End = max(bahrain$date_asdate)
StudyTimeFrame.TotalDays = as.numeric((StudyTimeFrame.End - StudyTimeFrame.Start), units="days")

print(paste("The study time frame starts from", StudyTimeFrame.Start,
            "until", StudyTimeFrame.End,
            "covering", StudyTimeFrame.TotalDays ,"days."))

# First Reporting (Case vs. Death)
FirstReported.Case = min(bahrain[which(bahrain$total_cases!=0),]$date_asdate)
FirstReported.Death = min(bahrain[which(bahrain$total_deaths!=0),]$date_asdate)
print(paste("The first case was reported on:", FirstReported.Case))
print(paste("The first death was reported on:", FirstReported.Death))

# Last Reporting (Case vs. Death)
LastReported.TotalCases = bahrain[which(bahrain$date_asdate == StudyTimeFrame.End),]$total_cases
LastReported.TotalDeaths = bahrain[which(bahrain$date_asdate == StudyTimeFrame.End),]$total_deaths
print(paste("The total number of cases by the last day covered in the study:", LastReported.TotalCases))
print(paste("The total number of deaths by the last day covered in the study:", LastReported.TotalDeaths))