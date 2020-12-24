#************************************************************#
#*  Reading/Loading data
#************************************************************#
COVID19.Full = read.csv('COVID19DATA_EUROPE.csv')

#Extract only the columns (Country, date, new cases and total cases)
COVID19 = COVID19.Full[,2:5] 

#View the data
fix(COVID19)

