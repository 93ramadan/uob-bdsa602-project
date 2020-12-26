#*******************************************************
#* Comparesion of MSE of Each Model for Each Country
#*******************************************************
#*
Models.Error = NULL
Days = c(1:50)

  Models.Error = data.frame(Models.Error)
  Models.Error$country = listOfCountries
  #rownames(Models.Error) <- NULL
  Models.Error$LR = Models.GLR.MSE
  Models.Error$SplineCubic
  Models.Error$SplineNatural
  Models.Error$SmoothingSpline
attach(Models.Error) 
  Models.Error$BestModel = Models.Error$SmoothingSpline
for (i in 1:50)
  {
  LR = as.double(Models.Error$LR[i])
  SC = as.double(Models.Error$SplineCubic[i])
  SN = as.double(Models.Error$SplineNatural[i])
  SS = as.double(Models.Error$SmoothingSpline[i])
  Models.Error$BestModel[i] = BestModelFuntion(LR, SC, SN, SS)
  }
fix(Models.Error)
result = BestModelFuntion(83,45,77,32)

BestModelFuntion <- function(LR, SC, SN, SS)
  {
  result = NULL
  if (min(LR, SC, SN, SS) == LR )
  {result = "Linaer Regression"
  print("LR")
  }
  else if( min(LR, SC, SN, SS) == SC )
  {result = "Cubic Spline"
  print("CS")
  }
  else if (min(LR, SC, SN, SS) == SN )
  {result = "Natural Spline"
  print("NS")
  }
  else if (min(LR, SC, SN, SS) == SS )
  {result = "Smoothing Spline"
  print("SS")
  }
  else result = "UNKNOWN"
  return(result)
  }
