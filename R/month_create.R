


#the function takes as input a list of the files
#in the directory

month_post2007 = function(daily) {
  
  #there is a change in the column names of files after the year
  #2007. The year 2007 itself is split, months 1-6 have the column
  #organization of years pre 2007
  
  daily.dat = read.csv(daily, header = TRUE, sep = ",",
                       colClasses = rep("character",50))
  
  station.dat = read.delim(station, header = TRUE, sep = "|")
  
  #these are the columns needed in the analysis
  daily.subset = dplyr::select(daily.dat, WBAN, YearMonthDay,Tmax,
                        Tmin,Tavg,DewPoint, WetBulb, 
                        Heat, Cool,PrecipTotal,StnPressure, 
                        AvgSpeed, Max5Speed,Max2Speed)
  
  wban.lat.long = dplyr::select(station.dat, WBAN, Latitude, Longitude)
    
  return(list(daily.subset, wban.lat.long))
  
  
}


month_pre2007 = function(daily) {
  
  #the pre2007 data has precipitation data in two colums, whereas
  #the post2007 has the data in one column. To add these columns
  #we need to import the data as numeric, which is the reason for the
  #extensive colClasses argument. The na.strings are the various ways
  #that na values are encoded
  
  daily.dat = read.table(daily, header = TRUE, sep = ",",colClasses = 
                           c("numeric","character","character","character","character",
                             "character","character","character","character",
                             "character","character","character","character","numeric",
                             "numeric","character","character","character","character",
                             "character","character","character","character","character"),
                         na.strings = c("M","T","-","sETE","****"))
  
  station.dat = read.delim(station, header = TRUE, sep = "|")
  
  #columns needed for the analysis
  daily.subset = dplyr::select(daily.dat, Wban.Number, YearMonthDay,Max.Temp,
                        Min.Temp,Avg.Temp,Avg.Dew.Pt, Avg.Wet.Bulb, 
                        Heating.Degree.Days, Cooling.Degree.Days, 
                        Precipitation.Snowfall,Precipitation.Water.Equiv, 
                        Pressue.Avg.Station, Wind.Avg.Speed, Max.5.sec.speed, 
                        Max.2.min.speed)
  
  
  #columns needed for the analysis. First we check for NA values in the
  #precipitation.snowfall column. If precipitation.snowfall is NA, the 
  #total is set to the water.equiv column
  
  daily.subset$PrecipTotal = ifelse(is.na(daily.dat$Precipitation.Snowfall),
                                    daily.dat$Precipitation.Water.Equiv, 
                                    daily.dat$Precipitation.Snowfall + 
                                      daily.dat$Precipitation.Water.Equiv)
  
  #if water.equiv is NA and snowfall isn't we want the total column
  #to take the snowfall column value
  daily.subset$PrecipTotal = ifelse(is.na(daily.dat$Precipitation.Water.Equiv),
                                    daily.dat$Precipitation.Snowfall,
                                    daily.subset$PrecipTotal)
  
  #dropping the two precipitation columns
  daily.subset = within(daily.subset, rm(Precipitation.Snowfall, Precipitation.Water.Equiv))
  
  #renaming the columns so that they match the post2007 column names
  colnames(daily.subset) = c("WBAN", "YearMonthDay","Tmax",
                             "Tmin","Tavg","DewPoint","WetBulb", 
                             "Heat","Cool","PrecipTotal","StnPressure", 
                             "AvgSpeed", "Max5Speed","Max2Speed")
  
  #we need three columns from the station.txt file
  wban.lat.long = dplyr::select(station.dat, WBAN.Number, Latitude, Longitude)
  
  #changing the wban column name to match the post2007 format
  names(wban.lat.long)[1] = "WBAN"
  
  return(list(daily.subset, wban.lat.long))
  
  
}


month_create = function(data.list) {
  
  
  data.complete = merge(data.list[[2]], data.list[[1]], by = "WBAN", all = TRUE)
  
  for (i in 1:length(colnames(data.complete))) {
    
    data.complete[,i][is.na(data.complete[,i])] = "M"
    
  }
  
  write.csv(data.complete, file = paste(substr(daily,1,6), ".csv", sep = ""), row.names = FALSE)
}







