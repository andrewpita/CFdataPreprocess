


#month_post2007 takes as input a list of the files
#in the directory and returns a list of 2 dataframes.
#the first is the subsetted data from the daily.txt file
#that we are interested in. The second is just the id
#column and the latiutude and longitude of that location.

#there is a change in the column names of the daily.txt
#files after the year 2007. The year 2007 itself is split, 
#months 1-6 have the column organization of years pre 2007. 
#This is why there are two functions, one for each of the 
#dataset types. The other function is defined below this one,
#called month_pre2007.

month_post2007 = function(daily) {
  
  #The input daily is the name of the daily.txt file
  #for that month. We reed it in with everything as a 
  #character.
  daily.dat = read.csv(daily, header = TRUE, sep = ",",
                       colClasses = rep("character",50))
  
  #The station.txt file is pipe delimitted. We read it in here.
  
  station.dat = read.delim(station, header = TRUE, sep = "|")
  
  #these are the columns needed in the analysis. We subset them
  #using dplyr's select function.
  
  daily.subset = dplyr::select(daily.dat, WBAN, YearMonthDay,Tmax,
                        Tmin,Tavg,DewPoint, WetBulb, 
                        Heat, Cool,PrecipTotal,StnPressure, 
                        AvgSpeed, Max5Speed,Max2Speed)
  
  #using the same function we grab just the id column (WBAN) and latitude
  #and longitude.
  
  wban.lat.long = dplyr::select(station.dat, WBAN, Latitude, Longitude)
    
  return(list(daily.subset, wban.lat.long))
  
  
}

#the pre2007 data has precipitation data in two colums, whereas
#the post2007 has the data in one column. To add these columns
#we need to import the data as numeric, which is the reason for the
#extensive colClasses argument. The na.strings are the various ways
#that na values are encoded in the preciptiation columns.

#this function works in much the same way as the post2007 function.

month_pre2007 = function(daily) {
  
  #daily is again the daily.txt file. We read it in here.
  
  daily.dat = read.table(daily, header = TRUE, sep = ",",colClasses = 
                           c("numeric","character","character","character","character",
                             "character","character","character","character",
                             "character","character","character","character","numeric",
                             "numeric","character","character","character","character",
                             "character","character","character","character","character"),
                         na.strings = c("M","T","-","sETE","****","E","P"))
  
  # reading in the station.txt pipe delimitted file.
  
  station.dat = read.delim(station, header = TRUE, sep = "|")
  
  #subsetting the columns needed for the analysis
  
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


#we want to preserve the 'M' coding for missing values and then
#merge the daily data to the latitude and longitude data

month_create = function(data.list) {
  
  #data.list is the output of the two functions above, a list of two
  #data frames. We merge the two items in the list on the id columns
  #WBAN
  
  data.complete = merge(data.list[[2]], data.list[[1]], by = "WBAN", all = TRUE)
  
  
  #we loop through the columns, and replace all
  #NAs with Ms
  
  for (i in 1:length(colnames(data.complete))) {
    
    data.complete[,i][is.na(data.complete[,i])] = "M"
    
  }
  
  #write the merged data set to a csv in the same folder
  #as the daily.txt and station.txt files
  write.csv(data.complete, file = paste(substr(daily,1,6), ".csv", sep = ""), row.names = FALSE)
}







