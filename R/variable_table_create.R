
library(dplyr)
library(lubridate)
library(geosphere)



input = read.csv("test.lat.long.csv", header = TRUE, sep = ",", 
                 colClasses = c("integer","character","numeric","numeric"))

dates = input[,"date"]

lats.longs = input[1,3:4]
longs.lats = rev(lats.longs)

variables = c("Tmax","Tmin","Tavg","DewPoint", "WetBulb", "Heat", 
              "Cool","PrecipTotal","StnPressure", "AvgSpeed","Max5Speed","Max2Speed")


#function that takes as input a variable of interest,
#numeric values of latitude and longitude, and a character
#year month day date (slashes or no slashes, both work)
  
variable_table_create = function(variable, longs.lats, date) {
  
  #take the character input and make a date
  #using lubridates function 
  d = ymd(date)
  
  #extract the year using another lubridate function.
  #Since we're looking for a window of 30 days before
  #first infection, we only need to read the csv containing
  #that years data and the previous year (in case the month is January,
  #and to leave the possiblity open for larger windows)
  
  year = year(d)
  preyear = year-1
  
  #except of course if the year is 2003, in which case
  #we only load that year because we are not using data from before
  #2003
  
  if (year == 2003) {
    
    csv = read.csv(paste(year,'.csv', sep = ""), header = TRUE, 
                    colClasses = "character")
  } else {
    
    #read the appropriate csv files
    
    csv1 = read.csv(paste(preyear,".csv",sep = ""), header = TRUE, 
                    colClasses = "character" )
    csv2 = read.csv(paste(year,'.csv', sep = ""), header = TRUE, 
                    colClasses = "character")
    
    csv = rbind(csv1, csv2)
    
    
  }

  
  #select only the variable of interest
  csv = select(csv,WBAN, Latitude, Longitude, YearMonthDay, variable)
  
  #create date objects from the character vector of dates in the csv
  csv$YearMonthDay = as.Date(csv$YearMonthDay, format = "%Y%m%d")

  #grab only observations that are within the 30 day window
  #and have latitudes and longitudes that are not missing
  
  csv = filter(csv, YearMonthDay > as.Date(date) - 30 &
                 YearMonthDay < as.Date(date), 
               Latitude != "M" & Longitude != "M")
  

  #converting lat and long to numeric
  
  csv$Latitude = as.numeric(csv$Latitude)
  csv$Longitude = as.numeric(csv$Longitude)
  
  #converting variable to numeric
  csv[,5] = as.numeric(csv[,5])
  
  ### begin block to find closest latitude and longitude 
  ### to input lat and long ####
  ######
  
  #the function takes input as longitude, latitude, 
  #so we reverse the order with rev
  csv.longs.lats = rev(csv[,2:3])
  
  #compute distance between first coordinate in the data
  #with the input coordinate
  
  distance = distm(longs.lats, csv.longs.lats[1,], 
                   fun = distHaversine)
  
  
  #we want the site with data that is a certain 
  #percent complete.  The while loop iterates
  #5 times.  If at any point it finds a WBAN
  #site that is over x% complete, it breaks
  
  cnt = 0
  while (cnt <= 4) {
    
    #to begin save the closest monitor site as the first site
    #(the current distance value is the distance
    #between the first monitoring site and the input 
    #coordinates)
    closest = csv$WBAN[1]
    
    
    #then we loop through the coordinates of all the sites
    #checking each time if its closer than the current closest
    #site
    for (i in 2:nrow(csv.longs.lats)) {
      
      distance.loop = distm(longs.lats, csv.longs.lats[i,],
                            fun = distHaversine)
      
      if (distance.loop < distance) {
        
        distance = distance.loop
        closest = csv$WBAN[i]
        
      }
      
    }
    
    #grab data from the closest site
    csv.data.filt = filter(csv, WBAN == as.numeric(closest))

    
    #calculate what percent of data at the site is missing
    pcnt = sum(is.na(csv.data.filt[,5]))/length(csv.data.filt[,1])
    
    #if more than 30% of the data is missing, we increment the 
    #cnt variable and filter out all the data for that site
    #then the while loop will run again with that site excluded
    if (pcnt > 0.3) {
      
      cnt = cnt + 1
      csv = filter(csv, WBAN != as.numeric(closest))
      
      #otherwise, if it's less than 30% missing we break the
      #loop and use the data from that site
    } else {
      
      cnt = cnt + 5
    }
  
  }
  
  
  #mean of variable for closest station in that
  #window of time
  avg = mean(csv.data.filt[,5],na.rm = TRUE)
  #return the mean
  return(avg)
  
}



#nested loops that iterate through each variable and 
#date in the csv file 

#for each variable
for (j in 1:length(variables)) {
  
  #make an empty vector to store the average
  #value for a given input date
  avg.vec = rep(NA, length(dates))
  
  #then for each variabiable, loop through 
  #each input date.  For each date 
  for (i in 1:length(dates)) {
    
    #grab the input coordinates associated with that subject
    lats.longs = input[i,3:4]
    
    #reverse them
    longs.lats = rev(lats.longs)
    
    #run the function that returns the average value
    #of that variable at the closest site
    mlist = variable_table_create(variables[j], longs.lats, dates[i])
    
    #add it to our vector
    avg.vec[i] = mlist[2]
    
    
  }
  
  #append that column to the input csv so that 
  #the average value for a given variable is 
  #adjacent to its input date and location
  input = cbind(input, avg.vec)
  #name the column
  colnames(input)[4 + j] = variables[j]
  
}

#finally output a csv that has the average for all variables
write.csv(input, file = "output.csv", row.names = FALSE)


