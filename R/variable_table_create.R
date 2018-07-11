
library(dplyr)
library(lubridate)

#function that takes as input a variable of interest,
#numeric values of latitude and longitude, and a character
#year month day date (slashes or no slashes, both work)

variable_table_create = function(variable, lat, long, date) {
  
  #take the character input and make a date
  #using lubridates function 
  d = ymd(date)
  
  #extract the year using another lubridate function.
  #Since we're looking for a window of 30 days before
  #first infection, we only need to read the csv containing
  #that years data
  year = year(d)
  
  #read the appropriate csv file
  
  csv = read.csv(paste(year,".csv",sep = ""), header = TRUE, colClasses = "character" )
  
  #select only the variable of interest
  csv = select(csv, Latitude, Longitude, YearMonthDay, variable)
  
  #create date objects from the character vector of dates in the 
  #csv. We use ifelse to account for the missing data
  csv$YearMonthDay = ifelse(csv$YearMonthDay == "M", NA,
                            as.Date(csv$YearMonthDay, format = "%Y%m%d"))
  
  #converting character latitude to numeric
  csv$Latitude = ifelse(csv$Latitude == "M", "M",as.numeric(csv$Latitude))
  
  csv$Longitude = ifelse(csv$Longitude == "M", "M", as.numeric(csv$Longitude))
  
  #grab data that matches our criteria
  csv = filter(csv, Latitude == lat & Longitude == long &
                    YearMonthDay > as.Date(date) - 30 &
                    YearMonthDay < as.Date(date))
  
  #this is the total number of data matching the criteria
  total = length(csv[,1])
  #total number missing of the data that matches the criteria
  miss = sum(csv[,4] == "M")
  #percent missing
  percent = miss/total
  
  #store and name the quantities above
  var.mat = c(total,miss,percent)
  names(var.mat) = c("Total Match", "Missing", "Percent")
  
  return(var.mat)
  
}


#tmax = variable_table_create("Tmax", 38.04, -102.41, "2005-01-29")





