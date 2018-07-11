
library(dplyr)
library(lubridate)

variable_table_create = function(variable, lat, long, date) {
  
  d = ymd(date)
  year = year(d)
  
  #files = list.files(pattern = ".csv$")
  #n = files[1]
  #year = substr(n, 1,4)
  
  csv = read.csv(paste(year,".csv",sep = ""), header = TRUE, colClasses = "character" )
  
  csv = select(csv, Latitude, Longitude, YearMonthDay, Tmax)
  
  
  csv$YearMonthDay = ifelse(csv$YearMonthDay == "M", NA,
                            as.Date(csv$YearMonthDay, format = "%Y%m%d"))
  
  csv$Latitude = ifelse(csv$Latitude == "M", "M",as.numeric(csv$Latitude))
  
  csv$Longitude = ifelse(csv$Longitude == "M", "M", as.numeric(csv$Longitude))
  
  csv = filter(csv, Latitude == lat & Longitude == long &
                    YearMonthDay > as.Date(date) - 10 &
                    YearMonthDay < as.Date(date))
  
  total = length(csv[,1])
  miss = csv[,4] == "M"
  percent = miss/total
  
  var.mat = c(total,miss,percent)
  names(var.mat) = c("Total Match", "Missing", "Percent")
  
  for (i in 2:length(files)) {
    
    csv.loop = read.csv(files[i], header = TRUE)
    
    csv.loop = select(csv.loop, Latitude, Longitude, YearMonthDay, 
                      variable)
    
    csv.loop$YearMonthDay = as.Date(as.character(csv$YearMonthDay), 
                                    format = "%Y%m%d")
    
    csv.loop$Latitude = as.numeric(as.character(csv.loop$Latitude))
    
    csv.loop$Longitude = as.numeric(as.character(csv.loop$Longitude))
    
    
    csv.loop = filter(csv.loop, Latitude == lat & Longitude == long &
                      YearMonthDay > as.Date(date) - 10 &
                      YearMonthDay < as.Date(date))
    
    total = length(csv.loop[,1])
    miss = sum(csv.loop[,4] == "M")
    percent = miss/total
    
    v = c(total, miss, percent)
    
    
    var.mat = rbind(var.mat, v)
  }
  
  
  #write.csv(csv, file = paste(variable,".csv", sep = ""), row.names = FALSE)
  return(var.mat)
  
}


check_location_missing = function(csv, lat, long, date) {
  
  csv = filter(csv, Latitude == lat, Longitude == long, YearMonthDay > date - 30)
  
  count = csv[,3] == "M"
  
  percent = sum(count)/length(count)
  
  print(percent)
  
  
}


tmax = variable_table_create("Tmax", 38.04, -102.41, "2005-01-29")


head(tmax)




