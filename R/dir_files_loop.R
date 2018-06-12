
source("month_create.R")
source("compile_months.R")


top.dirs = dir()

for (i in 1:length(top.dirs)) {
  
  setwd(top.dirs[i])
  
  sub.dirs = dir()
  
  year = substr(top.dirs[i],1,4)
  
  dir.create(paste("../../",year,"_csv", sep = ""))
  
  
  for (j in 1:length(sub.dirs)) {
    
    setwd(sub.dirs[j])
    
    daily = list.files(pattern = "daily.txt$")
    
    station = list.files(pattern = "station.txt$")
    
    if (as.numeric(year) > 2007) {
      
      month.list = month_post2007(daily)
      
    }
    else {
      
      if (as.numeric(year) == 2007 && as.numeric(substr(daily,5,6)) > 07) {
        
        month.list = month_post2007(daily)
      }
      else {
        
        month.list = month_pre2007(daily)
      }
    }
    
    month_create(month.list)
    
    csv.file = list.files(pattern = ".csv$")
    
    file.copy(csv.file, paste("../../../",year,"_csv", sep = ""))
    
    setwd("..")
    
  }
  
  setwd("..")
  
}

