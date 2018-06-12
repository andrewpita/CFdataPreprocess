
#function that collects a list of files in the 
#directory and joins them all into one csv


compile_months = function() {
  
  files = list.files(pattern = ".csv$")
  
  n = files[1]
  
  year = substr(n,1,4)
  
  dat = read.csv(n, header = TRUE)
  
  for (i in 2:length(files)) {
    
    dat1 = read.csv(files[i], header = TRUE)
    
    dat = rbind(dat,dat1)
    
  }
  
  write.csv(dat, file = paste(year, ".csv", sep = ""), row.names = FALSE)
}