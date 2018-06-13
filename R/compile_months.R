
#function that collects a list of csv files in a 
#directory and joins them all into one csv.
#In the context of the other functions this will be
#a folder that contains the csv file of the data
#for each month in a year. 

compile_months = function() {
  
  #a vector containing the name of each csv file in
  #the current directory
  
  files = list.files(pattern = ".csv$")
  
  #the name of the first file. We want this
  #just to create a character object of the year
  #in which we are working.
  
  n = files[1]
  
  #get the year
  
  year = substr(n,1,4)
  
  #read in the first month of data
  
  dat = read.csv(n, header = TRUE)
  
  #we start looping throug the files from the second
  #file name, because we've already loaded the first.
  #On each iteration we load a csv file, then join it
  #to either the first month, or the collection of
  #previous months.
  
  for (i in 2:length(files)) {
    
    dat1 = read.csv(files[i], header = TRUE)
    
    dat = rbind(dat,dat1)
    
  }
  
  #output a new csv file containing a years worth of data
  #
  write.csv(dat, file = paste(year, ".csv", sep = ""), row.names = FALSE)
}