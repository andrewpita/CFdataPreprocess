

#function that takes no input and outputs a csv
#file containing the data for a whole year

make_year = function() {
  
  #a vector of the names of all the csv files
  files = list.files(pattern = ".csv$")
  
  #the first file
  n = files[1]
  
  #the year that all the month csv files belong to
  year = substr(n, 1,4)
  
  #reading the first csv. In the loop we append each month
  #to this file
  dat1 = read.csv(files[1], header = TRUE)
  
  for (i in 2:length(files)) {
    
    #reading in the remaining months, one at each
    #iteration
    dat = read.csv(files[i], header = TRUE)
    
    #appending each iterations month to the 
    #months that have already been appended
    dat1 = rbind(dat1, dat)
  }
  
  #writing the complete year csv file
  write.csv(dat1, file = paste(year,".csv", sep = ""), row.names = FALSE)
  
  return(year)
  
}


#here we actually loop through all the year directories
#that contain the month data. In each one we run the make_year script
#then copy it to the year_csv folder

top.dirs = dir()

#since all the directories start with "20" I use
#that to make sure we only go into directories that 
#we need to go into

top.dirs = top.dirs[grep("20", top.dirs)]

dir.create("year_csv")

#for each directory
for (i in 1:length(top.dirs)) {
  
  #move into that directory
  setwd(top.dirs[i])
  
  #run the make year script. In addition to the 
  #csv file it returns the current year, so that 
  #we can copy the file
  year.name = make_year()
  
  #mv the file to the year_csv folder
  file.copy(paste(year.name,".csv",sep = ""), paste("../","year_csv", sep = ""))
  
  #move back one directory to repeat
  setwd("../")
  
}

