
#first we source the functions in the two scripts 
#below. They need to be in the same directory as this
#script. All three of this scripts should be in a folder
#along with a folder for each year that contains 12 folders,
#each containing one months worth of raw data. 

source("month_create.R")
source("compile_months.R")

#a vector containing the name of each directory in this 
#folder.

top.dirs = dir()

#loop through each of these directories. On each iteration
#we get a vector containing the names of the directories in 
#these folders (one for each month). We move into one of 
#directories, create a directory one up from our original
#directory called "year"_csv (sub the current year) to store
#the 12 csv files that will be created, one for each month. 

for (i in 1:length(top.dirs)) {
  
  setwd(top.dirs[i])
  
  sub.dirs = dir()
  
  year = substr(top.dirs[i],1,4)
  
  dir.create(paste("../../",year,"_csv", sep = ""))
  
  #on each iteration of the above loop, we
  #iterate through each month directory,
  #grab the data we need, and output it to a csv
  #file that is stored in the "year"_csv file
  #located one folder above our starting folder
  
  for (j in 1:length(sub.dirs)) {
    
    setwd(sub.dirs[j])
    
    #a character object containing the name of the
    #the daily.txt file
    daily = list.files(pattern = "daily.txt$")
    
    station = list.files(pattern = "station.txt$")
    
    #the if and else statements ensure that we apply
    #the correct function to the data. In each case
    #month.list is a list of two data frames
    
    if (as.numeric(year) > 2007) {
      
      month.list = month_post2007(daily)
      
    }
    else {
      
      if (as.numeric(year) == 2007 && as.numeric(substr(daily,5,6)) > 04) {
        
        month.list = month_post2007(daily)
      }
      else {
        
        month.list = month_pre2007(daily)
      }
    }
    
    #merge the two dataframes in month.list,
    #and write it to a csv file
    
    month_create(month.list)
    
    #this is the name of the csv file we just created
    
    csv.file = list.files(pattern = ".csv$")
    
    #we copy the csv file we just created to its "year"_csv file
    
    file.copy(csv.file, paste("../../../",year,"_csv", sep = ""))
    
    #move back one directory to continue onto the next month
    
    setwd("..")
    
  }
  
  #move back one directory to continue onto the next year
  
  setwd("..")
  
}

