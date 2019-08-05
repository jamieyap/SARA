# About File: -----------------------------------------------------------------
# io-utils.R contains functions to perform input/output tasks

ReadAll <- function(path.to.files, CleanData = NULL, ArgsList = NULL){
  # ReadAll() reads all files in a given working directory without having to 
  # assign a name for each file and provides flexibility for the user to
  # specify CleanData(), a function to perform a common data manipulation
  # task to all files in path.to.files, ArgsList() are inputs to CleanData()
  # in a list format
  
  current.working.directory <- getwd()
  on.exit(setwd(current.working.directory))
  
  setwd(path.to.files)
  files <- list.files()
  
  data.list <- lapply(files, function(f, CleanDataFUN = CleanData, UseArgsList = ArgsList){
    # read.csv() will throw an error when a given file is 0KB
    # try() allows this function to continue evaluation on the remaining items
    # on the list of file names even when read.csv() throws an error
    # read.csv() will throw an error if file is 0KB
    
    f1 <- try(read.csv(file.path(f)), silent=TRUE)  # f1 is a data frame 
    
    if(class(f1) == "try-error"){ # takes care of 0KB files
      return(NULL)
    }else{ # when file is >0KB
      # Peform custom data manipulation here if CleanData() is provided
      if(!is.null(CleanDataFUN)){
        if(!is.null(UseArgsList)){
          f1 <- CleanDataFUN(f1, UseArgsList)
          f1 <- unique(f1)  # takes care of duplicated rows within f1
        }else{
          f1 <- CleanDataFUN(f1)
          f1 <- unique(f1)  # takes care of duplicated rows within f1
        }
      }
      return(f1)
    }
  } # end function()
  ) # end lapply()
  
  # Participants whose files are 0KB will not be included in data
  data <- do.call(rbind, data.list)
  data <- as.data.frame(data)
  data <- unique(data)  # takes care of duplicated rows across different files
  return(data)
} # end ReadAll

SimpleReadAll <- function(path.to.files, format.date = FALSE, current.format = NULL){
  # SimpleReadAll() is similar to ReadAll() but specifically capable of just two tasks.
  # This function reads all files in a given working directory without having to 
  # assign a name for each file and
  # EITHER (1) If format.date==TRUE: changes format of calendar_date from 
  # current.format into %m/%d/%Y format
  # OR (2) If format.true==FALSE: does not perform any data manipulation tasks
  
  # Note that (2) can also be accomplished by ReadAll() through simply providing
  # path.to.files and keeping the CleanData and ArgsList arguments as NULL
  # However, SimpleReadAll() would return a list while ReadAll() would return
  # a data frame
  
  current.working.directory <- getwd()
  on.exit(setwd(current.working.directory))
  
  setwd(path.to.files)
  files <- list.files()
  
  if(format.date == TRUE){
    data.list <- lapply(files, function(f){
      f1 <- try(read.csv(file.path(f)), silent=TRUE)
      
      if(class(f1) == "try-error"){ # takes care of 0KB files
        return(NULL)
      }else{
        # This function expects to read data with a calendar_date column
        # Throw an error if f1 does not have a column called calendar_date
        stopifnot("calendar_date" %in% colnames(f1))
        
        # Proceed with changing column format
        f1$calendar_date <- strptime(f1$calendar_date, format = current.format, tz="EST5EDT")
        f1$calendar_date <- strftime(f1$calendar_date, format = "%m/%d/%Y", tz="EST5EDT")
        return(f1)
      }
    } # end function()
    ) # end lapply()
  }else{
    data.list <- lapply(files, function(f){
      f1 <- try(read.csv(file.path(f)), silent=TRUE)
      if(class(f1) == "try-error"){ # takes care of 0KB files
        return(NULL)
      }else{
        return(f1) 
      }
    } # end function()
    ) # end lapply()
  }
  
  names(data.list) <- files
  return(data.list)
}
