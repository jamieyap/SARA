# About File: -----------------------------------------------------------------
# file-check-utils.R contains functions to compare different files
# and sets of files between different folders

GetCommonPersonDays <- function(path.availability, map.long){
  # GetCommonPersonDays() lists down all unique person days
  # across all files in path.availability and then tabulates 
  # the number of files having a given person day in common
  # in the column in.more.dfs
  
  list.all.in.path <- SimpleReadAll(path.to.files = path.availability, format.date=TRUE, current.format = "%m/%d/%Y")
  result <- lapply(list.all.in.path, GetWithinStudyPeriod, map.long = map.long)
  result <- do.call(cbind,result)
  result <- cbind(result[,1:2], result[, grep("in.df", colnames(result))])
  result$in.more.dfs <- rowSums(result[,3:ncol(result)])
  
  return(result)
}


