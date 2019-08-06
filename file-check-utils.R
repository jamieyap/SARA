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

SummariseAll <- function(path, map.long){
  list.all.in.path <- SimpleReadAll(path, format.date = TRUE, current.format = "%m/%d/%Y")
  list.all.in.path <- lapply(list.all.in.path, TabulatePersonDays, map.long = map.long)
  df.all.in.path <- as.data.frame(do.call(rbind,list.all.in.path))
  df.all.in.path$filenames <- row.names(df.all.in.path)
  df.all.in.path <- df.all.in.path[,order(colnames(df.all.in.path))]
  row.names(df.all.in.path) <- 1:nrow(df.all.in.path)
  return(df.all.in.path)
}

TabulatePersonDays <- function(df, map.long){
  subset.df <- GetWithinStudyPeriod(df = df, map.long = map.long)
  subset.df <- subset.df[subset.df$in.df == 1,]
  result <- list(nparticipants = length(unique(subset.df$username)), npersondays = nrow(subset.df))
  return(result)
}

PrintKableTable <- function(output.SummariseAll, mycaption, mygenfootnote = NULL){
  total.output.SummariseAll <- t(cbind(c("Total",
                                         sum(as.data.frame(output.SummariseAll$nparticipants)),
                                         sum(as.data.frame(output.SummariseAll$npersondays))
  )
  )
  )
  total.output.SummariseAll <- as.data.frame(total.output.SummariseAll)
  colnames(total.output.SummariseAll) <- colnames(output.SummariseAll)
  output.SummariseAll <- rbind(output.SummariseAll, total.output.SummariseAll)
  
  kable(output.SummariseAll, format = "latex", caption = mycaption,
        align = c('c', 'c', 'c'),
        col.names = c("File Name","No. of Participants","No. of Participant Days"),
        row.names = FALSE, booktabs = TRUE) %>% 
    kable_styling(full_width = FALSE, latex_options = c("HOLD_position")) %>%
    row_spec(nrow(output.SummariseAll)-1, hline_after = TRUE)  %>%
    footnote(general = c(mygenfootnote), 
             threeparttable = TRUE)
}
