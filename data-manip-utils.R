# About File: -----------------------------------------------------------------
# data-manip-utils.R contains functions to perform data manipulation tasks

LongMap <- function(map.wide, MyFUN = NULL, ArgsList = NULL){
  # LongMap() reshapes map.wide from wide format into long format
  # map.wide is a table where each row corresponds to a participant in the study
  # Columns correspond to participant ID's and dates corresponding to each day of
  # the study. Participants were recruited at different dates. Hence, Day 1 for
  # any two participants are not necessarily on the same date
  # 
  # Data clean up tasks in this function are specific to a map.wide file
  # However, there is flexibility in specifying data manipulation tasks
  # after data clean up tasks through MyFUN and ArgsList
  # 
  # ArgsList are arguments to MyFUN provided as a list
  # e.g. ArgsList = list(arg1 = arg1_value, arg2 = arg2_value, arg3 = arg3_value, ...)
  
  # ---------------------------------------------------------------------------
  # Data clean up tasks
  # ---------------------------------------------------------------------------
  
  # The first row of this data frame is blank. Therefore, we delete it
  map.wide <- map.wide[-1,]
  
  # "Day 1" in the data analysis correspond to the first full day after
  # recruitment. However, "Day 1" in map.wide corresponds to day of recruitment
  map.wide <- map.wide[,-2]
  colnames(map.wide) <- c("username",paste("study_day.", 1:29, sep=""))
  
  # Reshape map data frame from wide format to long format
  map.long <- reshape(map.wide,
                      idvar = "username",
                      direction = "long",
                      varying = paste("study_day.", 1:29, sep=""),
                      v.names = "calendar_date",
                      timevar = "study_day",
                      times = 1:29)
  map.long <- map.long[order(map.long$username, map.long$study_day),]
  row.names(map.long) <- 1:(29*nrow(map.wide))
  map.long$calendar_date <- as.character(map.long$calendar_date)
  map.long$calendar_date <- strptime(map.long$calendar_date, format = "%m/%d/%Y", tz="EST5EDT")
  map.long$calendar_date <- strftime(map.long$calendar_date, format = "%m/%d/%Y", tz="EST5EDT")
  
  # ---------------------------------------------------------------------------
  # Data manipulation tasks
  # ---------------------------------------------------------------------------
  
  # At this point, map.long has just three columns: 
  # username, study_day, calendar_date
  # If MyFUN is provided, it will be evaluated here
  if(!is.null(MyFUN)){
    if(!is.null(ArgsList)){
      map.long <- MyFUN(map.long, ArgsList)
    }else{
      map.long <- MyFUN(map.long) 
    }
  }
  
  return(map.long)
}

DropAll <- function(data.all, data.to.exclude){
  # DropAll() expects data.all and data.to.exclude to both contain columns
  # username and study_day. This function excludes a set of participant-days 
  # specified in data.to.exclude from data.all
  
  stopifnot(c("username","study_day") %in% colnames(data.all),
            c("username","study_day") %in% colnames(data.to.exclude))
  
  data.to.exclude$exclude <- 1
  data.all <- merge(data.all, data.to.exclude, by = c("username","study_day"), all.x = TRUE, all.y = FALSE)
  data.all <- data.all[is.na(data.all$exclude),]
  data.all <- data.all[!(colnames(data.all) %in% "exclude")]
  
  return(data.all)
}

GetWithinStudyPeriod <- function(df, map.long){
  # GetWithinStudyPeriod checks whether rows in df correspond to data within
  # dates during the study period given by map.long
  # Both df and map.long must have the common columns username and calendar_date
  # Function returns a data frame with the same number of rows as map.long
  
  stopifnot(c("username","calendar_date") %in% colnames(df),
            c("username","calendar_date") %in% colnames(map.long))
  
  df$in.df <- 1
  map.long <- merge(map.long, df, all.x = TRUE, all.y = FALSE, by = c("username","calendar_date"))
  # if in.df=0 then a participant day in map.long does not have
  # a corresponding row in df
  map.long$in.df <- replace(map.long$in.df,is.na(map.long$in.df),0) 
  
  return(map.long)
}

CleanOutcomeData <- function(f1, ArgsList){
  # f1 is a data frame corresponding to a given participant
  # f1 must have rows ordered according to increasing study_day,
  # having only one row per study day, and having a row for each study day
  # before the operations below are performed. If a given study day has a 
  # missing value in the outcome, then it still has to have a row but the
  # value of the associated outcome must be missing
  # 
  # CleanOutcomeData takes a data frame f1 and uses ArgsList to perform data 
  # manipulation on f1, for example
  # ArgsList = list(map.long = map.long, 
  #                 issues.affecting.outcome.all.aims = issues.affecting.outcome.all.aims)
  
  map.long <- ArgsList$map.long
  map.long$username <- as.character(map.long$username)  # so that we are comparing character type against character type
  f1$username <- as.character(f1$username)
  issues.affecting.outcome.all.aims <- ArgsList$issues.affecting.outcome.all.aims
  
  if(!((unique(f1$username)) %in% (unique(map.long$username)))){  
    # there are some participants with daily survey files that will not be included in the data for analysis
    return(NULL)
  }else{  # perform data manipulation tasks
    
    this.participant.map.long <- map.long[map.long$username == (unique(f1$username)),]
    this.participant.day0.calendar_date <- this.participant.map.long[this.participant.map.long$study_day==1, c("priorday.calendar_date")]
    this.participant.day0.calendar_date <- strptime(this.participant.day0.calendar_date, format = "%m/%d/%Y", tz="EST5EDT")
    this.participant.day0.calendar_date <- strftime(this.participant.day0.calendar_date, format = "%m/%d/%Y", tz="EST5EDT")
    
    f1$date <- strptime(f1$date, format = "%Y%m%d", tz="EST5EDT")
    f1$date <- strftime(f1$date, format = "%m/%d/%Y", tz="EST5EDT")
    names(f1)[3] <- "calendar_date"
    names(f1)[4] <- "isCompleted"
    f1 <- f1[, !(colnames(f1)%in%c("day_count"))]
    
    if(sum(f1$calendar_date == this.participant.day0.calendar_date) == 0){
      f1$isCompleted_day0 <- NA
    }else{
      f1$isCompleted_day0 <- f1[f1$calendar_date == this.participant.day0.calendar_date,]$isCompleted
    }
    
    f1 <- merge(this.participant.map.long, f1, all.x = TRUE, all.y = FALSE, by = c("username","calendar_date"))
    f1 <- f1[order(f1$calendar_date),]  # important to order according to increasing calendar date
    
    issues.affecting.outcome.all.aims$username <- as.character(issues.affecting.outcome.all.aims$username)
    issues.affecting.outcome.all.aims$outcome.with.issue <- 1
    issues.affecting.outcome.all.aims$calendar_date <- strptime(issues.affecting.outcome.all.aims$calendar_date, format = "%m/%d/%Y", tz="EST5EDT")
    issues.affecting.outcome.all.aims$calendar_date <- strftime(issues.affecting.outcome.all.aims$calendar_date, format = "%m/%d/%Y", tz="EST5EDT")
    
    f1 <- merge(f1, issues.affecting.outcome.all.aims, by = c("username","calendar_date"), all.x=TRUE, all.y=FALSE)
    f1$outcome.with.issue <- replace(f1$outcome.with.issue, is.na(f1$outcome.with.issue), 0)
    f1$isCompleted <- replace(f1$isCompleted, f1$outcome.with.issue==1, 1)
    
    # Order rows by study_day. Do not use calendar_date to order rows because 
    # this does not handle dates across years well 
    # (e.g. when a participant's observations are from December 2017 to January 2018)
    f1 <- f1[order(f1$study_day),]
    # If f1 is not in the appropriate format then the 
    # operations below will not work properly
    f1$isCompleted_tomorrow <- c(tail(f1$isCompleted, n=-1L), NA)
    f1$isCompleted_yesterday <- c(NA,head(f1$isCompleted, n=-1L))
    return(f1)
  }
}

CleanInterventionDataAim1 <- function(f1){
  colnames(f1)[2] <- "calendar_date"
  f1$calendar_date <- as.character(f1$calendar_date)
  f1$calendar_date <- strptime(f1$calendar_date, format = "%Y%m%d", tz="EST5EDT")
  f1$calendar_date <- strftime(f1$calendar_date, format = "%m/%d/%Y", tz="EST5EDT")
  f1 <- f1[, !(colnames(f1) %in% c("day_count"))]
  return(f1)
}

CleanInterventionDataAim2 <- function(f1){
  colnames(f1)[6] <- "calendar_date"
  f1$calendar_date <- as.character(f1$calendar_date)
  f1$calendar_date <- strptime(f1$calendar_date, format = "%Y%m%d", tz="EST5EDT")
  f1$calendar_date <- strftime(f1$calendar_date, format = "%m/%d/%Y", tz="EST5EDT")
  f1 <- subset(f1, select = -day_count)
  f1$unix_ts <- f1$unix_ts/1000  # We convert unix time stamp into seconds
  return(f1)
}

CleanInterventionDataAim4 <- function(f1){
  colnames(f1)[6] <- "calendar_date"
  f1$calendar_date <- as.character(f1$calendar_date)
  f1$calendar_date <- strptime(f1$calendar_date, format = "%Y%m%d", tz="EST5EDT")
  f1$calendar_date <- strftime(f1$calendar_date, format = "%m/%d/%Y", tz="EST5EDT")
  f1$unix_ts <- f1$unix_ts/1000  # unix_ts is the time of randomization to aim 4 intervention
  f1 <- f1[, !(colnames(f1) %in% c("day_count","readable_ts"))]  
  colnames(f1) <- replace(colnames(f1), colnames(f1)=="unix_ts","unix_ts_lifeinsight") 
  return(f1)
}

CleanAvailabilityData <- function(f1){
  f1$calendar_date <- as.character(f1$calendar_date)
  f1$calendar_date <- strptime(f1$calendar_date, format = "%m/%d/%Y", tz="EST5EDT")
  f1$calendar_date <- strftime(f1$calendar_date, format = "%m/%d/%Y", tz="EST5EDT")
  f1$availability <- 0
  return(f1)
}

TimeFUNAim1 <- function(map.long){  # Time function for creating app usage variable for Aim 1
  map.long$unix_ts_12AM <- strptime(map.long$calendar_date, format = "%m/%d/%Y", tz="EST5EDT")
  map.long$unix_ts_12AM  <- as.numeric(map.long$unix_ts_12AM)
  map.long$unix_ts_4PM <- map.long$unix_ts_12AM + 16*60*60
  dat.start.study.clock <- map.long[map.long$study_day == 1, c("username","unix_ts_12AM")]
  colnames(dat.start.study.clock) <- c("username","start.study.clock")
  map.long <- merge(map.long, dat.start.study.clock, all.x = TRUE, all.y = FALSE, by = c("username"))
  map.long <- map.long[, !(colnames(map.long)%in% c("unix_ts_12AM"))]
  return(map.long)
}

TimeFUNAim2 <- function(map.long, ArgsList){  # Time function for creating app usage variable for Aim 2 and control variable of study staff contacting participants
  map.long$unix_ts_12AM <- strptime(map.long$calendar_date, format = "%m/%d/%Y", tz="EST5EDT")
  map.long$unix_ts_12AM  <- as.numeric(map.long$unix_ts_12AM)
  map.long$unix_ts_6PM <- map.long$unix_ts_12AM + 18*60*60
  dat.start.study.clock <- map.long[map.long$study_day == 1, c("username","unix_ts_12AM")]
  colnames(dat.start.study.clock) <- c("username","start.study.clock")
  map.long <- merge(map.long, dat.start.study.clock, all.x = TRUE, all.y = FALSE, by = c("username"))
  map.long <- map.long[, !(colnames(map.long)%in% c("unix_ts_12AM"))]
  
  memegifdata <- ArgsList$memegifdata
  memegifdata$calendar_date <-as.character(memegifdata$calendar_date)
  # unix_ts is the time stamp associated with randomization of meme/GIFs
  # ts_dailysurvey_completed is the time stamp associated with completion of daily survey
  # We do not need to divide unix_ts and ts_dailysurvey_completed by 1000 because these are already in seconds
  # In contrast, we needed to divide the unix time stamp in the app usage raw data because it is in milliseconds
  # Converting a calendar date in R into a unix time stamp will be displayed in seconds by default
  memegifdata <- memegifdata[, c("username", "calendar_date", "unix_ts", "ts_dailysurvey_completed")]
  map.long <- merge(map.long, memegifdata, by = c("username", "calendar_date"), all.x = TRUE, all.y = FALSE)
  colnames(map.long) <- replace(colnames(map.long), colnames(map.long) == "unix_ts", "unix_ts_memegif")
  return(map.long)
}

TimeFUNAim4 <- function(map.long, ArgsList){ 
  map.long$unix_ts_12AM <- strptime(map.long$calendar_date, format = "%m/%d/%Y", tz="EST5EDT")
  map.long$unix_ts_12AM  <- as.numeric(map.long$unix_ts_12AM)
  map.long$unix_ts_6PM <- map.long$unix_ts_12AM + 18*60*60
  dat.start.study.clock <- map.long[map.long$study_day == 1, c("username","unix_ts_12AM")]
  colnames(dat.start.study.clock) <- c("username","start.study.clock")
  map.long <- merge(map.long, dat.start.study.clock, all.x = TRUE, all.y = FALSE, by = c("username"))
  map.long <- map.long[, !(colnames(map.long)%in% c("unix_ts_12AM"))]
  
  lifeinsight <- ArgsList$lifeinsight
  lifeinsight$calendar_date <- as.character(lifeinsight$calendar_date)
  map.long <- merge(map.long, lifeinsight, by = c("username", "calendar_date","study_day"), all.x = TRUE, all.y = FALSE)
  
  return(map.long)
}

GetTimeBoundsAim1 <- function(map.wide, TimeFUNAim1, data.to.exclude){
  map.long.aim1 <- DropAll(LongMap(map.wide = map.wide, MyFUN = TimeFUNAim1), data.to.exclude)
  map.long.aim1$username <- factor(map.long.aim1$username)
  map.long.aim1$unixts.priorhhours <- apply(as.matrix(map.long.aim1$unix_ts_4PM), 
                                            MARGIN = 1,
                                            FUN = GetPriorHHours, 
                                            H = 72)
  colnames(map.long.aim1) <- c("username","study_day","calendar_date","upper","start.study.clock","lower")
  return(map.long.aim1)
}

GetTimeBoundsAim2 <- function(map.wide, TimeFUNAim2, memegifdata, data.to.exclude){
  map.long.aim2 <- DropAll(LongMap(map.wide = map.wide, 
                                   MyFUN = TimeFUNAim2, 
                                   ArgsList = list(memegifdata = memegifdata)), 
                           data.to.exclude = data.to.exclude)
  
  map.long.aim2$unix_ts_memegif.priorhhours <- apply(as.matrix(map.long.aim2$unix_ts_memegif), 
                                                     MARGIN = 1,
                                                     FUN = GetPriorHHours, 
                                                     H = 80)
  
  map.long.aim2$ts_dailysurvey_completed.priorhhours <- apply(as.matrix(map.long.aim2$ts_dailysurvey_completed), 
                                                              MARGIN = 1,
                                                              FUN = GetPriorHHours, 
                                                              H = 80)
  
  map.long.aim2$unix_ts_6PM.priorhhours <- apply(as.matrix(map.long.aim2$unix_ts_6PM), 
                                                 MARGIN = 1,
                                                 FUN = GetPriorHHours, 
                                                 H = 80)
  
  map.long.aim2$upper <- NA
  map.long.aim2$lower <- NA
  
  map.long.aim2$upper <- map.long.aim2$unix_ts_memegif
  map.long.aim2$upper <- ifelse(is.na(map.long.aim2$unix_ts_memegif) & (!is.na(map.long.aim2$ts_dailysurvey_completed)), map.long.aim2$ts_dailysurvey_completed, map.long.aim2$upper)
  map.long.aim2$upper <- ifelse(is.na(map.long.aim2$unix_ts_memegif) & is.na(map.long.aim2$ts_dailysurvey_completed), map.long.aim2$unix_ts_6PM, map.long.aim2$upper)
  
  map.long.aim2$lower <- map.long.aim2$unix_ts_memegif.priorhhours
  map.long.aim2$lower <- ifelse(is.na(map.long.aim2$unix_ts_memegif.priorhhours) & (!is.na(map.long.aim2$ts_dailysurvey_completed.priorhhours)), map.long.aim2$ts_dailysurvey_completed.priorhhours, map.long.aim2$lower)
  map.long.aim2$lower <- ifelse(is.na(map.long.aim2$unix_ts_memegif.priorhhours) & is.na(map.long.aim2$ts_dailysurvey_completed.priorhhours), map.long.aim2$unix_ts_6PM.priorhhours, map.long.aim2$lower)
  map.long.aim2 <- map.long.aim2[, colnames(map.long.aim2) %in% c("username","study_day","calendar_date","start.study.clock","upper","lower")]
  return(map.long.aim2)
}

GetTimeBoundsAim4 <- function(map.wide, TimeFUNAim4, lifeinsight, data.to.exclude){
  map.long.aim4 <- DropAll(LongMap(map.wide = map.wide, 
                                   MyFUN = TimeFUNAim4, 
                                   ArgsList = list(lifeinsight = lifeinsight)), 
                           data.to.exclude = data.to.exclude)
  
  map.long.aim4$unix_ts_lifeinsight.priorhhours <- apply(as.matrix(map.long.aim4$unix_ts_lifeinsight), 
                                                         MARGIN = 1,
                                                         FUN = GetPriorHHours, 
                                                         H = 80)
  
  map.long.aim4$ts_activetasks_completed.priorhhours <- apply(as.matrix(map.long.aim4$ts_activetasks_completed), 
                                                              MARGIN = 1,
                                                              FUN = GetPriorHHours, 
                                                              H = 80)
  
  map.long.aim4$unix_ts_6PM.priorhhours <- apply(as.matrix(map.long.aim4$unix_ts_6PM), 
                                                 MARGIN = 1,
                                                 FUN = GetPriorHHours, 
                                                 H = 80)
  
  map.long.aim4$upper <- NA
  map.long.aim4$lower <- NA
  
  map.long.aim4$upper <- map.long.aim4$unix_ts_lifeinsight
  map.long.aim4$upper <- ifelse(is.na(map.long.aim4$unix_ts_lifeinsight) & (!is.na(map.long.aim4$ts_activetasks_completed)), map.long.aim4$ts_activetasks_completed, map.long.aim4$upper)
  map.long.aim4$upper <- ifelse(is.na(map.long.aim4$unix_ts_lifeinsight) & is.na(map.long.aim4$ts_activetasks_completed), map.long.aim4$unix_ts_6PM, map.long.aim4$upper)
  
  map.long.aim4$lower <- map.long.aim4$unix_ts_lifeinsight.priorhhours
  map.long.aim4$lower <- ifelse(is.na(map.long.aim4$unix_ts_lifeinsight.priorhhours) & (!is.na(map.long.aim4$ts_activetasks_completed.priorhhours)), map.long.aim4$ts_activetasks_completed.priorhhours, map.long.aim4$lower)
  map.long.aim4$lower <- ifelse(is.na(map.long.aim4$unix_ts_lifeinsight.priorhhours) & is.na(map.long.aim4$ts_activetasks_completed.priorhhours), map.long.aim4$unix_ts_6PM.priorhhours, map.long.aim4$lower)
  map.long.aim4 <- map.long.aim4[, colnames(map.long.aim4) %in% c("username","study_day","calendar_date","start.study.clock","upper","lower")]
  return(map.long.aim4)
}

CleanAppUsageUnknownData <- function(f1){
  f1$calendar_date <- as.character(f1$calendar_date)
  f1$calendar_date <- strptime(f1$calendar_date, format = "%m/%d/%Y", tz="EST5EDT")
  f1$calendar_date <- strftime(f1$calendar_date, format = "%m/%d/%Y", tz="EST5EDT")
  f1$appusage_unknown <- 1
  return(f1)
}

CleanAppData <- function(f1){
  names(f1)[7] <- "readable_ts"  
  f1$unix_ts <- f1$unix_ts/1000
  f1 <- f1[, !(colnames(f1) %in% c("day_count"))]
  colnames(f1) <- c("username", "calendar_date", "unix_ts", "view", "status", "readable_ts")
  f1$calendar_date <- as.character(f1$calendar_date)
  f1$calendar_date <- strptime(f1$calendar_date, format = "%Y%m%d", tz="EST5EDT")
  f1$calendar_date <- strftime(f1$calendar_date, format = "%m/%d/%Y", tz="EST5EDT")
  f1$calendar_date <- as.character(f1$calendar_date)
  f1 <- f1[order(f1$unix_ts),]
  f1$eventid <- 1:nrow(f1)
  return(f1)
}

GetAnchorTimeIntervals <- function(appdata, anchor, lag){
  
  row.names(appdata) <- 1:dim(appdata)[1]
  appdata$view <- as.character(appdata$view)
  appdata$status <- as.character(appdata$status)
  
  is.anchor <- lapply(anchor, function(one.view.status.pair, 
                                       enumerate.view =  appdata$view, 
                                       enumerate.status =  appdata$status){
    indicator.view <- grepl(one.view.status.pair[1], enumerate.view)
    indicator.status <- grepl(one.view.status.pair[2], enumerate.status)
    is.anchor <- indicator.view*indicator.status
    return(is.anchor)
  })
  
  is.anchor <- do.call(cbind, is.anchor)
  appdata$is.any.anchor <- rowSums(is.anchor) 
  anchor.time.intervals <- appdata[appdata$is.any.anchor > 0,]
  anchor.time.intervals$unix_ts_upper <- anchor.time.intervals$unix_ts + lag
  anchor.time.intervals <- anchor.time.intervals[c("username","eventid","unix_ts","unix_ts_upper")]
  colnames(anchor.time.intervals) <- c("username","eventid","lower","upper")
  anchor.time.intervals <- anchor.time.intervals[order(anchor.time.intervals$username, anchor.time.intervals$eventid),]
  return(anchor.time.intervals)
}

GetPriorHHours <- function(current.timestamp, H){
  return(current.timestamp - H*60*60) # convert H hours into seconds
}

CheckElapsedTime <- function(current.timestamp, reference.timestamp, H){
  # Checks whether time elapsed since start.time is greater than H
  # H is in hours
  # current.timestamp is the time point we wish to count back from and is in UNIX timestamp format
  
  result <- 1*((current.timestamp - reference.timestamp) < H*60*60)
  
  return(result)
}

SplitDF <- function(df1, df2){
  # Given two data frames, df1 and df2 having a common reference ID column, 'username'
  # Create list indexed by username whose elements are the subsets of rows from df1 and df2 associated with username
  
  by.obj <- by(df1, df1$username, function(dfx, dfy=df2){
    dfy.user <- dfy[dfy$username %in% unique(dfx$username),]
    out.list <- list(dfx, dfy.user)
    return(out.list)
  })
  split.obj <- lapply(by.obj, as.list)
  return(split.obj)
}

SplitObjMethod <- function(my.split.obj, this.df){
  # this.df = 1: For each given timestamp, check whether the timestamp falls inside of any interval from a given fixed list of intervals
  # and repeat this same operation for a large list of timestamps
  # this.df = 2: For each given time interval, check whether the interval contains any timestamp from a given fixed list of timestamps
  # and repeat this same operation for a large list of time intervals
  
  dfx <- try(my.split.obj[[1]], silent = TRUE)
  dfy <- try(my.split.obj[[2]], silent = TRUE)
  
  if(class(dfx) == "try-error" | class(dfy) == "try-error"){ # takes care of 0KB files
    return(NULL)
  }else{
    if(this.df == 1){  # apply function to rows of dfx
      eventid.val <- as.matrix(dfx$eventid)
      mid <- as.matrix(dfx$mid)
      lower.val <- as.matrix(dfy$lower)
      upper.val <- as.matrix(dfy$upper)
      
      isin.interval <- apply(mid, 1, function(mid,lower=lower.val,upper=upper.val){
        # lower is a vector of time stamps that form the lower bound of a time interval
        # upper is a vector of time stamps that form the upper bound of a time interval
        checks <- 1*(mid>=lower & mid<=upper)
        out <- sum(checks)
        return(out)
      })
      
      tmpdf <- cbind(eventid.val,isin.interval)
      tmpdf <- as.data.frame(tmpdf)
      colnames(tmpdf) <- c("eventid","isin.interval")
      tmpdf <- tmpdf[order(tmpdf$eventid),]
      this.participant.output <- merge(dfx, tmpdf, all.x = TRUE, all.y=TRUE, by = c("eventid"))
      this.participant.output <- this.participant.output[!(colnames(this.participant.output) %in% c("mid"))]
      
    }else{  # apply function to rows of dfy
      
      mid.val <- as.matrix(dfx$mid)
      lower.val <- as.matrix(dfy$lower)
      upper.val <- as.matrix(dfy$upper)
      days.val <- as.matrix(dfy$study_day)
      
      isin.interval <- mapply(function(lower, upper, mid = mid.val){
        # mid is a vector of time stamps
        # we wish to check whether there are any time stamps in mid that fall within (lower,upper)
        checks <- 1*(mid>=lower & mid<=upper)
        return(sum(checks))
      }, lower = lower.val, upper = upper.val)
      
      
      tmpdf <- cbind(days.val, isin.interval)
      tmpdf <- as.data.frame(tmpdf)
      colnames(tmpdf) <- c("study_day","isin.interval")
      this.participant.output <- merge(dfy, tmpdf, all.x = TRUE, all.y=TRUE, by = c("study_day"))
      this.participant.output <- this.participant.output[!(colnames(this.participant.output) %in% c("lower","upper"))]
    }
    
    return(this.participant.output)
  }
}
