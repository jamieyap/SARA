# Specify file paths to be used in any of the aims ----------------------------
# File paths are saved in an .Renviron file

path.code <- Sys.getenv("path.code")  # Location of all code
path.exclude.all <- Sys.getenv("path.exclude.all")  # Location of files indicating which participant-days to exclude from all aims
path.dataforanalysis <- Sys.getenv("path.dataforanalysis")  # Location of files to be used for data analysis
path.mapdata <- Sys.getenv("path.mapdata") # Location of file mapping days and dates for study participants

path.appusage <- Sys.getenv("path.appusage")  # Location of files containing app usage data

# Location of files containing info on when app usage is unknown for each aim
path.appusage.unknown.aim1 <- Sys.getenv("path.appusage.unknown.aim1")  
path.appusage.unknown.aim2 <- Sys.getenv("path.appusage.unknown.aim2") 
path.appusage.unknown.aim4 <- Sys.getenv("path.appusage.unknown.aim4")  

# Read in functions -----------------------------------------------------------
source(file.path(path.code, "io-utils.R"))
source(file.path(path.code, "data-manip-utils.R"))
source(file.path(path.code, "file-check-utils.R"))

# Run preprocessing and set variable values needed for all aims ---------------
participant.days.to.exclude <- ReadAll(path.to.files = path.exclude.all)
map.wide <- ReadAll(path.to.files = path.mapdata)
map.long <- DropAll(LongMap(map.wide = map.wide), participant.days.to.exclude)
map.long$username <- factor(map.long$username)

appdata <- ReadAll(path.appusage, CleanData = CleanAppData)
appdata <- merge(appdata, map.long, by = c("username","calendar_date"), all.x = TRUE, all.y = FALSE)
appdata <- appdata[!is.na(appdata$study_day),]
appdata <- DropAll(appdata, participant.days.to.exclude)
appdata$username <- factor(appdata$username)
appdata <- appdata[order(appdata$username, appdata$eventid),]

use.anchor <- list(c("dailysurvey","start"), c("dailysurvey","destroy"),
                   c("weekly_survey","start"), c("weekly_survey","destroy"),
                   c("TappingTask","start"), c("TappingTask","destroy"),
                   c("SpatialTask","start"), c("SpatialTask","destroy"),
                   c("ActiveTask","start"), c("ActiveTask","destroy"),
                   c("login","start"), c("login","destroy"),
                   c("app","resume"))
use.lag <- 10 # in seconds
anchor.time.intervals <- GetAnchorTimeIntervals(appdata = appdata, anchor = use.anchor, lag = use.lag)

tmpdf <- appdata[c("username","eventid","unix_ts")]
colnames(tmpdf) <- c("username","eventid","mid")

# Call to SplitDF is needed to ensure that we are not mixing up data across different participants
split.obj <- SplitDF(df1 = tmpdf, df2 = anchor.time.intervals)

all.participant.output <- lapply(split.obj, SplitObjMethod, this.df=1)
all.participant.output <- do.call(rbind, all.participant.output)
colnames(all.participant.output) <- c("eventid","username","is.within.any.anchor")

appdata <- merge(appdata, all.participant.output, all.x = TRUE, all.y = FALSE, by = c("username","eventid"))
appdata <- appdata[order(appdata$username, appdata$eventid),]
appdata$is.within.any.anchor <- 1*(appdata$is.within.any.anchor > 0)

subset.appdata <- appdata[appdata$is.within.any.anchor == 0,] # we take only the subset of data corresponding to app usage outside of daily survey or active task
subset.appdata <- subset.appdata[order(subset.appdata$username, subset.appdata$eventid),]

# Create app usage variable for Aim 1 -----------------------------------------
# Call to GetTimeBoundsAim1: Get lower and upper bound per participant day, 
# i.e. lower = 4PM on a given day, upper = 72 hours before 4PM
map.long.aim1 <- GetTimeBoundsAim1(map.wide = map.wide, TimeFUNAim1 = TimeFUNAim1, data.to.exclude = participant.days.to.exclude)  
tmpdf.aim1 <- subset.appdata[c("username","eventid","unix_ts")]
colnames(tmpdf.aim1) <- c("username","eventid","mid")
split.obj.aim1 <- SplitDF(df1 = tmpdf.aim1, df2 = map.long.aim1[c("username","study_day","lower","upper")])
all.participant.output.aim1 <- lapply(split.obj.aim1, SplitObjMethod, this.df=2)
all.participant.output.aim1 <- do.call(rbind, all.participant.output.aim1)
map.long.aim1 <- merge(map.long.aim1, all.participant.output.aim1, all.x = TRUE, all.y = FALSE, by = c("username","study_day"))
map.long.aim1$appusage_yes <- ifelse(1*(map.long.aim1$isin.interval>0) & (!is.na(map.long.aim1$isin.interval)), 1, 0)
map.long.aim1 <- map.long.aim1[order(map.long.aim1$username, map.long.aim1$study_day),]

# Unknown app usage
map.long.aim1$belowhhours <- mapply(CheckElapsedTime, 
                                    current.timestamp = map.long.aim1$upper, 
                                    reference.timestamp = map.long.aim1$start.study.clock, 
                                    H = 72)
write.csv(map.long.aim1[map.long.aim1$belowhhours==1,c("username","calendar_date")], 
          file.path(path.appusage.unknown.aim1, "aim1.belowhhours.csv"),
          row.names = FALSE)

appusage.unknown.aim1 <- ReadAll(path.to.files = path.appusage.unknown.aim1, CleanData = CleanAppUsageUnknownData)
map.long.aim1 <- merge(map.long.aim1, appusage.unknown.aim1, all.x = TRUE, all.y = FALSE, by = c("username","calendar_date"))
map.long.aim1$appusage_unknown <- replace(map.long.aim1$appusage_unknown,
                                          map.long.aim1$appusage_yes == 1,
                                          0)
map.long.aim1$appusage_unknown <- replace(map.long.aim1$appusage_unknown,
                                          is.na(map.long.aim1$appusage_unknown),
                                          0)

# No app usage
map.long.aim1$appusage_no <- ifelse((map.long.aim1$appusage_yes==0) & (map.long.aim1$appusage_unknown==0), 1, 0)

# Create app usage variable for Aim 2 -----------------------------------------
memegifdata <- read.csv(file.path(path.dataforanalysis, "memegifdata.csv"))
# Call to GetTimeBoundsAim2: Get lower and upper bound per participant day, 
# e.g. lower = time of meme/GIF randomization on a given day, upper = 80 hours before time of meme/GIF randomization
map.long.aim2 <- GetTimeBoundsAim2(map.wide=map.wide, TimeFUNAim2=TimeFUNAim2, memegifdata=memegifdata, data.to.exclude=participant.days.to.exclude) 
tmpdf.aim2 <- subset.appdata[c("username","eventid","unix_ts")]
colnames(tmpdf.aim2) <- c("username","eventid","mid")
split.obj.aim2 <- SplitDF(df1 = tmpdf.aim2, df2 = map.long.aim2[c("username","study_day","lower","upper")])
all.participant.output.aim2 <- lapply(split.obj.aim2, SplitObjMethod, this.df=2)
all.participant.output.aim2 <- do.call(rbind, all.participant.output.aim2)
map.long.aim2 <- merge(map.long.aim2, all.participant.output.aim2, all.x = TRUE, all.y = FALSE, by = c("username","study_day"))
map.long.aim2$appusage_yes <- ifelse(1*(map.long.aim2$isin.interval>0)  & (!is.na(map.long.aim2$isin.interval)), 1, 0)
map.long.aim2$appusage_yes <- replace(map.long.aim2$appusage_yes, is.na(map.long.aim2$upper), NA)  #upper is NA since randomization did not occur
map.long.aim2 <- map.long.aim2[order(map.long.aim2$username, map.long.aim2$study_day),]

# Unknown app usage
map.long.aim2$belowhhours <- mapply(CheckElapsedTime, 
                                    current.timestamp = map.long.aim2$upper, 
                                    reference.timestamp = map.long.aim2$start.study.clock, 
                                    H = 80)
write.csv(map.long.aim2[map.long.aim2$belowhhours==1,c("username","calendar_date")], 
          file.path(path.appusage.unknown.aim2, "aim2.belowhhours.csv"),
          row.names = FALSE)

appusage.unknown.aim2 <- ReadAll(path.to.files = path.appusage.unknown.aim2, CleanData = CleanAppUsageUnknownData)
map.long.aim2 <- merge(map.long.aim2, appusage.unknown.aim2, all.x = TRUE, all.y = FALSE, by = c("username","calendar_date"))
map.long.aim2$appusage_unknown <- replace(map.long.aim2$appusage_unknown,
                                          map.long.aim2$appusage_yes == 1,
                                          0)
map.long.aim2$appusage_unknown <- replace(map.long.aim2$appusage_unknown,
                                          is.na(map.long.aim2$appusage_unknown),
                                          0)
map.long.aim2$appusage_unknown <- replace(map.long.aim2$appusage_unknown, is.na(map.long.aim2$upper), NA)  #upper is NA since randomization did not occur

# No app usage
map.long.aim2$appusage_no <- ifelse((map.long.aim2$appusage_yes==0) & (map.long.aim2$appusage_unknown==0), 1, 0)
map.long.aim2$appusage_no <- replace(map.long.aim2$appusage_no, is.na(map.long.aim2$upper), NA)  #upper is NA since randomization did not occur

# Create app usage variable for Aim 4 -----------------------------------------
lifeinsight <- read.csv(file.path(path.dataforanalysis, "lifeinsight.csv"))
map.long.aim4 <- GetTimeBoundsAim4(map.wide=map.wide, TimeFUNAim4=TimeFUNAim4, lifeinsight=lifeinsight, data.to.exclude=participant.days.to.exclude) 
tmpdf.aim4 <- subset.appdata[c("username","eventid","unix_ts")]
colnames(tmpdf.aim4) <- c("username","eventid","mid")
split.obj.aim4 <- SplitDF(df1 = tmpdf.aim4, df2 = map.long.aim4[c("username","study_day","lower","upper")])
all.participant.output.aim4 <- lapply(split.obj.aim4, SplitObjMethod, this.df=2)
all.participant.output.aim4 <- do.call(rbind, all.participant.output.aim4)
map.long.aim4 <- merge(map.long.aim4, all.participant.output.aim4, all.x = TRUE, all.y = FALSE, by = c("username","study_day"))
map.long.aim4$appusage_yes <- ifelse(1*(map.long.aim4$isin.interval>0)  & (!is.na(map.long.aim4$isin.interval)), 1, 0)
map.long.aim4$appusage_yes <- replace(map.long.aim4$appusage_yes, is.na(map.long.aim4$upper), NA)  #upper is NA since randomization did not occur
map.long.aim4 <- map.long.aim4[order(map.long.aim4$username, map.long.aim4$study_day),]

# Unknown app usage
map.long.aim4$belowhhours <- mapply(CheckElapsedTime, 
                                    current.timestamp = map.long.aim4$upper, 
                                    reference.timestamp = map.long.aim4$start.study.clock, 
                                    H = 80)
write.csv(map.long.aim4[map.long.aim4$belowhhours==1,c("username","calendar_date")], 
          file.path(path.appusage.unknown.aim4, "aim4.belowhhours.csv"),
          row.names = FALSE)

appusage.unknown.aim4 <- ReadAll(path.to.files = path.appusage.unknown.aim4, CleanData = CleanAppUsageUnknownData)
map.long.aim4 <- merge(map.long.aim4, appusage.unknown.aim4, all.x = TRUE, all.y = FALSE, by = c("username","calendar_date"))
map.long.aim4$appusage_unknown <- replace(map.long.aim4$appusage_unknown,
                                          map.long.aim4$appusage_yes == 1,
                                          0)
map.long.aim4$appusage_unknown <- replace(map.long.aim4$appusage_unknown,
                                          is.na(map.long.aim4$appusage_unknown),
                                          0)
map.long.aim4$appusage_unknown <- replace(map.long.aim4$appusage_unknown, is.na(map.long.aim4$upper), NA)  #upper is NA since randomization did not occur

# No app usage
map.long.aim4$appusage_no <- ifelse((map.long.aim4$appusage_yes==0) & (map.long.aim4$appusage_unknown==0), 1, 0)
map.long.aim4$appusage_no <- replace(map.long.aim4$appusage_no, is.na(map.long.aim4$upper), NA)  #upper is NA since randomization did not occur

# Save app usage data for all aims --------------------------------------------
map.long.aim1 <- map.long.aim1[c("username", "study_day", "calendar_date", "appusage_yes", "appusage_unknown", "appusage_no")]
map.long.aim2 <- map.long.aim2[c("username", "study_day", "calendar_date", "appusage_yes", "appusage_unknown", "appusage_no")]
map.long.aim4 <- map.long.aim4[c("username", "study_day", "calendar_date", "appusage_yes", "appusage_unknown", "appusage_no")]

write.csv(map.long.aim1, file.path(path.dataforanalysis, "appusage.aim1.csv"), row.names = FALSE)
write.csv(map.long.aim2, file.path(path.dataforanalysis, "appusage.aim2.csv"), row.names = FALSE)
write.csv(map.long.aim4, file.path(path.dataforanalysis, "appusage.aim4.csv"), row.names = FALSE)



