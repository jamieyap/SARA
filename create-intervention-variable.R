# Specify file paths to be used in any of the aims ----------------------------
# File paths are saved in an .Renviron file
path.code <- Sys.getenv("path.code")  # Location of all code
path.exclude.all <- Sys.getenv("path.exclude.all")  # Location of files indicating which participant-days to exclude from all aims
path.dataforanalysis <- Sys.getenv("path.dataforanalysis")  # Location of files to be used for data analysis
path.mapdata <- Sys.getenv("path.mapdata") # Location of file mapping days and dates for study participants
path.dailysurveycompletion_timestamps <- Sys.getenv("path.dailysurveycompletion_timestamps") # Location of file containing timestamps of when daily survey was completed
path.activetaskcompletion_timestamps <- Sys.getenv("path.activetaskcompletion_timestamps") # Location of file containing timestamps of when both active tasks was completed
path.issues.affecting.outcome.all.aims <- Sys.getenv("path.issues.affecting.outcome.all.aims")  # Location of files indicating which participant-days have outcome impacted by issues affecting all aims

# Location of files indicating randomization assignment for a given participant-day
path.aim1.intervention <- Sys.getenv("path.aim1.intervention")
path.aim2.intervention <- Sys.getenv("path.aim2.intervention")
path.aim4.intervention <- Sys.getenv("path.aim4.intervention")

# Location of files indicating availability for a given participant-day
path.aim1.availability <- Sys.getenv("path.aim1.availability")
path.aim2.availability <- Sys.getenv("path.aim2.availability")
path.aim4.availability <- Sys.getenv("path.aim4.availability")

# Read in functions -----------------------------------------------------------
source(file.path(path.code, "io-utils.R"))
source(file.path(path.code, "data-manip-utils.R"))
source(file.path(path.code, "file-check-utils.R"))

# Obtain record of participant days to include --------------------------------
participant.days.to.exclude <- ReadAll(path.exclude.all)
map.wide <- ReadAll(path.mapdata)
map.long <- DropAll(LongMap(map.wide), participant.days.to.exclude)

# Obtain time stamps of dailysurvey completion --------------------------------
dailysurveycompletion.timestamps <- SimpleReadAll(path.dailysurveycompletion_timestamps, format.date = TRUE, current.format = "%Y%m%d")
# Convert list into data frame
dailysurveycompletion.timestamps <- data.frame(dailysurveycompletion.timestamps)
# Rename column and obtain subset of data
colnames(dailysurveycompletion.timestamps) <- gsub("all_daily_study_and_SMS_ts.csv.", "", colnames(dailysurveycompletion.timestamps))
dailysurveycompletion.timestamps <- dailysurveycompletion.timestamps[, c("username", "calendar_date","ts_now")]
colnames(dailysurveycompletion.timestamps) <- c("username", "calendar_date","ts_dailysurvey_completed")
# Detect if there is more than one daily survey completion timestamp on a 
# given participant day. If there is more than one, only keep the earliest 
# recorded timestamp of the day
dailysurveycompletion.timestamps <- dailysurveycompletion.timestamps[order(dailysurveycompletion.timestamps$username, dailysurveycompletion.timestamps$ts_dailysurvey_completed),]
dailysurveycompletion.timestamps <- dailysurveycompletion.timestamps[!duplicated(dailysurveycompletion.timestamps[,c("username","calendar_date")]),]

# Obtain time stamps of completion of both active tasks -----------------------
activetaskcompletion_timestamps <- SimpleReadAll(path.activetaskcompletion_timestamps, format.date = TRUE, current.format = "%Y%m%d")
# Convert list into data frame
activetaskcompletion_timestamps <- data.frame(activetaskcompletion_timestamps)
# Rename column
colnames(activetaskcompletion_timestamps) <- c("username","calendar_date","ts_activetasks_completed")
# Convert timestamps from milliseconds to seconds
activetaskcompletion_timestamps$ts <- (activetaskcompletion_timestamps$ts)/1000
# Detect if there is more than one active task completion timestamp on a 
# given participant day. If there is more than one, only keep the latest
# recorded timestamp of the day
activetaskcompletion_timestamps <- activetaskcompletion_timestamps[order(activetaskcompletion_timestamps$username, activetaskcompletion_timestamps$ts_activetasks_completed),]
activetaskcompletion_timestamps  <- activetaskcompletion_timestamps[!(duplicated(activetaskcompletion_timestamps[,c("username","calendar_date")], fromLast = TRUE)),]

# Intervention Variable for Aim 1 ---------------------------------------------
all.4PM.data <- ReadAll(path.to.files = path.aim1.intervention, CleanData = CleanInterventionDataAim1)

# Obtain aim 1 randomization assignment for each participant day in map.long
subset.4PM.data  <- GetWithinStudyPeriod(all.4PM.data, map.long)
subset.4PM.data <- subset.4PM.data[, !(colnames(subset.4PM.data)%in%c("in.df"))]

# Obtain availability to aim 1 intervention for each participant day in map.long
availability.4PM.data <- ReadAll(path.to.files = path.aim1.availability, CleanData = CleanAvailabilityData)
availability.4PM.data <- GetWithinStudyPeriod(availability.4PM.data, map.long)
availability.4PM.data$availability <- replace(availability.4PM.data$availability, availability.4PM.data$in.df==0, 1)
availability.4PM.data <- availability.4PM.data[,!(colnames(availability.4PM.data) %in% c("study_day","in.df"))]
subset.4PM.data <- merge(subset.4PM.data, availability.4PM.data, all.x=TRUE, all.y=FALSE, by = c("username", "calendar_date"))

# Intervention Variable for Aim 2 ---------------------------------------------
all.memegif.data <- ReadAll(path.to.files = path.aim2.intervention, CleanData = CleanInterventionDataAim2)

# Obtain aim 2 randomization assignment for each participant day in map.long
subset.memegif.data <- GetWithinStudyPeriod(all.memegif.data, map.long)
subset.memegif.data <- subset.memegif.data[, !(colnames(subset.memegif.data)%in%c("in.df"))]

# Obtain availability to aim 2 intervention for each participant day in map.long
outcomedata <- read.csv(file.path(path.dataforanalysis, "outcomedata.csv"))
non.completion.daily.survey <- outcomedata[outcomedata$daily_survey_isCompleted == 0, c("username", "calendar_date")]
write.csv(non.completion.daily.survey, file.path(path.aim2.availability, "noncompletion_aim2.csv"), row.names = FALSE)

availability.memegif.data <- ReadAll(path.to.files = path.aim2.availability, CleanData = CleanAvailabilityData)
availability.memegif.data <- GetWithinStudyPeriod(availability.memegif.data, map.long)
availability.memegif.data$availability <- replace(availability.memegif.data$availability, availability.memegif.data$in.df==0, 1)
availability.memegif.data <- availability.memegif.data[,!(colnames(availability.memegif.data) %in% c("study_day","in.df"))]
subset.memegif.data <- merge(subset.memegif.data, availability.memegif.data, all.x=TRUE, all.y=FALSE, by = c("username", "calendar_date"))

# -----------------------------------------------------------------------------
# Identify all participants who were impacted by "big bug" at any point during 
# the study in user.memegifbug
# -----------------------------------------------------------------------------
aim2.availability.commondays <- GetCommonPersonDays(path.aim2.availability, map.long)
user.memegifbug <- as.character(unique(aim2.availability.commondays[aim2.availability.commondays$issue_01.csv.in.df==1,"issue_00.csv.username"]))

# Create variable to indicate which participant days impacted by "big bug"
subset.memegif.data$memegifbug <- ifelse(subset.memegif.data$username %in% user.memegifbug, 1, 0)
# Drop unnecessary columns
# the column unix_ts should not be dropped since create-appusage-variable.R uses it
subset.memegif.data <- subset.memegif.data[, !(colnames(subset.memegif.data) %in% c("readable_ts"))] 

# Intervention Variable for Aim 4 ---------------------------------------------
all.lifeinsight.data <- ReadAll(path.to.files = path.aim4.intervention, CleanData = CleanInterventionDataAim4)

# Obtain aim 4 randomization assignment for each participant day in map.long
subset.lifeinsight.data <- GetWithinStudyPeriod(all.lifeinsight.data, map.long)
subset.lifeinsight.data <- subset.lifeinsight.data[, !(colnames(subset.lifeinsight.data)%in%c("in.df"))]

# Obtain availability to aim 4 intervention for each participant day in map.long
outcomedata <- read.csv(file.path(path.dataforanalysis, "outcomedata.csv"))
non.completion.lifeinsight <- outcomedata[outcomedata$both_active_tasks_isCompleted == 0, c("username", "calendar_date")]
# availability = 0 for Aim 4
write.csv(non.completion.lifeinsight, file.path(path.aim4.availability, "noncompletion_aim4.csv"), row.names = FALSE)  

availability.lifeinsight.data <- ReadAll(path.to.files = path.aim4.availability, CleanData = CleanAvailabilityData)
availability.lifeinsight.data <- GetWithinStudyPeriod(availability.lifeinsight.data, map.long)
availability.lifeinsight.data$availability <- replace(availability.lifeinsight.data$availability, availability.lifeinsight.data$in.df==0, 1)
availability.lifeinsight.data <- availability.lifeinsight.data[,!(colnames(availability.lifeinsight.data) %in% c("study_day","in.df"))]
subset.lifeinsight.data <- merge(subset.lifeinsight.data, availability.lifeinsight.data, all.x=TRUE, all.y=FALSE, by = c("username", "calendar_date"))

# Create variable to indicate which participant days impacted by "big bug" in Aim 2 for Aim 4 analysis
subset.lifeinsight.data$memegifbug <- ifelse(subset.lifeinsight.data$username %in% user.memegifbug, 1, 0)

# Save datasets created -------------------------------------------------------
subset.4PM.data <- merge(subset.4PM.data, dailysurveycompletion.timestamps, all.x = TRUE, all.y = FALSE, by = c("username", "calendar_date")) 
subset.memegif.data <- merge(subset.memegif.data, dailysurveycompletion.timestamps, all.x = TRUE, all.y = FALSE, by = c("username", "calendar_date")) 
subset.lifeinsight.data <- merge(subset.lifeinsight.data, activetaskcompletion_timestamps, all.x = TRUE, all.y = FALSE, by = c("username", "calendar_date"))

write.csv(subset.4PM.data, file.path(path.dataforanalysis, "fourPMdata.csv"), row.names = FALSE)
write.csv(subset.memegif.data, file.path(path.dataforanalysis, "memegifdata.csv"), row.names = FALSE)
write.csv(subset.lifeinsight.data, file.path(path.dataforanalysis, "lifeinsight.csv"), row.names = FALSE)
