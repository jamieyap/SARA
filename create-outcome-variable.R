# Specify file paths to be used in any of the aims ----------------------------
# File paths are saved in an .Renviron file

path.code <- Sys.getenv("path.code")  # Location of all code
path.exclude.all <- Sys.getenv("path.exclude.all")  # Location of files indicating which participant-days to exclude from all aims
path.dataforanalysis <- Sys.getenv("path.dataforanalysis")  # Location of files to be used for data analysis
path.mapdata <- Sys.getenv("path.mapdata") # Location of file mapping study days and calendar dates for study participants
path.dailysurvey.rawdata <- Sys.getenv("path.dailysurvey.rawdata")  # Location of files recording whether or not participant completed daily survey
path.activetask.rawdata <- Sys.getenv("path.activetask.rawdata")  # Location of files recording whether or not participant completed active tasks
path.issues.affecting.outcome.all.aims <- Sys.getenv("path.issues.affecting.outcome.all.aims")  # Location of files indicating which participant-days have outcome impacted by issues affecting all aims

# Read in functions -----------------------------------------------------------
source(file.path(path.code, "io-utils.R"))
source(file.path(path.code, "data-manip-utils.R"))
source(file.path(path.code, "file-check-utils.R"))

# Run preprocessing and set variable values -----------------------------------
participant.days.to.exclude <- ReadAll(path.exclude.all)
map.wide <- ReadAll(path.mapdata)
map.long <- DropAll(LongMap(map.wide), participant.days.to.exclude)
map.long$username <- factor(map.long$username)

# Get Day 0 dates
map.long$priorday.calendar_date <- strptime(map.long$calendar_date, format = "%m/%d/%Y", tz = "EST5EDT") - 1
map.long$priorday.calendar_date <- strftime(map.long$priorday.calendar_date, format = "%m/%d/%Y", tz = "EST5EDT")

# Create outcome variables for all aims ---------------------------------------
issues.affecting.outcome.all.aims <- ReadAll(path.to.files = path.issues.affecting.outcome.all.aims, CleanData = NULL)
dailysurvey.rawdata <- ReadAll(path.to.files = path.dailysurvey.rawdata, CleanData = CleanOutcomeData, 
                               ArgsList = list(map.long=map.long, issues.affecting.outcome.all.aims = issues.affecting.outcome.all.aims))

activetask.rawdata <- ReadAll(path.to.files = path.activetask.rawdata, CleanData = CleanOutcomeData,
                              ArgsList = list(map.long=map.long, issues.affecting.outcome.all.aims = issues.affecting.outcome.all.aims))

dailysurvey.rawdata <- dailysurvey.rawdata[, !(colnames(dailysurvey.rawdata) %in% c("priorday.calendar_date"))]
activetask.rawdata <- activetask.rawdata[, !(colnames(activetask.rawdata) %in% c("priorday.calendar_date"))]

# All column names prefixed by isCompleted we add the prefix "daily_survey_"
colnames(dailysurvey.rawdata) <- gsub(pattern = "isCompleted", replacement = "daily_survey_isCompleted", x = colnames(dailysurvey.rawdata))
# All column names prefixed by isCompleted we add the prefix "both_active_tasks_"
colnames(activetask.rawdata) <- gsub(pattern = "isCompleted", replacement = "both_active_tasks_isCompleted", x = colnames(activetask.rawdata))

# Merge daily survey and active task data in order to construct outcome data
outcomedata <- merge(dailysurvey.rawdata, activetask.rawdata, all.x = TRUE, all.y = TRUE, 
                     by = c("username","calendar_date","study_day"),
                     suffixes = c(".dailysurveydata",".activetaskdata"))

# Outcome for Aims 1 & 3
outcomedata$isCompleted <- ifelse(outcomedata$daily_survey_isCompleted==0 & outcomedata$both_active_tasks_isCompleted==0, 0, 1)

# Outcome for Aims 2 & 4
outcomedata$isCompleted_tomorrow <- ifelse(outcomedata$daily_survey_isCompleted_tomorrow==0 & outcomedata$both_active_tasks_isCompleted_tomorrow==0, 0, 1)

# Control Variable on Past Day Survey or Active Task Completion
outcomedata$isCompleted_yesterday <- ifelse(outcomedata$daily_survey_isCompleted_yesterday==0 & outcomedata$both_active_tasks_isCompleted_yesterday==0, 0, 1)

# Drop unnecessary columns
outcomedata <- outcomedata[, !(colnames(outcomedata) %in% c("outcome.with.issue.dailysurveydata", "outcome.with.issue.activetaskdata"))]

# Create an "unknown" level for isCompleted_yesterday
outcomedata$isCompleted_day0 <- ifelse((outcomedata$daily_survey_isCompleted_day0 == 0) & (outcomedata$both_active_tasks_isCompleted_day0 == 0), 0, 1)
outcomedata$isCompleted_yesterday_yes <- outcomedata$isCompleted_yesterday
outcomedata$isCompleted_yesterday_yes <- ifelse(outcomedata$study_day==1, outcomedata$isCompleted_day0, outcomedata$isCompleted_yesterday_yes)

outcomedata$isCompleted_yesterday_unknown <- ifelse((outcomedata$study_day == 1) & (outcomedata$isCompleted_day0 == 0), 1, 0)
outcomedata$isCompleted_yesterday_no <- ifelse((outcomedata$isCompleted_yesterday_unknown==0) & (outcomedata$isCompleted_yesterday_yes==0), 1, 0)

write.csv(outcomedata, file.path(path.dataforanalysis, "outcomedata.csv"), row.names = FALSE)
