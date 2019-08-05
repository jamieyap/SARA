path.code <- Sys.getenv("path.code")  # Location of all code
path.dataforanalysis <- Sys.getenv("path.dataforanalysis")  # Location of files to be used for data analysis
setwd(path.dataforanalysis)

# Data for all aims -----------------------------------------------------------
outcomedata <- read.csv("outcomedata.csv", header = TRUE)  # Outcome data for all aims
sexdata <- read.csv("sex.csv", header = TRUE)

# Data for aim 1 --------------------------------------------------------------
fourPMdata <- read.csv("fourPMdata.csv", header = TRUE)  # Intervention assignment and availability for aim 1
appusagedata.aim1 <- read.csv("appusage.aim1.csv", header = TRUE)  # App usage control variable for aim 1
contactdata.aim1 <- read.csv("contactdata.aim1.csv", header = TRUE)  # Study staff contacting participants control variable for aim 1

dataforanalysis.aim1 <- merge(outcomedata, fourPMdata, all.x = TRUE, all.y = TRUE, by = c("username", "calendar_date", "study_day"))
dataforanalysis.aim1 <- merge(dataforanalysis.aim1, appusagedata.aim1, all.x = TRUE, all.y = TRUE, by = c("username", "calendar_date", "study_day"))
dataforanalysis.aim1 <- merge(dataforanalysis.aim1, contactdata.aim1, all.x = TRUE, all.y = TRUE, by = c("username", "calendar_date", "study_day"))
dataforanalysis.aim1 <- merge(dataforanalysis.aim1, sexdata, all.x = TRUE, all.y = FALSE, by = c("username"))
dataforanalysis.aim1 <- dataforanalysis.aim1[order(dataforanalysis.aim1$username, dataforanalysis.aim1$study_day),]
dataforanalysis.aim1$day.squared <- (dataforanalysis.aim1$study_day)^2

# Data for aim 2 --------------------------------------------------------------
memegifdata <- read.csv("memegifdata.csv", header = TRUE)  # Intervention assignment and availability for aim 2
appusagedata.aim2 <- read.csv("appusage.aim2.csv", header = TRUE)  # Control variable for aim 2
contactdata.aim2 <- read.csv("contactdata.aim2.csv", header = TRUE)  # Study staff contacting participants control variable for aim 2

dataforanalysis.aim2 <- merge(outcomedata, memegifdata, all.x = TRUE, all.y = TRUE, by = c("username", "calendar_date", "study_day"))
dataforanalysis.aim2 <- merge(dataforanalysis.aim2, appusagedata.aim2, all.x = TRUE, all.y = TRUE, by = c("username", "calendar_date", "study_day"))
dataforanalysis.aim2 <- merge(dataforanalysis.aim2, contactdata.aim2, all.x = TRUE, all.y = TRUE, by = c("username", "calendar_date", "study_day"))
dataforanalysis.aim2 <- merge(dataforanalysis.aim2, sexdata, all.x = TRUE, all.y = FALSE, by = c("username"))
dataforanalysis.aim2 <- dataforanalysis.aim2[order(dataforanalysis.aim2$username, dataforanalysis.aim2$study_day),]
dataforanalysis.aim2$day.squared <- (dataforanalysis.aim2$study_day)^2

# Data for aim 4 --------------------------------------------------------------
lifeinsight <- read.csv("lifeinsight.csv", header = TRUE)  # Intervention assignment and availability for aim 4
appusagedata.aim4 <- read.csv("appusage.aim4.csv", header = TRUE)  # Control variable for aim 4
contactdata.aim4 <- read.csv("contactdata.aim4.csv", header = TRUE)  # Study staff contacting participants control variable for aim 4

dataforanalysis.aim4 <- merge(lifeinsight, outcomedata, all.x = TRUE, all.y = FALSE, by = c("username", "calendar_date", "study_day"))
dataforanalysis.aim4 <- merge(dataforanalysis.aim4, appusagedata.aim4, all.x = TRUE, all.y = FALSE, by = c("username", "calendar_date", "study_day"))
dataforanalysis.aim4 <- merge(dataforanalysis.aim4, contactdata.aim4, all.x = TRUE, all.y = FALSE, by = c("username", "calendar_date", "study_day"))
dataforanalysis.aim4 <- merge(dataforanalysis.aim4, sexdata, all.x = TRUE, all.y = FALSE, by = c("username"))
dataforanalysis.aim4 <- dataforanalysis.aim4[order(dataforanalysis.aim4$username, dataforanalysis.aim4$study_day),]
dataforanalysis.aim4$day.squared <- (dataforanalysis.aim4$study_day)^2

# Save datasets created -------------------------------------------------------
write.csv(dataforanalysis.aim1, file.path(path.dataforanalysis, "dataforanalysis.aim1.csv"), row.names = FALSE)
write.csv(dataforanalysis.aim2, file.path(path.dataforanalysis, "dataforanalysis.aim2.csv"), row.names = FALSE)
write.csv(dataforanalysis.aim4, file.path(path.dataforanalysis, "dataforanalysis.aim4.csv"), row.names = FALSE)
