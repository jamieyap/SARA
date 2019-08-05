path.code <- Sys.getenv("path.code")  # Location of all code
path.exclude.all <- Sys.getenv("path.exclude.all")  # Location of files indicating which participant-days to exclude from all aims
path.dataforanalysis <- Sys.getenv("path.dataforanalysis")  # Location of files to be used for data analysis
path.mapdata <- Sys.getenv("path.mapdata") # Location of file mapping days and dates for study participants
path.contactdata <- Sys.getenv("path.contactdata") # Location of file recording study staff contacting participants

# Read in functions -----------------------------------------------------------
source(file.path(path.code, "io-utils.R"))
source(file.path(path.code, "data-manip-utils.R"))
source(file.path(path.code, "file-check-utils.R"))

# Run preprocessing and set variable values needed for all aims ---------------
participant.days.to.exclude <- ReadAll(path.exclude.all)
map.wide <- ReadAll(path.mapdata)
map.long <- DropAll(LongMap(map.wide), participant.days.to.exclude)
map.long$username <- factor(map.long$username)

contactdata <- ReadAll(path.contactdata)
contactdata <- contactdata[, !(colnames(contactdata) %in% "Same.Interaction")]
colnames(contactdata) <- c("username", "calendar_date", "time_of_day")
contactdata$username <- gsub("squad", "study", contactdata$username)
contactdata <- contactdata[!(contactdata$time_of_day == "999"),]  # Remove rows with time of day recorded as "999"
contactdata$eventid<- 1:nrow(contactdata)

contactdata$contact_hrtime <- paste(contactdata$calendar_date, contactdata$time_of_day, sep = " ")
contactdata$contact_hrtime <- strptime(contactdata$contact_hrtime, format = "%m/%d/%Y %I:%M%p", tz="EST5EDT")
contactdata$contact_ts <- as.numeric(contactdata$contact_hrtime)
tmpdf <- contactdata[c("username","eventid","contact_ts")]
colnames(tmpdf) <- c("username","eventid","mid")

# Aim 1 -----------------------------------------------------------------------
map.long.aim1 <- GetTimeBoundsAim1(map.wide = map.wide, TimeFUNAim1 = TimeFUNAim1, data.to.exclude = participant.days.to.exclude)
split.obj.aim1 <- SplitDF(df1 = tmpdf, df2 = map.long.aim1[c("username","study_day","lower","upper")])
all.participant.output.aim1 <- lapply(split.obj.aim1, SplitObjMethod, this.df=2)
all.participant.output.aim1 <- do.call(rbind, all.participant.output.aim1)
map.long.aim1 <- merge(map.long.aim1, all.participant.output.aim1, all.x = TRUE, all.y = FALSE, by = c("username","study_day"))
map.long.aim1$contact_yes <- ifelse(1*(map.long.aim1$isin.interval>0), 1, 0)

# Create contact_unknown and contact_no
map.long.aim1$belowhhours <- mapply(CheckElapsedTime, 
                                    current.timestamp = map.long.aim1$upper, 
                                    reference.timestamp = map.long.aim1$start.study.clock, 
                                    H = 24)
map.long.aim1$contact_unknown <- ifelse((map.long.aim1$contact_yes == 0) & (map.long.aim1$belowhhours == 1), 1, 0)
map.long.aim1$contact_no <- ifelse((map.long.aim1$contact_yes == 0) & (map.long.aim1$contact_unknown == 0), 1, 0)
map.long.aim1 <- map.long.aim1[, c("username", "calendar_date", "study_day", "contact_yes", "contact_no", "contact_unknown")]
map.long.aim1 <- map.long.aim1[order(map.long.aim1$username, map.long.aim1$study_day),]

# Aim 2 -----------------------------------------------------------------------
memegifdata <- read.csv(file.path(path.dataforanalysis, "memegifdata.csv"))
map.long.aim2 <- GetTimeBoundsAim2(map.wide=map.wide, TimeFUNAim2=TimeFUNAim2, memegifdata=memegifdata, data.to.exclude=participant.days.to.exclude)
split.obj.aim2 <- SplitDF(df1 = tmpdf, df2 = map.long.aim2[c("username","study_day","lower","upper")])
all.participant.output.aim2 <- lapply(split.obj.aim2, SplitObjMethod, this.df=2)
all.participant.output.aim2 <- do.call(rbind, all.participant.output.aim2)
map.long.aim2 <- merge(map.long.aim2, all.participant.output.aim2, all.x = TRUE, all.y = FALSE, by = c("username","study_day"))
map.long.aim2$contact_yes <- ifelse(1*(map.long.aim2$isin.interval>0), 1, 0)

# Create contact_unknown and contact_no
map.long.aim2$belowhhours <- mapply(CheckElapsedTime, 
                                    current.timestamp = map.long.aim2$upper, 
                                    reference.timestamp = map.long.aim2$start.study.clock, 
                                    H = 30)
map.long.aim2$contact_unknown <- ifelse((map.long.aim2$contact_yes == 0) & (map.long.aim2$belowhhours == 1), 1, 0)
map.long.aim2$contact_no <- ifelse((map.long.aim2$contact_yes == 0) & (map.long.aim2$contact_unknown == 0), 1, 0)
map.long.aim2 <- map.long.aim2[, c("username", "calendar_date", "study_day", "contact_yes", "contact_no", "contact_unknown")]
map.long.aim2 <- map.long.aim2[order(map.long.aim2$username, map.long.aim2$study_day),]

# Aim 4 -----------------------------------------------------------------------
lifeinsight <- read.csv(file.path(path.dataforanalysis, "lifeinsight.csv"))
map.long.aim4 <- GetTimeBoundsAim4(map.wide=map.wide, TimeFUNAim4=TimeFUNAim4, lifeinsight = lifeinsight, data.to.exclude=participant.days.to.exclude)
split.obj.aim4 <- SplitDF(df1 = tmpdf, df2 = map.long.aim4[c("username","study_day","lower","upper")])
all.participant.output.aim4 <- lapply(split.obj.aim4, SplitObjMethod, this.df=2)
all.participant.output.aim4 <- do.call(rbind, all.participant.output.aim4)
map.long.aim4 <- merge(map.long.aim4, all.participant.output.aim4, all.x = TRUE, all.y = FALSE, by = c("username","study_day"))
map.long.aim4$contact_yes <- ifelse(1*(map.long.aim4$isin.interval>0), 1, 0)

# Create contact_unknown and contact_no
map.long.aim4$belowhhours <- mapply(CheckElapsedTime, 
                                    current.timestamp = map.long.aim4$upper, 
                                    reference.timestamp = map.long.aim4$start.study.clock, 
                                    H = 30)
map.long.aim4$contact_unknown <- ifelse((map.long.aim4$contact_yes == 0) & (map.long.aim4$belowhhours == 1), 1, 0)
map.long.aim4$contact_no <- ifelse((map.long.aim4$contact_yes == 0) & (map.long.aim4$contact_unknown == 0), 1, 0)
map.long.aim4 <- map.long.aim4[, c("username", "calendar_date", "study_day", "contact_yes", "contact_no", "contact_unknown")]
map.long.aim4 <- map.long.aim4[order(map.long.aim4$username, map.long.aim4$study_day),]

# Save variables to csv file --------------------------------------------------
write.csv(map.long.aim1, file.path(path.dataforanalysis, "contactdata.aim1.csv"), row.names = FALSE)
write.csv(map.long.aim2, file.path(path.dataforanalysis, "contactdata.aim2.csv"), row.names = FALSE)
write.csv(map.long.aim4, file.path(path.dataforanalysis, "contactdata.aim4.csv"), row.names = FALSE)
