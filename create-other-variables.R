path.dataforanalysis <- Sys.getenv("path.dataforanalysis")  
path.otherdata <- Sys.getenv("path.otherdata")  

# Incorporate data on sex of participants
map_qualtricsid_studyid <- read.csv(file.path(path.otherdata, "Qualtrics IDs & Study IDs_Real Trial.csv"), header = TRUE)
map_qualtricsid_studyid <- map_qualtricsid_studyid[,1:2]
map_qualtricsid_studyid$username <- paste("sara-study-", as.character(map_qualtricsid_studyid$StudyID), sep ="")
map_qualtricsid_studyid <- subset(map_qualtricsid_studyid, select = c("QualtricsID", "username"))
map_qualtricsid_studyid$QualtricsID <- as.character(map_qualtricsid_studyid$QualtricsID)

# Do some data cleaning: some Qualtrics IDs start with a white space
# Remove white spaces to facilitate merging with another data frame
# on Qualtrics IDs

x <- as.character(map_qualtricsid_studyid$QualtricsID)
map_qualtricsid_studyid[37,]$QualtricsID <- substr(x[37],2,nchar(x[37]))
map_qualtricsid_studyid[63,]$QualtricsID <- substr(x[37],2,nchar(x[63]))

# Read in sarascreen which contains info on participants' sex
sarascreen <- read.csv(file.path(path.otherdata, "SARASCREEN.csv"), header = TRUE)
sarascreen <- sarascreen[sarascreen$v1 %in% map_qualtricsid_studyid$QualtricsID,]
sarascreen <- subset(sarascreen, select = c("v1", "DEMOSEX"))
sarascreen <- subset(sarascreen, select = c("v1","DEMOSEX"))
colnames(sarascreen) <- c("QualtricsID", "sex")
sarascreen$QualtricsID <- as.character(sarascreen$QualtricsID)

# Merge data frame with mapping of qualtrics IDs and user names to
# data frame with participants' sex and then to dataforanalysis
map_qualtricsid_studyid <- merge(map_qualtricsid_studyid, sarascreen, all.x = TRUE, all.y = TRUE, by = "QualtricsID")
map_qualtricsid_studyid$sex <- ifelse(map_qualtricsid_studyid$sex == "Female", 1, 0)
colnames(map_qualtricsid_studyid)[colnames(map_qualtricsid_studyid) == "sex"] <- "female"
map_qualtricsid_studyid <- map_qualtricsid_studyid[, c("username", "female")]

write.csv(map_qualtricsid_studyid, file.path(path.dataforanalysis,"sex.csv"), row.names = FALSE)
