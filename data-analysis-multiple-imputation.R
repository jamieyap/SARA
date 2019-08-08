# This file contains code to perform analysis with mulyiple imputed data for 
# a given aim. Estimation of model parameters is done by 
# primary_and_secondary_analysis.R

# -----------------------------------------------------------------------------
# Preliminary steps: Read in file paths and data for analysis
# -----------------------------------------------------------------------------

path.code <- Sys.getenv("path.code")  # Location of all code
path.dataforanalysis <- Sys.getenv("path.dataforanalysis")  # Location of files to be used for data analysis
path.analysis <- Sys.getenv("path.analysis")
this.participant.withdrew <- Sys.getenv("this.participant.withdrew")
source(file.path(path.code, "main-utils.R"))
source(file.path(path.analysis, "primary_and_secondary_analysis.R"))

# Aim 1: Read in data for analysis
#dataforanalysis.aim1 <- read.csv(file.path(path.dataforanalysis,"dataforanalysis.aim1.csv"))
#dataforanalysis.aim1$username <- as.character(dataforanalysis.aim1$username)

# Aim 2: Read in data for analysis
#dataforanalysis.aim2 <- read.csv(file.path(path.dataforanalysis,"dataforanalysis.aim2.csv"))
#dataforanalysis.aim2$username <- as.character(dataforanalysis.aim2$username)

# Aim 4: Read in data for analysis
#dataforanalysis.aim4 <- read.csv(file.path(path.dataforanalysis,"dataforanalysis.aim4.csv"))
#dataforanalysis.aim4$username <- as.character(dataforanalysis.aim4$username)

# ----------------------------------------------------------------------------- 
# Portion of code to change: uncomment section corresponding to desired aim           
# ----------------------------------------------------------------------------- 

# Aim 1 -------------------------------
#dataforanalysis.aimX <- dataforanalysis.aim1
# In data analyses, study_day is zero-indexed
#dataforanalysis.aimX$study_day <- dataforanalysis.aimX$study_day - 1
#dataforanalysis.aimX$study_day_squared <- (dataforanalysis.aimX$study_day)^2
#drop.criteria.aimX <- NULL

# Aim 2 -------------------------------
#dataforanalysis.aimX <- dataforanalysis.aim2
# In data analyses, study_day is zero-indexed
#dataforanalysis.aimX$study_day <- dataforanalysis.aimX$study_day - 1
#dataforanalysis.aimX$study_day_squared <- (dataforanalysis.aimX$study_day)^2
#drop.criteria.aimX <- (dataforanalysis.aimX[,"username"] != this.participant.withdrew & dataforanalysis.aimX[,"study_day"] == 28) | 
#  (dataforanalysis.aimX[,"username"] == this.participant.withdrew & dataforanalysis.aimX[,"study_day"] == 10) |
#  (dataforanalysis.aimX[,"memegifbug"] == 1)

# Aim 4 -------------------------------
# Drop study_day 1 and 2: mathematically equivalent to setting availability=0 for these study days
#dataforanalysis.aim4$availability <- replace(dataforanalysis.aim4$availability,
#                                             (dataforanalysis.aim4$study_day==1) | (dataforanalysis.aim4$study_day==2) ,
#                                             0)
#dataforanalysis.aimX <- dataforanalysis.aim4
# In data analyses, study_day is zero-indexed
#dataforanalysis.aimX$study_day <- dataforanalysis.aimX$study_day - 1
#dataforanalysis.aimX$study_day_squared <- (dataforanalysis.aimX$study_day)^2
#drop.criteria.aimX <- (dataforanalysis.aimX[,"username"] != this.participant.withdrew & dataforanalysis.aimX[,"study_day"] == 28) | 
#  (dataforanalysis.aimX[,"username"] == this.participant.withdrew & dataforanalysis.aimX[,"study_day"] == 10) |
#  (dataforanalysis.aimX[,"memegifbug"] == 1)

# -----------------------------------------------------------------------------
# Create new time variables, create data for complete case analysis and for
# analysis with multiply imputed data
# -----------------------------------------------------------------------------

dataforanalysis.aimX$day.of.week <- strptime(dataforanalysis.aimX$calendar_date, format = "%m/%d/%Y", tz = "EST5EDT")
dataforanalysis.aimX$day.of.week <- strftime(dataforanalysis.aimX$day.of.week, format = "%u")  # Monday = 1
dataforanalysis.aimX$weekend <- ifelse(dataforanalysis.aimX$day.of.week >=6, 1, 0)
dataforanalysis.aimX$sunday <- ifelse(dataforanalysis.aimX$day.of.week ==7, 1, 0)

# Multiply imputed data -------------------------------------------------------
# Generate 10 imputed datasets
all.seeds <- c(398783,737213,980124,201881,471247,809199,342341,895465,786324,213627)
m <- length(all.seeds)

mi.data.aimX <- mapply(ImputeInterventionAssignment, 
                       use.this.seed = all.seeds, 
                       MoreArgs = list(dataforanalysis.aimX = dataforanalysis.aimX),
                       SIMPLIFY = FALSE)

mi.data.aimX <- lapply(mi.data.aimX, FUN = function(dat){
  return(dat[order(dat$username, dat$study_day),])
})

# Other parameters ------------------------------------------------------------
n <- length(unique(dataforanalysis.aimX$username))

# -----------------------------------------------------------------------------
# Data analysis: main analyses
# -----------------------------------------------------------------------------

if(is.null(drop.criteria.aimX)){
  # Aim 1 or Aim 3
  mi.main.aimX <- lapply(mi.data.aimX, FUN = function(this.imputed.data){
    main.analysis.aimX <- SARA_primary_hypothesis_1(dta = this.imputed.data, 
                                                    control_var = c("appusage_yes","isCompleted_yesterday_yes","contact_yes"),
                                                    id_var = "username",
                                                    day_var = "study_day",
                                                    trt_var = "isRandomized",
                                                    outcome_var = "isCompleted",
                                                    avail_var = "availability",
                                                    prob_treatment = 1/2,
                                                    significance_level = 0.025)
    return(main.analysis.aimX)
  })
  
  
  mi.main.aimX <- do.call(cbind, mi.main.aimX)
  mi.main.aimX <- apply(mi.main.aimX, 2, unlist)
  
  est.mi.main.aimX <- PoolEstimatesMI(inputs.beta = mi.main.aimX[c(1,6:9),],
                                      inputs.stderr = mi.main.aimX[c(2, 10:13),],
                                      n = n,
                                      p = 1,
                                      q = 4,
                                      m = m,
                                      p.val.one.sided = TRUE)
  est.mi.main.aimX <- do.call(cbind, est.mi.main.aimX)
  row.names(est.mi.main.aimX) <- c("beta","Intercept","appusage_yes","isCompleted_yesterday_yes","contact_yes")
  
}else{ 
  # !is.null(drop.criteria.aimX)
  # Aim 2 or Aim 4
  mi.main.aimX <- lapply(mi.data.aimX, FUN = function(this.imputed.data){
    main.analysis.aimX <- SARA_primary_hypothesis_2(dta = this.imputed.data, 
                                                    control_var = c("appusage_yes", "isCompleted_yesterday_yes", "contact_yes"),
                                                    id_var = "username",
                                                    day_var = "study_day",
                                                    trt_var = "isRandomized",
                                                    survey_completion_var = "isCompleted",
                                                    outcome_var = "isCompleted",
                                                    avail_var = "availability",
                                                    prob_treatment = 1/2,
                                                    significance_level = 0.025)
    return(main.analysis.aimX)
  })
  
  
  mi.main.aimX <- do.call(cbind, mi.main.aimX)
  mi.main.aimX <- apply(mi.main.aimX, 2, unlist)
  
  est.mi.main.aimX <- PoolEstimatesMI(inputs.beta = mi.main.aimX[c(1,6:9),],
                                      inputs.stderr = mi.main.aimX[c(2, 10:13),],
                                      n = n,
                                      p = 1,
                                      q = 4,
                                      m = m,
                                      p.val.one.sided = TRUE)
  est.mi.main.aimX <- do.call(cbind, est.mi.main.aimX)
  
  row.names(est.mi.main.aimX) <- c("beta","Intercept","appusage_yes", "isCompleted_yesterday_yes", "contact_yes")
  
}

# -----------------------------------------------------------------------------
# Data analysis: moderators analyses
# -----------------------------------------------------------------------------

# Obtain subset of data if drop.criteria.aimX is provided
if(is.null(drop.criteria.aimX)){
  # Aim 1 or Aim 3
  my_outcome_variable <- "isCompleted"
}else{
  # !is.null(drop.criteria.aimX)
  # Aim 2 or Aim 4
  
  mi.data.aimX <- lapply(mi.data.aimX, FUN = function(dat){
    dat <- dat[!drop.criteria.aimX,]
    return(dat[order(dat$username, dat$study_day),])
  })
  
  my_outcome_variable <- "isCompleted_tomorrow"
}

# -----------------------------------------------------------------------------
# Data analysis: more moderators analyses
# -----------------------------------------------------------------------------

# female vs. male
mi.exploratory.female.aimX <- lapply(mi.data.aimX, FUN = function(this.imputed.data){
  
  aimX.exploratory.female <- SARA_exploratory_analysis_general_F_test(dta = this.imputed.data, 
                                                                      control_var = c("appusage_yes", "isCompleted_yesterday_yes", "contact_yes"),
                                                                      moderator = c("female"),
                                                                      id_var = "username",
                                                                      day_var = "study_day",
                                                                      trt_var = "isRandomized",
                                                                      outcome_var = my_outcome_variable,
                                                                      avail_var = "availability",
                                                                      prob_treatment = 1/2,
                                                                      significance_level = 0.05,
                                                                      F_test_L = matrix(c(1,0,
                                                                                          1,1), nrow=2, byrow=TRUE),
                                                                      F_test_c = NULL) 
  return(aimX.exploratory.female)
})

mi.exploratory.female.aimX <- do.call(cbind, mi.exploratory.female.aimX)
mi.exploratory.female.aimX <- apply(mi.exploratory.female.aimX, 2, unlist)

est.mi.exploratory.female.aimX <- PoolEstimatesMI(inputs.beta = mi.exploratory.female.aimX[c(1:2, 21:22, 13:16),],
                                                  inputs.stderr = mi.exploratory.female.aimX[c(3:4, 23:24, 17:20),],
                                                  n = n,
                                                  p = 2,
                                                  q = 4,
                                                  m = m)
est.mi.exploratory.female.aimX <- do.call(cbind, est.mi.exploratory.female.aimX)
row.names(est.mi.exploratory.female.aimX) <- c("beta1","beta2",
                                               "contrast: male","contrast: female",
                                               "Intercept","appusage_yes", "isCompleted_yesterday_yes", "contact_yes")

# weekend = 1 vs. weekend = 0
mi.exploratory.weekend.aimX <- lapply(mi.data.aimX, FUN = function(this.imputed.data){
  
  aimX.exploratory.weekend <- SARA_exploratory_analysis_general_F_test(dta = this.imputed.data, 
                                                                       control_var = c("appusage_yes", "isCompleted_yesterday_yes", "contact_yes"),
                                                                       moderator = c("weekend"),
                                                                       id_var = "username",
                                                                       day_var = "study_day",
                                                                       trt_var = "isRandomized",
                                                                       outcome_var = my_outcome_variable,
                                                                       avail_var = "availability",
                                                                       prob_treatment = 1/2,
                                                                       significance_level = 0.05,
                                                                       F_test_L = matrix(c(1,0,
                                                                                           1,1), byrow=TRUE, nrow=2),
                                                                       F_test_c = NULL) 
  return(aimX.exploratory.weekend)
})

mi.exploratory.weekend.aimX <- do.call(cbind, mi.exploratory.weekend.aimX)
mi.exploratory.weekend.aimX <- apply(mi.exploratory.weekend.aimX, 2, unlist)

est.mi.exploratory.weekend.aimX <- PoolEstimatesMI(inputs.beta = mi.exploratory.weekend.aimX[c(1:2, 21:22, 13:16),],
                                                   inputs.stderr = mi.exploratory.weekend.aimX[c(3:4, 23:24, 17:20),],
                                                   n = n,
                                                   p = 2,
                                                   q = 4,
                                                   m = m)
est.mi.exploratory.weekend.aimX <- do.call(cbind, est.mi.exploratory.weekend.aimX)
row.names(est.mi.exploratory.weekend.aimX) <- c("beta1","beta2",
                                                "contrast: weekday","contrast: weekend",
                                                "Intercept","appusage_yes", "isCompleted_yesterday_yes", "contact_yes")


# appusage_yes=1 vs. appusage_yes=0

mi.exploratory.appusage_yes.aimX <- lapply(mi.data.aimX, FUN = function(this.imputed.data){
  
  aimX.exploratory.appusage_yes <- SARA_exploratory_analysis_general_F_test(dta = this.imputed.data, 
                                                                            control_var = c("appusage_yes", "isCompleted_yesterday_yes", "contact_yes"),
                                                                            moderator = c("appusage_yes"),
                                                                            id_var = "username",
                                                                            day_var = "study_day",
                                                                            trt_var = "isRandomized",
                                                                            outcome_var = my_outcome_variable,
                                                                            avail_var = "availability",
                                                                            prob_treatment = 1/2,
                                                                            significance_level = 0.05,
                                                                            F_test_L = matrix(c(1,0,
                                                                                                1,1), byrow=TRUE, nrow=2),
                                                                            F_test_c = NULL) 
  return(aimX.exploratory.appusage_yes)
})

mi.exploratory.appusage_yes.aimX <- do.call(cbind, mi.exploratory.appusage_yes.aimX)
mi.exploratory.appusage_yes.aimX <- apply(mi.exploratory.appusage_yes.aimX, 2, unlist)

est.mi.exploratory.appusage_yes.aimX <- PoolEstimatesMI(inputs.beta = mi.exploratory.appusage_yes.aimX[c(1:2,21:22,13:16),],
                                                        inputs.stderr = mi.exploratory.appusage_yes.aimX[c(3:4,23:24,17:20),],
                                                        n = n,
                                                        p = 2,
                                                        q = 4,
                                                        m = m)
est.mi.exploratory.appusage_yes.aimX <- do.call(cbind, est.mi.exploratory.appusage_yes.aimX)
row.names(est.mi.exploratory.appusage_yes.aimX) <- c("beta1","beta2",
                                                     "contrast: appusage_yes=0","contrast: appusage_yes=1",
                                                     "Intercept","appusage_yes", "isCompleted_yesterday_yes", "contact_yes")

# isCompleted_yesterday_yes=1 vs. isCompleted_yesterday_yes=0
mi.exploratory.isCompleted_yesterday_yes.aimX <- lapply(mi.data.aimX, FUN = function(this.imputed.data){
  
  aimX.exploratory.isCompleted_yesterday_yes <- SARA_exploratory_analysis_general_F_test(dta = this.imputed.data, 
                                                                                         control_var = c("appusage_yes", "isCompleted_yesterday_yes", "contact_yes"),
                                                                                         moderator = c("isCompleted_yesterday_yes"),
                                                                                         id_var = "username",
                                                                                         day_var = "study_day",
                                                                                         trt_var = "isRandomized",
                                                                                         outcome_var = my_outcome_variable,
                                                                                         avail_var = "availability",
                                                                                         prob_treatment = 1/2,
                                                                                         significance_level = 0.05,
                                                                                         F_test_L = matrix(c(1,0,
                                                                                                             1,1), byrow=TRUE, nrow=2),
                                                                                         F_test_c = NULL) 
  return(aimX.exploratory.isCompleted_yesterday_yes)
})

mi.exploratory.isCompleted_yesterday_yes.aimX <- do.call(cbind, mi.exploratory.isCompleted_yesterday_yes.aimX)
mi.exploratory.isCompleted_yesterday_yes.aimX <- apply(mi.exploratory.isCompleted_yesterday_yes.aimX, 2, unlist)

est.mi.exploratory.isCompleted_yesterday_yes.aimX <- PoolEstimatesMI(inputs.beta = mi.exploratory.isCompleted_yesterday_yes.aimX[c(1:2,21:22,13:16),],
                                                                     inputs.stderr = mi.exploratory.isCompleted_yesterday_yes.aimX[c(3:4, 23:24, 17:20),],
                                                                     n = n,
                                                                     p = 2,
                                                                     q = 4,
                                                                     m = m)
est.mi.exploratory.isCompleted_yesterday_yes.aimX <- do.call(cbind, est.mi.exploratory.isCompleted_yesterday_yes.aimX)
row.names(est.mi.exploratory.isCompleted_yesterday_yes.aimX) <- c("beta1","beta2",
                                                                  "contrast: isCompleted_yesterday_yes=0","contrast: isCompleted_yesterday_yes=1",
                                                                  "Intercept","appusage_yes", "isCompleted_yesterday_yes", "contact_yes")

# contact_yes=1 vs. contact_yes=0

mi.exploratory.contact_yes.aimX <- lapply(mi.data.aimX, FUN = function(this.imputed.data){
  
  aimX.exploratory.contact_yes <- SARA_exploratory_analysis_general_F_test(dta = this.imputed.data, 
                                                                           control_var = c("appusage_yes", "isCompleted_yesterday_yes", "contact_yes"),
                                                                           moderator = c("contact_yes"),
                                                                           id_var = "username",
                                                                           day_var = "study_day",
                                                                           trt_var = "isRandomized",
                                                                           outcome_var = my_outcome_variable,
                                                                           avail_var = "availability",
                                                                           prob_treatment = 1/2,
                                                                           significance_level = 0.05,
                                                                           F_test_L = matrix(c(1,0,
                                                                                               1,1), byrow=TRUE, nrow=2),
                                                                           F_test_c = NULL) 
  return(aimX.exploratory.contact_yes)
})

mi.exploratory.contact_yes.aimX <- do.call(cbind, mi.exploratory.contact_yes.aimX)
mi.exploratory.contact_yes.aimX <- apply(mi.exploratory.contact_yes.aimX, 2, unlist)

est.mi.exploratory.contact_yes.aimX <- PoolEstimatesMI(inputs.beta = mi.exploratory.contact_yes.aimX[c(1:2,21:22,13:16),],
                                                       inputs.stderr = mi.exploratory.contact_yes.aimX[c(3:4,23:24,17:20),],
                                                       n = n,
                                                       p = 2,
                                                       q = 4,
                                                       m = m)
est.mi.exploratory.contact_yes.aimX <- do.call(cbind, est.mi.exploratory.contact_yes.aimX)
row.names(est.mi.exploratory.contact_yes.aimX) <- c("beta1","beta2",
                                                    "contrast: contact_yes=0","contrast: contact_yes=1",
                                                    "Intercept","appusage_yes", "isCompleted_yesterday_yes", "contact_yes")

# Time trend: study_day
mi.exploratory.study_day.aimX <- lapply(mi.data.aimX, FUN = function(this.imputed.data){
  
  aimX.exploratory.study_day <- SARA_exploratory_analysis_general_F_test(dta = this.imputed.data, 
                                                                         control_var = c("appusage_yes", "isCompleted_yesterday_yes", "contact_yes"),
                                                                         moderator = c("study_day"),
                                                                         id_var = "username",
                                                                         day_var = "study_day",
                                                                         trt_var = "isRandomized",
                                                                         outcome_var = my_outcome_variable,
                                                                         avail_var = "availability",
                                                                         prob_treatment = 1/2,
                                                                         significance_level = 0.05,
                                                                         F_test_L = c(1,8),
                                                                         F_test_c = NULL) 
  return(aimX.exploratory.study_day)
})

mi.exploratory.study_day.aimX <- do.call(cbind, mi.exploratory.study_day.aimX)
mi.exploratory.study_day.aimX <- apply(mi.exploratory.study_day.aimX, 2, unlist)

est.mi.exploratory.study_day.aimX <- PoolEstimatesMI(inputs.beta = mi.exploratory.study_day.aimX[c(1:2,13:16),],
                                                     inputs.stderr = mi.exploratory.study_day.aimX[c(3:4,17:20),],
                                                     n = n,
                                                     p = 2,
                                                     q = 4,
                                                     m = m)
est.mi.exploratory.study_day.aimX <- do.call(cbind, est.mi.exploratory.study_day.aimX)
row.names(est.mi.exploratory.study_day.aimX) <- c("beta1","beta2",
                                                  "Intercept","appusage_yes", "isCompleted_yesterday_yes", "contact_yes")

# Time trend: study_day and study_day_squared
mi.exploratory.study_day_squared.aimX <- lapply(mi.data.aimX, FUN = function(this.imputed.data){
  
  aimX.exploratory.study_day <- SARA_exploratory_analysis_general_F_test(dta = this.imputed.data, 
                                                                         control_var = c("appusage_yes", "isCompleted_yesterday_yes", "contact_yes"),
                                                                         moderator = c("study_day","study_day_squared"),
                                                                         id_var = "username",
                                                                         day_var = "study_day",
                                                                         trt_var = "isRandomized",
                                                                         outcome_var = my_outcome_variable,
                                                                         avail_var = "availability",
                                                                         prob_treatment = 1/2,
                                                                         significance_level = 0.05,
                                                                         F_test_L = c(1,8,64),
                                                                         F_test_c = NULL) 
  return(aimX.exploratory.study_day)
})

mi.exploratory.study_day_squared.aimX <- do.call(cbind, mi.exploratory.study_day_squared.aimX)
mi.exploratory.study_day_squared.aimX <- apply(mi.exploratory.study_day_squared.aimX, 2, unlist)

est.mi.exploratory.study_day_squared.aimX <- PoolEstimatesMI(inputs.beta = mi.exploratory.study_day_squared.aimX[c(1:3,17:20),],
                                                             inputs.stderr = mi.exploratory.study_day_squared.aimX[c(4:6,21:24),],
                                                             n = n,
                                                             p = 3,
                                                             q = 4,
                                                             m = m)
est.mi.exploratory.study_day_squared.aimX <- do.call(cbind, est.mi.exploratory.study_day_squared.aimX)
row.names(est.mi.exploratory.study_day_squared.aimX) <- c("beta1","beta2","beta3",
                                                          "Intercept","appusage_yes", "isCompleted_yesterday_yes", "contact_yes")

# all four moderators in one model --------------------------------------------

mi.exploratory.all.aimX <- lapply(mi.data.aimX, FUN = function(this.imputed.data){
  
  aimX.exploratory.all <- SARA_exploratory_analysis_general_F_test(dta = this.imputed.data, 
                                                                   control_var = c("appusage_yes", "isCompleted_yesterday_yes", "contact_yes"),
                                                                   moderator = c("weekend",
                                                                                 "contact_yes", 
                                                                                 "appusage_yes",
                                                                                 "isCompleted_yesterday_yes"),
                                                                   id_var = "username",
                                                                   day_var = "study_day",
                                                                   trt_var = "isRandomized",
                                                                   outcome_var = my_outcome_variable,
                                                                   avail_var = "availability",
                                                                   prob_treatment = 1/2,
                                                                   significance_level = 0.05,
                                                                   F_test_L = matrix(c(1,0,0,0,0), byrow=TRUE, nrow=1),
                                                                   F_test_c = NULL) 
  return(aimX.exploratory.all)
})


mi.exploratory.all.aimX <- do.call(cbind, mi.exploratory.all.aimX)
mi.exploratory.all.aimX <- apply(mi.exploratory.all.aimX, 2, unlist)
est.mi.exploratory.all.aimX <- PoolEstimatesMI(inputs.beta = mi.exploratory.all.aimX[c(1:5),],
                                               inputs.stderr = mi.exploratory.all.aimX[c(6:10),],
                                               n = n,
                                               p = 5,
                                               q = 4,
                                               m = m)
est.mi.exploratory.all.aimX <- do.call(cbind, est.mi.exploratory.all.aimX)

# R & V: all four moderators in one model -------------------------------------

mi.exploratory.all.R_V.aimX <- lapply(mi.data.aimX, FUN = function(this.imputed.data){
  
  aimX.exploratory.all <- SARA_exploratory_analysis_general_F_test(dta = this.imputed.data, 
                                                                   control_var = c("appusage_yes", "isCompleted_yesterday_yes", "contact_yes"),
                                                                   moderator = c("weekend",
                                                                                 "contact_yes", 
                                                                                 "appusage_yes",
                                                                                 "isCompleted_yesterday_yes"),
                                                                   id_var = "username",
                                                                   day_var = "study_day",
                                                                   trt_var = "isRandomized",
                                                                   outcome_var = my_outcome_variable,
                                                                   avail_var = "availability",
                                                                   prob_treatment = 1/2,
                                                                   significance_level = 0.05,
                                                                   F_test_L = matrix(c(1,1,1,0,0,
                                                                                       1,1,0,0,0,
                                                                                       1,0,1,0,0), byrow=TRUE, nrow=3),
                                                                   F_test_c = NULL) 
  return(aimX.exploratory.all)
})

mi.exploratory.all.R_V.aimX <- do.call(cbind, mi.exploratory.all.R_V.aimX)
mi.exploratory.all.R_V.aimX <- apply(mi.exploratory.all.R_V.aimX, 2, unlist)
est.mi.exploratory.all.R_V.aimX <- PoolEstimatesMI(inputs.beta = mi.exploratory.all.R_V.aimX[c("beta_contrast1","beta_contrast2","beta_contrast3"),],
                                                   inputs.stderr = mi.exploratory.all.R_V.aimX[c("se_beta_contrast1","se_beta_contrast2","se_beta_contrast3"),],
                                                   n = n,
                                                   p = 5,
                                                   q = 4,
                                                   m = m)
est.mi.exploratory.all.R_V.aimX <- do.call(cbind, est.mi.exploratory.all.R_V.aimX)
row.names(est.mi.exploratory.all.R_V.aimX) <- c("R&V: (1,1,1,0,0)", "R&V: (1,1,0,0,0)", "R&V: (1,0,1,0,0)") 

est.mi.exploratory.all.R_V.aimX_01 <- est.mi.exploratory.all.R_V.aimX

mi.exploratory.all.R_V.aimX <- lapply(mi.data.aimX, FUN = function(this.imputed.data){
  
  aimX.exploratory.all <- SARA_exploratory_analysis_general_F_test(dta = this.imputed.data, 
                                                                   control_var = c("appusage_yes", "isCompleted_yesterday_yes", "contact_yes"),
                                                                   moderator = c("weekend",
                                                                                 "contact_yes", 
                                                                                 "appusage_yes",
                                                                                 "isCompleted_yesterday_yes"),
                                                                   id_var = "username",
                                                                   day_var = "study_day",
                                                                   trt_var = "isRandomized",
                                                                   outcome_var = my_outcome_variable,
                                                                   avail_var = "availability",
                                                                   prob_treatment = 1/2,
                                                                   significance_level = 0.05,
                                                                   F_test_L = matrix(c(1,1,1,0,1,
                                                                                       1,1,0,0,1,
                                                                                       1,0,1,0,1), byrow=TRUE, nrow=3),
                                                                   F_test_c = NULL) 
  return(aimX.exploratory.all)
})

mi.exploratory.all.R_V.aimX <- do.call(cbind, mi.exploratory.all.R_V.aimX)
mi.exploratory.all.R_V.aimX <- apply(mi.exploratory.all.R_V.aimX, 2, unlist)
est.mi.exploratory.all.R_V.aimX <- PoolEstimatesMI(inputs.beta = mi.exploratory.all.R_V.aimX[c("beta_contrast1","beta_contrast2","beta_contrast3"),],
                                                   inputs.stderr = mi.exploratory.all.R_V.aimX[c("se_beta_contrast1","se_beta_contrast2","se_beta_contrast3"),],
                                                   n = n,
                                                   p = 5,
                                                   q = 4,
                                                   m = m)
est.mi.exploratory.all.R_V.aimX <- do.call(cbind, est.mi.exploratory.all.R_V.aimX)
row.names(est.mi.exploratory.all.R_V.aimX) <- c("R&V: (1,1,1,0,1)", "R&V: (1,1,0,0,1)", "R&V: (1,0,1,0,1)") 
est.mi.exploratory.all.R_V.aimX_02 <- est.mi.exploratory.all.R_V.aimX


mi.exploratory.all.R_V.aimX <- lapply(mi.data.aimX, FUN = function(this.imputed.data){
  
  aimX.exploratory.all <- SARA_exploratory_analysis_general_F_test(dta = this.imputed.data, 
                                                                   control_var = c("appusage_yes", "isCompleted_yesterday_yes", "contact_yes"),
                                                                   moderator = c("weekend",
                                                                                 "contact_yes", 
                                                                                 "appusage_yes",
                                                                                 "isCompleted_yesterday_yes"),
                                                                   id_var = "username",
                                                                   day_var = "study_day",
                                                                   trt_var = "isRandomized",
                                                                   outcome_var = my_outcome_variable,
                                                                   avail_var = "availability",
                                                                   prob_treatment = 1/2,
                                                                   significance_level = 0.05,
                                                                   F_test_L = matrix(c(1,1,1,1,0,
                                                                                       1,1,0,1,0,
                                                                                       1,0,1,1,0), byrow=TRUE, nrow=3),
                                                                   F_test_c = NULL) 
  return(aimX.exploratory.all)
})

mi.exploratory.all.R_V.aimX <- do.call(cbind, mi.exploratory.all.R_V.aimX)
mi.exploratory.all.R_V.aimX <- apply(mi.exploratory.all.R_V.aimX, 2, unlist)
est.mi.exploratory.all.R_V.aimX <- PoolEstimatesMI(inputs.beta = mi.exploratory.all.R_V.aimX[c("beta_contrast1","beta_contrast2","beta_contrast3"),],
                                                   inputs.stderr = mi.exploratory.all.R_V.aimX[c("se_beta_contrast1","se_beta_contrast2","se_beta_contrast3"),],
                                                   n = n,
                                                   p = 5,
                                                   q = 4,
                                                   m = m)
est.mi.exploratory.all.R_V.aimX <- do.call(cbind, est.mi.exploratory.all.R_V.aimX)
row.names(est.mi.exploratory.all.R_V.aimX) <- c("R&V: (1,1,1,1,0)", "R&V: (1,1,0,1,0)", "R&V: (1,0,1,1,0)") 

est.mi.exploratory.all.R_V.aimX_03 <- est.mi.exploratory.all.R_V.aimX

# R & NV
mi.exploratory.all.R_NV.aimX <- lapply(mi.data.aimX, FUN = function(this.imputed.data){
  
  aimX.exploratory.all <- SARA_exploratory_analysis_general_F_test(dta = this.imputed.data, 
                                                                   control_var = c("appusage_yes", "isCompleted_yesterday_yes", "contact_yes"),
                                                                   moderator = c("weekend",
                                                                                 "contact_yes", 
                                                                                 "appusage_yes",
                                                                                 "isCompleted_yesterday_yes"),
                                                                   id_var = "username",
                                                                   day_var = "study_day",
                                                                   trt_var = "isRandomized",
                                                                   outcome_var = my_outcome_variable,
                                                                   avail_var = "availability",
                                                                   prob_treatment = 1/2,
                                                                   significance_level = 0.05,
                                                                   F_test_L = matrix(c(1,1,1,1,1,
                                                                                       1,1,0,1,1,
                                                                                       1,0,1,1,1), byrow=TRUE, nrow=3),
                                                                   F_test_c = NULL) 
  return(aimX.exploratory.all)
})

mi.exploratory.all.R_NV.aimX <- do.call(cbind, mi.exploratory.all.R_NV.aimX)
mi.exploratory.all.R_NV.aimX <- apply(mi.exploratory.all.R_NV.aimX, 2, unlist)
est.mi.exploratory.all.R_NV.aimX <- PoolEstimatesMI(inputs.beta = mi.exploratory.all.R_NV.aimX[c("beta_contrast1","beta_contrast2","beta_contrast3"),],
                                                    inputs.stderr = mi.exploratory.all.R_NV.aimX[c("se_beta_contrast1","se_beta_contrast2","se_beta_contrast3"),],
                                                    n = n,
                                                    p = 5,
                                                    q = 4,
                                                    m = m)
est.mi.exploratory.all.R_NV.aimX <- do.call(cbind, est.mi.exploratory.all.R_NV.aimX)
row.names(est.mi.exploratory.all.R_NV.aimX) <- c("R&NV: (1,1,1,1,1)", "R&NV: (1,1,0,1,1)", "R&NV: (1,0,1,1,1)") 

# NR_V
mi.exploratory.all.NR_V.aimX <- lapply(mi.data.aimX, FUN = function(this.imputed.data){
  
  aimX.exploratory.all <- SARA_exploratory_analysis_general_F_test(dta = this.imputed.data, 
                                                                   control_var = c("appusage_yes", "isCompleted_yesterday_yes", "contact_yes"),
                                                                   moderator = c("weekend",
                                                                                 "contact_yes", 
                                                                                 "appusage_yes",
                                                                                 "isCompleted_yesterday_yes"),
                                                                   id_var = "username",
                                                                   day_var = "study_day",
                                                                   trt_var = "isRandomized",
                                                                   outcome_var = my_outcome_variable,
                                                                   avail_var = "availability",
                                                                   prob_treatment = 1/2,
                                                                   significance_level = 0.05,
                                                                   F_test_L = matrix(c(1,0,0,0,0,
                                                                                       1,0,0,0,1,
                                                                                       1,0,0,1,0), byrow=TRUE, nrow=3),
                                                                   F_test_c = NULL) 
  return(aimX.exploratory.all)
})

mi.exploratory.all.NR_V.aimX <- do.call(cbind, mi.exploratory.all.NR_V.aimX)
mi.exploratory.all.NR_V.aimX <- apply(mi.exploratory.all.NR_V.aimX, 2, unlist)
est.mi.exploratory.all.NR_V.aimX <- PoolEstimatesMI(inputs.beta = mi.exploratory.all.NR_V.aimX[c("beta_contrast1","beta_contrast2","beta_contrast3"),],
                                                    inputs.stderr = mi.exploratory.all.NR_V.aimX[c("se_beta_contrast1","se_beta_contrast2","se_beta_contrast3"),],
                                                    n = n,
                                                    p = 5,
                                                    q = 4,
                                                    m = m)
est.mi.exploratory.all.NR_V.aimX <- do.call(cbind, est.mi.exploratory.all.NR_V.aimX)
row.names(est.mi.exploratory.all.NR_V.aimX) <- c("NR&V: (1,0,0,0,0)", "NR&V: (1,0,0,0,1)", "NR&V: (1,0,0,1,0)") 

# NR & NV
mi.exploratory.all.NR_NV.aimX <- lapply(mi.data.aimX, FUN = function(this.imputed.data){
  
  aimX.exploratory.all <- SARA_exploratory_analysis_general_F_test(dta = this.imputed.data, 
                                                                   control_var = c("appusage_yes", "isCompleted_yesterday_yes", "contact_yes"),
                                                                   moderator = c("weekend",
                                                                                 "contact_yes", 
                                                                                 "appusage_yes",
                                                                                 "isCompleted_yesterday_yes"),
                                                                   id_var = "username",
                                                                   day_var = "study_day",
                                                                   trt_var = "isRandomized",
                                                                   outcome_var = my_outcome_variable,
                                                                   avail_var = "availability",
                                                                   prob_treatment = 1/2,
                                                                   significance_level = 0.05,
                                                                   F_test_L = matrix(c(1,0,0,1,1), byrow=TRUE, nrow=1),
                                                                   F_test_c = NULL) 
  return(aimX.exploratory.all)
})

mi.exploratory.all.NR_NV.aimX <- do.call(cbind, mi.exploratory.all.NR_NV.aimX)
mi.exploratory.all.NR_NV.aimX <- apply(mi.exploratory.all.NR_NV.aimX, 2, unlist)
est.mi.exploratory.all.NR_NV.aimX <- PoolEstimatesMI(inputs.beta = matrix(mi.exploratory.all.NR_NV.aimX[c("beta_contrast"),], nrow=1),
                                                     inputs.stderr = matrix(mi.exploratory.all.NR_NV.aimX[c("se_beta_contrast"),], nrow=1),
                                                     n = n,
                                                     p = 5,
                                                     q = 4,
                                                     m = m)
est.mi.exploratory.all.NR_NV.aimX <- do.call(cbind, est.mi.exploratory.all.NR_NV.aimX)
row.names(est.mi.exploratory.all.NR_NV.aimX) <- c("NR&NV: (1,0,0,1,1)") 

row.names(est.mi.exploratory.all.R_V.aimX_01) <- substring(row.names(est.mi.exploratory.all.R_V.aimX_01), 6)
row.names(est.mi.exploratory.all.R_V.aimX_02) <- substring(row.names(est.mi.exploratory.all.R_V.aimX_02), 6)
row.names(est.mi.exploratory.all.R_V.aimX_03) <- substring(row.names(est.mi.exploratory.all.R_V.aimX_03), 6)
row.names(est.mi.exploratory.all.R_NV.aimX) <- substring(row.names(est.mi.exploratory.all.R_NV.aimX), 7)
row.names(est.mi.exploratory.all.NR_V.aimX) <- substring(row.names(est.mi.exploratory.all.NR_V.aimX), 7)
row.names(est.mi.exploratory.all.NR_NV.aimX) <- substring(row.names(est.mi.exploratory.all.NR_NV.aimX), 8)

est.mi.exploratory.all.aimX <- rbind(est.mi.exploratory.all.aimX,
                                     est.mi.exploratory.all.R_V.aimX_01,
                                     est.mi.exploratory.all.R_V.aimX_02,
                                     est.mi.exploratory.all.R_V.aimX_03,
                                     est.mi.exploratory.all.R_NV.aimX,
                                     est.mi.exploratory.all.NR_V.aimX,
                                     est.mi.exploratory.all.NR_NV.aimX)

# -----------------------------------------------------------------------------
# Save all objects into a list
# -----------------------------------------------------------------------------

results.names <- c("est.mi.main.aimX",
                   "est.mi.exploratory.appusage_yes.aimX",
                   "est.mi.exploratory.contact_yes.aimX",
                   "est.mi.exploratory.isCompleted_yesterday_yes.aimX",
                   "est.mi.exploratory.female.aimX",
                   "est.mi.exploratory.study_day.aimX",
                   "est.mi.exploratory.study_day_squared.aimX",
                   "est.mi.exploratory.weekend.aimX",
                   "est.mi.exploratory.all.aimX")
objects.aimX <- lapply(results.names, function(this.string){
  return(get(this.string))
})

names(objects.aimX) <- results.names

table.labels <- c("Analysis with Multiple Imputed Data: Main Analysis",
                  "Analysis with Multiple Imputed Data: appusage_yes=1 vs. appusage_yes=0",
                  "Analysis with Multiple Imputed Data: contact_yes=1 vs. contact_yes=0",
                  "Analysis with Multiple Imputed Data: isCompleted_yesterday_yes=1 vs. isCompleted_yesterday_yes=0",
                  "Analysis with Multiple Imputed Data: female=1 vs. female=0",
                  "Analysis with Multiple Imputed Data: study_day",
                  "Analysis with Multiple Imputed Data: study_day_squared",
                  "Analysis with Multiple Imputed Data: weekend=1 vs. weekend=0",
                  "Analysis with Multiple Imputed Data: Four Moderators in One Model")

