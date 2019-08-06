# This file contains code to perform complete case analysis for a given aim
# Estimation of model parameters is done by primary_and_secondary_analysis.R

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
#drop.criteria.aimX <- NULL

# Aim 2 -------------------------------
#dataforanalysis.aimX <- dataforanalysis.aim2
#drop.criteria.aimX <- (dataforanalysis.aimX[,"username"] != this.participant.withdrew & dataforanalysis.aimX[,"study_day"] == 29) | 
#  (dataforanalysis.aimX[,"username"] == this.participant.withdrew & dataforanalysis.aimX[,"study_day"] == 11) |
#  (dataforanalysis.aimX[,"memegifbug"] == 1)

# Aim 4 -------------------------------
#dataforanalysis.aimX <- dataforanalysis.aim4
#(dataforanalysis.aimX[,"username"] != this.participant.withdrew & dataforanalysis.aimX[,"study_day"] == 29) | 
#  (dataforanalysis.aimX[,"username"] == this.participant.withdrew & dataforanalysis.aimX[,"study_day"] == 11) |
#  (dataforanalysis.aimX[,"memegifbug"] == 1)

# -----------------------------------------------------------------------------
# Create new time variables, create data for complete case analysis and for
# analysis with multiply imputed data
# -----------------------------------------------------------------------------

dataforanalysis.aimX$day.of.week <- strptime(dataforanalysis.aimX$calendar_date, format = "%m/%d/%Y", tz = "EST5EDT")
dataforanalysis.aimX$day.of.week <- strftime(dataforanalysis.aimX$day.of.week, format = "%u")  # Monday = 1
dataforanalysis.aimX$weekend <- ifelse(dataforanalysis.aimX$day.of.week >=6, 1, 0)
dataforanalysis.aimX$sunday <- ifelse(dataforanalysis.aimX$day.of.week ==7, 1, 0)
dataforanalysis.aimX$study_day <- dataforanalysis.aimX$study_day-1  # Subtract 1 from study_day so that it is zero-indexed 
dataforanalysis.aimX$study_day_squared <- (dataforanalysis.aimX$study_day)^2

# Data for complete case analysis ---------------------------------------------
complete.cases.data.aimX <- dataforanalysis.aimX
complete.cases.data.aimX$availability <- replace(complete.cases.data.aimX$availability, is.na(complete.cases.data.aimX$isRandomized), 0)

# Other parameters ------------------------------------------------------------
n <- length(unique(dataforanalysis.aimX$username))

# -----------------------------------------------------------------------------
# Data analysis: main analyses
# -----------------------------------------------------------------------------

if(is.null(drop.criteria.aimX)){
  # Aim 1 or Aim 3
  complete.case.main.aimX <- SARA_primary_hypothesis_1(dta = complete.cases.data.aimX, 
                                                       control_var = c("appusage_yes","isCompleted_yesterday_yes","contact_yes"),
                                                       id_var = "username",
                                                       day_var = "study_day",
                                                       trt_var = "isRandomized",
                                                       outcome_var = "isCompleted",
                                                       avail_var = "availability",
                                                       prob_treatment = 1/2,
                                                       significance_level = 0.025)
  
  p <- length(complete.case.main.aimX$beta)
  q <- length(complete.case.main.aimX$alpha)
  complete.case.main.aimX.est <- c(complete.case.main.aimX$beta, complete.case.main.aimX$alpha)
  complete.case.main.aimX.se <- c(complete.case.main.aimX$beta_se, complete.case.main.aimX$alpha_se)
  complete.case.main.aimX.teststat <- complete.case.main.aimX.est/complete.case.main.aimX.se
  complete.case.main.aimX.p.val <- 2 * pt(abs(complete.case.main.aimX.teststat), df = n - p - q, lower.tail = FALSE)
  
  complete.case.main.aimX <- data.frame(exp = exp(complete.case.main.aimX.est),
                                        beta = complete.case.main.aimX.est,
                                        se.beta = complete.case.main.aimX.se,
                                        test.stat = complete.case.main.aimX.teststat,
                                        p.val = complete.case.main.aimX.p.val)
  
  complete.case.main.aimX <- round(complete.case.main.aimX, digits=3)
  #row.names(complete.case.main.aimX) <- c("beta","alpha1","alpha2","alpha3","alpha4")
  row.names(complete.case.main.aimX) <- c("beta","Intercept","appusage_yes","isCompleted_yesterday_yes","contact_yes")
}else{ 
  # !is.null(drop.criteria.aimX
  # Aim 2 or Aim 4
  complete.case.main.aimX <- SARA_primary_hypothesis_2(dta = complete.cases.data.aimX, 
                                                       control_var = c("appusage_yes", "isCompleted_yesterday_yes", "contact_yes"),
                                                       id_var = "username",
                                                       day_var = "study_day",
                                                       trt_var = "isRandomized",
                                                       survey_completion_var = "isCompleted",
                                                       outcome_var = "isCompleted",
                                                       avail_var = "availability",
                                                       prob_treatment = 1/2,
                                                       significance_level = 0.025)
  
  
  p <- length(complete.case.main.aimX$beta)
  q <- length(complete.case.main.aimX$alpha)
  complete.case.main.aimX.est <- c(complete.case.main.aimX$beta, complete.case.main.aimX$alpha)
  complete.case.main.aimX.se <- c(complete.case.main.aimX$beta_se, complete.case.main.aimX$alpha_se)
  complete.case.main.aimX.teststat <- complete.case.main.aimX.est/complete.case.main.aimX.se
  complete.case.main.aimX.p.val <- 2 * pt(abs(complete.case.main.aimX.teststat), df = n - p - q, lower.tail = FALSE)
  
  complete.case.main.aimX <- data.frame(exp = exp(complete.case.main.aimX.est),
                                        beta = complete.case.main.aimX.est,
                                        se.beta = complete.case.main.aimX.se,
                                        test.stat = complete.case.main.aimX.teststat,
                                        p.val = complete.case.main.aimX.p.val)
  complete.case.main.aimX <- round(complete.case.main.aimX, digits=3)
  #row.names(complete.case.main.aimX) <- c("beta","alpha1","alpha2","alpha3","alpha4")
  row.names(complete.case.main.aimX) <- c("beta","Intercept","appusage_yes", "isCompleted_yesterday_yes", "contact_yes")
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
  complete.cases.data.aimX <- complete.cases.data.aimX[!drop.criteria.aimX,]
  my_outcome_variable <- "isCompleted_tomorrow"
}

# female vs. male
complete.case.exploratory.female.aimX <- SARA_exploratory_analysis_general_F_test(dta = complete.cases.data.aimX, 
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


p <- length(complete.case.exploratory.female.aimX$beta)
q <- length(complete.case.exploratory.female.aimX$alpha)
complete.case.exploratory.female.aimX.est <- c(complete.case.exploratory.female.aimX$beta,
                                               complete.case.exploratory.female.aimX$beta_contrast,
                                               complete.case.exploratory.female.aimX$alpha)
complete.case.exploratory.female.aimX.se <- c(complete.case.exploratory.female.aimX$beta_se,
                                              complete.case.exploratory.female.aimX$se_beta_contrast,
                                              complete.case.exploratory.female.aimX$alpha_se)
complete.case.exploratory.female.aimX.teststat <- complete.case.exploratory.female.aimX.est/complete.case.exploratory.female.aimX.se
complete.case.exploratory.female.aimX.p.val <- 2 * pt(abs(complete.case.exploratory.female.aimX.teststat), df = n - p - q, lower.tail = FALSE)

complete.case.exploratory.female.aimX <- data.frame(exp = exp(complete.case.exploratory.female.aimX.est),
                                                    beta = complete.case.exploratory.female.aimX.est,
                                                    se.beta = complete.case.exploratory.female.aimX.se,
                                                    test.stat = complete.case.exploratory.female.aimX.teststat,
                                                    p.val = complete.case.exploratory.female.aimX.p.val)
complete.case.exploratory.female.aimX <- round(complete.case.exploratory.female.aimX, digits = 3)
#row.names(complete.case.exploratory.female.aimX) <- c("beta1","beta2","alpha1","alpha2","alpha3","alpha4","alpha5")
row.names(complete.case.exploratory.female.aimX) <- c("beta1","beta2",
                                                      "contrast: male", "contrast: female",
                                                      "Intercept","appusage_yes", "isCompleted_yesterday_yes", "contact_yes")

# Weekend vs. weekday
complete.case.exploratory.weekend.aimX <- SARA_exploratory_analysis_general_F_test(dta = complete.cases.data.aimX, 
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
                                                                                                       1,1), nrow=2, byrow=TRUE),
                                                                                   F_test_c = NULL) 


p <- length(complete.case.exploratory.weekend.aimX$beta)
q <- length(complete.case.exploratory.weekend.aimX$alpha)
complete.case.exploratory.weekend.aimX.est <- c(complete.case.exploratory.weekend.aimX$beta, 
                                                complete.case.exploratory.weekend.aimX$beta_contrast,
                                                complete.case.exploratory.weekend.aimX$alpha)
complete.case.exploratory.weekend.aimX.se <- c(complete.case.exploratory.weekend.aimX$beta_se, 
                                               complete.case.exploratory.weekend.aimX$se_beta_contrast,
                                               complete.case.exploratory.weekend.aimX$alpha_se)
complete.case.exploratory.weekend.aimX.teststat <- complete.case.exploratory.weekend.aimX.est/complete.case.exploratory.weekend.aimX.se
complete.case.exploratory.weekend.aimX.p.val <- 2 * pt(abs(complete.case.exploratory.weekend.aimX.teststat), df = n - p - q, lower.tail = FALSE)

complete.case.exploratory.weekend.aimX <- data.frame(exp = exp(complete.case.exploratory.weekend.aimX.est),
                                                     beta = complete.case.exploratory.weekend.aimX.est,
                                                     se.beta = complete.case.exploratory.weekend.aimX.se,
                                                     test.stat = complete.case.exploratory.weekend.aimX.teststat,
                                                     p.val = complete.case.exploratory.weekend.aimX.p.val)
complete.case.exploratory.weekend.aimX <- round(complete.case.exploratory.weekend.aimX, digits = 3)
#row.names(complete.case.exploratory.weekend.aimX) <- c("beta1","beta2","alpha1","alpha2","alpha3","alpha4","alpha5")
row.names(complete.case.exploratory.weekend.aimX) <- c("beta1","beta2",
                                                       "contrast: weekday", "contrast: weekend",
                                                       "Intercept","appusage_yes", "isCompleted_yesterday_yes", "contact_yes")

# appusage_yes = 1 vs. appusage_yes = 0
complete.case.exploratory.appusage_yes.aimX <- SARA_exploratory_analysis_general_F_test(dta = complete.cases.data.aimX, 
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
                                                                                                            1,1), nrow=2, byrow=TRUE),
                                                                                        F_test_c = NULL)



p <- length(complete.case.exploratory.appusage_yes.aimX$beta)
q <- length(complete.case.exploratory.appusage_yes.aimX$alpha)
complete.case.exploratory.appusage_yes.aimX.est <- c(complete.case.exploratory.appusage_yes.aimX$beta, 
                                                     complete.case.exploratory.appusage_yes.aimX$beta_contrast,
                                                     complete.case.exploratory.appusage_yes.aimX$alpha)
complete.case.exploratory.appusage_yes.aimX.se <- c(complete.case.exploratory.appusage_yes.aimX$beta_se, 
                                                    complete.case.exploratory.appusage_yes.aimX$se_beta_contrast,
                                                    complete.case.exploratory.appusage_yes.aimX$alpha_se)
complete.case.exploratory.appusage_yes.aimX.teststat <- complete.case.exploratory.appusage_yes.aimX.est/complete.case.exploratory.appusage_yes.aimX.se
complete.case.exploratory.appusage_yes.aimX.p.val <- 2 * pt(abs(complete.case.exploratory.appusage_yes.aimX.teststat), df = n - p - q, lower.tail = FALSE)

complete.case.exploratory.appusage_yes.aimX <- data.frame(exp = exp(complete.case.exploratory.appusage_yes.aimX.est),
                                                          beta = complete.case.exploratory.appusage_yes.aimX.est,
                                                          se.beta = complete.case.exploratory.appusage_yes.aimX.se,
                                                          test.stat = complete.case.exploratory.appusage_yes.aimX.teststat,
                                                          p.val = complete.case.exploratory.appusage_yes.aimX.p.val)
complete.case.exploratory.appusage_yes.aimX <- round(complete.case.exploratory.appusage_yes.aimX, digits = 3)
#row.names(complete.case.exploratory.appusage_yes.aimX) <- c("beta1","beta2","alpha1","alpha2","alpha3","alpha4","alpha5")
row.names(complete.case.exploratory.appusage_yes.aimX) <- c("beta1","beta2",
                                                            "contrast: appusage_yes=0","contrast: appusage_yes=1",
                                                            "Intercept","appusage_yes", "isCompleted_yesterday_yes", "contact_yes")

# isCompleted_yesterday_yes=1 vs. isCompleted_yesterday_yes=0
complete.case.exploratory.isCompleted_yesterday_yes.aimX <- SARA_exploratory_analysis_general_F_test(dta = complete.cases.data.aimX, 
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
                                                                                                                         1,1), nrow=2, byrow=TRUE),
                                                                                                     F_test_c = NULL)


p <- length(complete.case.exploratory.isCompleted_yesterday_yes.aimX$beta)
q <- length(complete.case.exploratory.isCompleted_yesterday_yes.aimX$alpha)
complete.case.exploratory.isCompleted_yesterday_yes.aimX.est <- c(complete.case.exploratory.isCompleted_yesterday_yes.aimX$beta, 
                                                                  complete.case.exploratory.isCompleted_yesterday_yes.aimX$beta_contrast,
                                                                  complete.case.exploratory.isCompleted_yesterday_yes.aimX$alpha)
complete.case.exploratory.isCompleted_yesterday_yes.aimX.se <- c(complete.case.exploratory.isCompleted_yesterday_yes.aimX$beta_se, 
                                                                 complete.case.exploratory.isCompleted_yesterday_yes.aimX$se_beta_contrast,
                                                                 complete.case.exploratory.isCompleted_yesterday_yes.aimX$alpha_se)
complete.case.exploratory.isCompleted_yesterday_yes.aimX.teststat <- complete.case.exploratory.isCompleted_yesterday_yes.aimX.est/complete.case.exploratory.isCompleted_yesterday_yes.aimX.se
complete.case.exploratory.isCompleted_yesterday_yes.aimX.p.val <- 2 * pt(abs(complete.case.exploratory.isCompleted_yesterday_yes.aimX.teststat), df = n - p - q, lower.tail = FALSE)

complete.case.exploratory.isCompleted_yesterday_yes.aimX <- data.frame(exp = exp(complete.case.exploratory.isCompleted_yesterday_yes.aimX.est),
                                                                       beta = complete.case.exploratory.isCompleted_yesterday_yes.aimX.est,
                                                                       se.beta = complete.case.exploratory.isCompleted_yesterday_yes.aimX.se,
                                                                       test.stat = complete.case.exploratory.isCompleted_yesterday_yes.aimX.teststat,
                                                                       p.val = complete.case.exploratory.isCompleted_yesterday_yes.aimX.p.val)
complete.case.exploratory.isCompleted_yesterday_yes.aimX <- round(complete.case.exploratory.isCompleted_yesterday_yes.aimX, digits = 3)
#row.names(complete.case.exploratory.isCompleted_yesterday_yes.aimX) <- c("beta1","beta2","alpha1","alpha2","alpha3","alpha4","alpha5")
row.names(complete.case.exploratory.isCompleted_yesterday_yes.aimX) <- c("beta1","beta2",
                                                                         "contrast: isCompleted_yesterday_yes=0","contrast: isCompleted_yesterday_yes=1",
                                                                         "Intercept","appusage_yes", "isCompleted_yesterday_yes", "contact_yes")

# contact_yes=1 vs. contact_yes=0
complete.case.exploratory.contact_yes.aimX <- SARA_exploratory_analysis_general_F_test(dta = complete.cases.data.aimX, 
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


p <- length(complete.case.exploratory.contact_yes.aimX$beta)
q <- length(complete.case.exploratory.contact_yes.aimX$alpha)
complete.case.exploratory.contact_yes.aimX.est <- c(complete.case.exploratory.contact_yes.aimX$beta, 
                                                    complete.case.exploratory.contact_yes.aimX$beta_contrast,
                                                    complete.case.exploratory.contact_yes.aimX$alpha)
complete.case.exploratory.contact_yes.aimX.se <- c(complete.case.exploratory.contact_yes.aimX$beta_se, 
                                                   complete.case.exploratory.contact_yes.aimX$se_beta_contrast,
                                                   complete.case.exploratory.contact_yes.aimX$alpha_se)
complete.case.exploratory.contact_yes.aimX.teststat <- complete.case.exploratory.contact_yes.aimX.est/complete.case.exploratory.contact_yes.aimX.se
complete.case.exploratory.contact_yes.aimX.p.val <- 2 * pt(abs(complete.case.exploratory.contact_yes.aimX.teststat), df = n - p - q, lower.tail = FALSE)

complete.case.exploratory.contact_yes.aimX <- data.frame(exp = exp(complete.case.exploratory.contact_yes.aimX.est),
                                                         beta = complete.case.exploratory.contact_yes.aimX.est,
                                                         se.beta = complete.case.exploratory.contact_yes.aimX.se,
                                                         test.stat = complete.case.exploratory.contact_yes.aimX.teststat,
                                                         p.val = complete.case.exploratory.contact_yes.aimX.p.val)
complete.case.exploratory.contact_yes.aimX <- round(complete.case.exploratory.contact_yes.aimX, digits = 3)
#row.names(complete.case.exploratory.contact_yes.aimX) <- c("beta1","beta2","alpha1","alpha2","alpha3","alpha4","alpha5")
row.names(complete.case.exploratory.contact_yes.aimX) <- c("beta1","beta2",
                                                           "contrast: contact_yes=0","contrast: contact_yes=1",
                                                           "Intercept","appusage_yes", "isCompleted_yesterday_yes", "contact_yes")

# Time trend: study_day
complete.case.exploratory.study_day.aimX <- SARA_exploratory_analysis_general_F_test(dta = complete.cases.data.aimX, 
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


p <- length(complete.case.exploratory.study_day.aimX$beta)
q <- length(complete.case.exploratory.study_day.aimX$alpha)
complete.case.exploratory.study_day.aimX.est <- c(complete.case.exploratory.study_day.aimX$beta, 
                                                  complete.case.exploratory.study_day.aimX$alpha)
complete.case.exploratory.study_day.aimX.se <- c(complete.case.exploratory.study_day.aimX$beta_se, 
                                                 complete.case.exploratory.study_day.aimX$alpha_se)
complete.case.exploratory.study_day.aimX.teststat <- complete.case.exploratory.study_day.aimX.est/complete.case.exploratory.study_day.aimX.se
complete.case.exploratory.study_day.aimX.p.val <- 2 * pt(abs(complete.case.exploratory.study_day.aimX.teststat), df = n - p - q, lower.tail = FALSE)

complete.case.exploratory.study_day.aimX <- data.frame(exp = exp(complete.case.exploratory.study_day.aimX.est),
                                                       beta = complete.case.exploratory.study_day.aimX.est,
                                                       se.beta = complete.case.exploratory.study_day.aimX.se,
                                                       test.stat = complete.case.exploratory.study_day.aimX.teststat,
                                                       p.val = complete.case.exploratory.study_day.aimX.p.val)
complete.case.exploratory.study_day.aimX <- round(complete.case.exploratory.study_day.aimX, digits = 3)
#row.names(complete.case.exploratory.study_day.aimX) <- c("beta1","beta2","alpha1","alpha2","alpha3","alpha4","alpha5")
row.names(complete.case.exploratory.study_day.aimX) <- c("beta1","beta2",
                                                         "Intercept","appusage_yes", "isCompleted_yesterday_yes", "contact_yes")

# Time trend: study_day and study_day_squared
complete.case.exploratory.study_day_squared.aimX <- SARA_exploratory_analysis_general_F_test(dta = complete.cases.data.aimX, 
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


p <- length(complete.case.exploratory.study_day_squared.aimX$beta)
q <- length(complete.case.exploratory.study_day_squared.aimX$alpha)
complete.case.exploratory.study_day_squared.aimX.est <- c(complete.case.exploratory.study_day_squared.aimX$beta, 
                                                          complete.case.exploratory.study_day_squared.aimX$alpha)
complete.case.exploratory.study_day_squared.aimX.se <- c(complete.case.exploratory.study_day_squared.aimX$beta_se, 
                                                         complete.case.exploratory.study_day_squared.aimX$alpha_se)
complete.case.exploratory.study_day_squared.aimX.teststat <- complete.case.exploratory.study_day_squared.aimX.est/complete.case.exploratory.study_day_squared.aimX.se
complete.case.exploratory.study_day_squared.aimX.p.val <- 2 * pt(abs(complete.case.exploratory.study_day_squared.aimX.teststat), df = n - p - q, lower.tail = FALSE)

complete.case.exploratory.study_day_squared.aimX <- data.frame(exp = exp(complete.case.exploratory.study_day_squared.aimX.est),
                                                               beta = complete.case.exploratory.study_day_squared.aimX.est,
                                                               se.beta = complete.case.exploratory.study_day_squared.aimX.se,
                                                               test.stat = complete.case.exploratory.study_day_squared.aimX.teststat,
                                                               p.val = complete.case.exploratory.study_day_squared.aimX.p.val)
complete.case.exploratory.study_day_squared.aimX <- round(complete.case.exploratory.study_day_squared.aimX, digits = 3)
#row.names(complete.case.exploratory.study_day_squared.aimX) <- c("beta1","beta2","alpha1","alpha2","alpha3","alpha4","alpha5")
row.names(complete.case.exploratory.study_day_squared.aimX) <- c("beta1","beta2","beta3",
                                                                 "Intercept","appusage_yes", "isCompleted_yesterday_yes", "contact_yes")

# -----------------------------------------------------------------------------
# Save all objects into a list
# -----------------------------------------------------------------------------

results.names <- c("complete.case.main.aimX",
                   "complete.case.exploratory.appusage_yes.aimX",
                   "complete.case.exploratory.contact_yes.aimX",
                   "complete.case.exploratory.isCompleted_yesterday_yes.aimX",
                   "complete.case.exploratory.female.aimX",
                   "complete.case.exploratory.study_day.aimX",
                   "complete.case.exploratory.study_day_squared.aimX",
                   "complete.case.exploratory.weekend.aimX"
                   )
objects.aimX <- lapply(results.names, function(this.string){
  return(get(this.string))
})

names(objects.aimX) <- results.names

table.labels <- c("Complete Case Analysis: Main Analysis",
                  "Complete Case Analysis: appusage_yes=1 vs. appusage_yes=0",
                  "Complete Case Analysis: contact_yes=1 vs. contact_yes=0",
                  "Complete Case Analysis: isCompleted_yesterday_yes=1 vs. isCompleted_yesterday_yes=0",
                  "Complete Case Analysis: female=1 vs. female=0",
                  "Complete Case Analysis: study_day",
                  "Complete Case Analysis: study_day_squared",
                  "Complete Case Analysis: weekend=1 vs. weekend=0"
                  )