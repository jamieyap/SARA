# Place tests related to output data here

library(testthat)

test_that("No duplicates in participant days",{
  context("Sanity check on data for analysis")
  path.dataforanalysis <- Sys.getenv("path.dataforanalysis") 
  dataforanalysis.aim1 <- read.csv(file.path(path.dataforanalysis, "dataforanalysis.aim1.csv"), header = TRUE)
  dataforanalysis.aim2 <- read.csv(file.path(path.dataforanalysis, "dataforanalysis.aim2.csv"), header = TRUE)
  dataforanalysis.aim4 <- read.csv(file.path(path.dataforanalysis, "dataforanalysis.aim4.csv"), header = TRUE)
  
  # Checks start here ---------------------------------------------------------
  # No duplicates in participant days 
  calculated <- sum(duplicated(dataforanalysis.aim1[, c("username", "calendar_date")]))
  expect_equal(object = calculated, expected = 0)
  
  calculated <- sum(duplicated(dataforanalysis.aim2[, c("username", "calendar_date")]))
  expect_equal(object = calculated, expected = 0)
  
  calculated <- sum(duplicated(dataforanalysis.aim4[, c("username", "calendar_date")]))
  expect_equal(object = calculated, expected = 0)
})

test_that("Number of days per participant must be at most 29",{
  context("Sanity check on data for analysis")
  path.dataforanalysis <- Sys.getenv("path.dataforanalysis") 
  dataforanalysis.aim1 <- read.csv(file.path(path.dataforanalysis, "dataforanalysis.aim1.csv"), header = TRUE)
  dataforanalysis.aim2 <- read.csv(file.path(path.dataforanalysis, "dataforanalysis.aim2.csv"), header = TRUE)
  dataforanalysis.aim4 <- read.csv(file.path(path.dataforanalysis, "dataforanalysis.aim4.csv"), header = TRUE)
  
  # Checks start here ---------------------------------------------------------
  calculated <- max(aggregate(study_day ~ username, data = dataforanalysis.aim1, length)[,2])
  expect_lte(object = calculated, expected = 29)
  
  calculated <- max(aggregate(study_day ~ username, data = dataforanalysis.aim2, length)[,2])
  expect_lte(object = calculated, expected = 29)
  
  calculated <- max(aggregate(study_day ~ username, data = dataforanalysis.aim4, length)[,2])
  expect_lte(object = calculated, expected = 29)
})

test_that("Participant days that should be excluded from the data to be used for analyses for all aims have been excluded",{
  context("Sanity check on data for analysis")
  path.exclude.all <- Sys.getenv("path.exclude.all")
  path.dataforanalysis <- Sys.getenv("path.dataforanalysis") 
  dataforanalysis.aim1 <- read.csv(file.path(path.dataforanalysis, "dataforanalysis.aim1.csv"), header = TRUE)
  dataforanalysis.aim2 <- read.csv(file.path(path.dataforanalysis, "dataforanalysis.aim2.csv"), header = TRUE)
  dataforanalysis.aim4 <- read.csv(file.path(path.dataforanalysis, "dataforanalysis.aim4.csv"), header = TRUE)
  
  # Read in functions
  path.code <- Sys.getenv("path.code")  # Location of all code
  source(file.path(path.code, "io-utils.R"))
  source(file.path(path.code, "data-manip-utils.R"))
  source(file.path(path.code, "file-check-utils.R"))
  source(file.path(path.code, "main-utils.R"))
  
  # Checks start here ---------------------------------------------------------
  dataforanalysis.aim1$to.exclude <- 0
  participant.days.to.exclude <- ReadAll(path.to.files = path.exclude.all)
  participant.days.to.exclude <- merge(participant.days.to.exclude, 
                                       dataforanalysis.aim1[, c("username", "study_day", "to.exclude")], 
                                       all.x = TRUE, all.y = FALSE,
                                       by = c("username", "study_day"))
  calculated <- sum(!is.na(participant.days.to.exclude$to.exclude))
  expect_equal(object = calculated, expected = 0)
  
  dataforanalysis.aim2$to.exclude <- 0
  participant.days.to.exclude <- ReadAll(path.to.files = path.exclude.all)
  participant.days.to.exclude <- merge(participant.days.to.exclude, 
                                       dataforanalysis.aim2[, c("username", "study_day", "to.exclude")], 
                                       all.x = TRUE, all.y = FALSE,
                                       by = c("username", "study_day"))
  calculated <- sum(!is.na(participant.days.to.exclude$to.exclude))
  expect_equal(object = calculated, expected = 0)
  
  dataforanalysis.aim4$to.exclude <- 0
  participant.days.to.exclude <- ReadAll(path.to.files = path.exclude.all)
  participant.days.to.exclude <- merge(participant.days.to.exclude, 
                                       dataforanalysis.aim4[, c("username", "study_day", "to.exclude")], 
                                       all.x = TRUE, all.y = FALSE,
                                       by = c("username", "study_day"))
  calculated <- sum(!is.na(participant.days.to.exclude$to.exclude))
  expect_equal(object = calculated, expected = 0)
})

test_that("appusage_yes + appusage_no + appusage_unknown == 1", {
  context("Sanity check on data for analysis")
  path.dataforanalysis <- Sys.getenv("path.dataforanalysis") 
  dataforanalysis.aim1 <- read.csv(file.path(path.dataforanalysis, "dataforanalysis.aim1.csv"), header = TRUE)
  dataforanalysis.aim2 <- read.csv(file.path(path.dataforanalysis, "dataforanalysis.aim2.csv"), header = TRUE)
  dataforanalysis.aim4 <- read.csv(file.path(path.dataforanalysis, "dataforanalysis.aim4.csv"), header = TRUE)
  
  # Checks start here ---------------------------------------------------------
  calculated <- as.numeric(dataforanalysis.aim1$appusage_yes + dataforanalysis.aim1$appusage_no + dataforanalysis.aim1$appusage_unknown)
  expect_identical(object = calculated, expected = rep(1, nrow(dataforanalysis.aim1)))
  
  # For Aim 2, apply this test only when availability==1
  calculated <- as.numeric(dataforanalysis.aim2[dataforanalysis.aim2$availability == 1,"appusage_yes"] + 
                             dataforanalysis.aim2[dataforanalysis.aim2$availability == 1,"appusage_no"] +
                             dataforanalysis.aim2[dataforanalysis.aim2$availability == 1,"appusage_unknown"])
  expect_identical(object = calculated, expected = rep(1, nrow(dataforanalysis.aim2[dataforanalysis.aim2$availability == 1,])))
  
  # For Aim 4, apply this test only when availability==1
  calculated <- as.numeric(dataforanalysis.aim4[dataforanalysis.aim4$availability == 1,"appusage_yes"] + 
                             dataforanalysis.aim4[dataforanalysis.aim4$availability == 1,"appusage_no"] +
                             dataforanalysis.aim4[dataforanalysis.aim4$availability == 1,"appusage_unknown"])
  expect_identical(object = calculated, expected = rep(1, nrow(dataforanalysis.aim4[dataforanalysis.aim4$availability == 1,])))
})

test_that("isCompleted_yesterday_yes + isCompleted_yesterday_no + isCompleted_yesterday_unknown == 1", {
  context("Sanity check on data for analysis")
  path.dataforanalysis <- Sys.getenv("path.dataforanalysis") 
  dataforanalysis.aim1 <- read.csv(file.path(path.dataforanalysis, "dataforanalysis.aim1.csv"), header = TRUE)
  dataforanalysis.aim2 <- read.csv(file.path(path.dataforanalysis, "dataforanalysis.aim2.csv"), header = TRUE)
  dataforanalysis.aim4 <- read.csv(file.path(path.dataforanalysis, "dataforanalysis.aim4.csv"), header = TRUE)
  
  # Checks start here ---------------------------------------------------------
  calculated <- as.numeric(dataforanalysis.aim1$isCompleted_yesterday_yes + dataforanalysis.aim1$isCompleted_yesterday_no + dataforanalysis.aim1$isCompleted_yesterday_unknown)
  expect_identical(object = calculated, expected = rep(1, nrow(dataforanalysis.aim1)))
  
  # For Aim 2, apply this test only when availability==1
  calculated <- as.numeric(dataforanalysis.aim2[dataforanalysis.aim2$availability == 1,"isCompleted_yesterday_yes"] + 
                             dataforanalysis.aim2[dataforanalysis.aim2$availability == 1,"isCompleted_yesterday_no"] +
                             dataforanalysis.aim2[dataforanalysis.aim2$availability == 1,"isCompleted_yesterday_unknown"])
  expect_identical(object = calculated, expected = rep(1, nrow(dataforanalysis.aim2[dataforanalysis.aim2$availability == 1,])))
  
  # For Aim 4, apply this test only when availability==1
  calculated <- as.numeric(dataforanalysis.aim4[dataforanalysis.aim4$availability == 1,"isCompleted_yesterday_yes"] + 
                             dataforanalysis.aim4[dataforanalysis.aim4$availability == 1,"isCompleted_yesterday_no"] +
                             dataforanalysis.aim4[dataforanalysis.aim4$availability == 1,"isCompleted_yesterday_unknown"])
  expect_identical(object = calculated, expected = rep(1, nrow(dataforanalysis.aim4[dataforanalysis.aim4$availability == 1,])))
})

test_that("contact_yes + contact_no + contact_unknown == 1", {
  context("Sanity check on data for analysis")
  path.dataforanalysis <- Sys.getenv("path.dataforanalysis") 
  dataforanalysis.aim1 <- read.csv(file.path(path.dataforanalysis, "dataforanalysis.aim1.csv"), header = TRUE)
  dataforanalysis.aim2 <- read.csv(file.path(path.dataforanalysis, "dataforanalysis.aim2.csv"), header = TRUE)
  dataforanalysis.aim4 <- read.csv(file.path(path.dataforanalysis, "dataforanalysis.aim4.csv"), header = TRUE)
  
  # Checks start here ---------------------------------------------------------
  calculated <- as.numeric(dataforanalysis.aim1$contact_yes + dataforanalysis.aim1$contact_no + dataforanalysis.aim1$contact_unknown)
  expect_identical(object = calculated, expected = rep(1, nrow(dataforanalysis.aim1)))
  
  # For Aim 2, apply this test only when availability==1
  calculated <- as.numeric(dataforanalysis.aim2[dataforanalysis.aim2$availability == 1,"contact_yes"] + 
                             dataforanalysis.aim2[dataforanalysis.aim2$availability == 1,"contact_no"] +
                             dataforanalysis.aim2[dataforanalysis.aim2$availability == 1,"contact_unknown"])
  expect_identical(object = calculated, expected = rep(1, nrow(dataforanalysis.aim2[dataforanalysis.aim2$availability == 1,])))
  
  # For Aim 4, apply this test only when availability==1
  calculated <- as.numeric(dataforanalysis.aim4[dataforanalysis.aim4$availability == 1,"contact_yes"] + 
                             dataforanalysis.aim4[dataforanalysis.aim4$availability == 1,"contact_no"] +
                             dataforanalysis.aim4[dataforanalysis.aim4$availability == 1,"contact_unknown"])
  expect_identical(object = calculated, expected = rep(1, nrow(dataforanalysis.aim4[dataforanalysis.aim4$availability == 1,])))
})

