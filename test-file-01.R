# Place tests related to input data here

library(testthat)

test_that("Check whether files across all locations are identical",{
  context("Integrity of inputs to data curation process")
  # All aims: File with articipants and calendar dates when outcome variable are affected by issues
  path.issues.affecting.outcome.all.aims <- Sys.getenv("path.issues.affecting.outcome.all.aims")  
  
  # Files with participant ID's and dates that are unavailable for intervention
  path.aim1.availability <- Sys.getenv("path.aim1.availability") 
  path.aim2.availability <- Sys.getenv("path.aim2.availability")
  path.aim4.availability <- Sys.getenv("path.aim4.availability")
  
  # Files with participant ID's and dates that have missing intervention assignment among available participant days
  path.aim1.missing.intervention <- Sys.getenv("path.aim1.missing.intervention")
  path.aim2.missing.intervention <- Sys.getenv("path.aim2.missing.intervention")
  path.aim4.missing.intervention <- Sys.getenv("path.aim4.missing.intervention") 
  
  # Files with participant ID's and dates when app usage control covariate is considered unknown
  path.appusage.unknown.aim1 <- Sys.getenv("path.appusage.unknown.aim1")
  path.appusage.unknown.aim2 <- Sys.getenv("path.appusage.unknown.aim2")
  path.appusage.unknown.aim4 <- Sys.getenv("path.appusage.unknown.aim4") 
  
  all.paths.to.check <- c(path.issues.affecting.outcome.all.aims, 
                          # aim 1
                          path.aim1.availability,
                          path.aim1.missing.intervention,
                          path.appusage.unknown.aim1,
                          # aim 2
                          path.aim2.availability,
                          path.aim2.missing.intervention,
                          path.appusage.unknown.aim2,
                          # aim 4
                          path.aim4.availability,
                          path.aim4.missing.intervention,
                          path.appusage.unknown.aim4
  )
  
  grid <- expand.grid(path1 = all.paths.to.check, path2 = all.paths.to.check)
  grid[,1] <- as.character(grid[,1])
  grid[,2] <- as.character(grid[,2])
  # ---------------------------------------------------------------------------  
  path.code <- Sys.getenv("path.code")  # Location of all code
  source(file.path(path.code, "io-utils.R"))
  source(file.path(path.code, "data-manip-utils.R"))
  source(file.path(path.code, "file-check-utils.R"))
  source(file.path(path.code, "main-utils.R"))
  all.results <- mapply(FUN = CheckCommonFiles, path1 = grid[,1], path2 = grid[,2])
  names(all.results) <- NULL
  
  # ---------------------------------------------------------------------------  
  expected.result <- rep(TRUE, nrow(grid))
  expect_identical(object = all.results, expected = expected.result)
})

