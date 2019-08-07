library(testthat)
path.code <- Sys.getenv("path.code")
test_file(file.path(path.code,"test-file-01.R"))  
test_file(file.path(path.code,"test-file-02.R"))

