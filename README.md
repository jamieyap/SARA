# Substance Abuse Research Assistant: Curation and Analysis of Micro-Randomized Trial Data

## 0. About Substance Abuse Research Assistant (SARA)

"SARA is a mobile application to increase or sustain engagement of substance data collection overtime." (Rabbi and others, 2018) A micro-randomized trial (MRT) was conducted to assess the effectiveness of employing various engagement strategies integrated with the mobile application.

## 1. About This Repository

This repository contains code for performing curation and analysis of SARA MRT data and documentation. Files corresponding to particular stages of the project are placed under the relevant header.

## 2. Before Proceeding

Questions about the material in this repository can be directed at https://github.com/jamieyap/SARA/issues by submitting a `New issue`.

## 3. Documentation

### 3.1 Essential

File | Description
------------ | -------------
[display-results.pdf](https://github.com/jamieyap/SARA/blob/master/display-results.pdf) | Displays estimates of marginal and moderated causal effects
[check-randomization.pdf](https://github.com/jamieyap/SARA/blob/master/check-randomization.pdf) | Describes checks on quality of randomization of the SARA MRT
[sara-tables-main.pdf](https://github.com/jamieyap/SARA/blob/master/sara-tables-main.pdf) | Displays summary statistics on availability and missing data

### 3.2 More Details

#### 3.2.1 Data Curation

File | Description
------------ | -------------
[create-outcome-variable.R](https://github.com/jamieyap/SARA/blob/master/create-outcome-variable.R) | Construct outcome variables
[create-intervention-variable.R](https://github.com/jamieyap/SARA/blob/master/create-intervention-variable.R) | Construct variables for randomization assignment and availability for intervention
[create-appusage-variable.R](https://github.com/jamieyap/SARA/blob/master/create-appusage-variable.R) | Construct control variable: app usage in the past X hours from time T
[create-contact-variable.R](https://github.com/jamieyap/SARA/blob/master/create-contact-variable.R) | Construct control variable: any contact by study staff in the past X hours from time T
[create-other-variables.R](https://github.com/jamieyap/SARA/blob/master/create-other-variables.R) | Construct other variables
[merge-data-frames.R](https://github.com/jamieyap/SARA/blob/master/merge-data-frames.R) | Merge variables into one data frame in preparation for data analysis

#### 3.2.2 Tests on Input and Output Data

File | Description
------------ | -------------
[test-file-01.R](https://github.com/jamieyap/SARA/blob/master/test-file-01.R) | A collection of tests checking whether input data are as expected
[test-file-02.R](https://github.com/jamieyap/SARA/blob/master/test-file-02.R) | A collection of tests checking whether data curation was performed as planned
[run-test-file.R](https://github.com/jamieyap/SARA/blob/master/run-test-file.R) | Runs `test-file.R` and requires the R package `testthat` (Wickham, 2011). test-file.R can be run with `testthat` version 2.2.1

#### 3.2.3 Check Quality of Randomization and Calculate Summary Statistics
File | Description
------------ | -------------
[check-randomization.Rmd](https://github.com/jamieyap/SARA/blob/master/check-randomization.Rmd) | Check quality of randomization and visualize availability and missing data over time. These are displayed in [check-randomization.pdf](https://github.com/jamieyap/SARA/blob/master/check-randomization.pdf)
[sara-tables-main.Rmd](https://github.com/jamieyap/SARA/blob/master/sara-tables-main.Rmd) | Calculate summary statistics on availability for intervention and missing data. These are displayed in [sara-tables-main.pdf](https://github.com/jamieyap/SARA/blob/master/sara-tables-main.pdf) 

#### 3.2.4 Estimation of Causal Effects

File | Description
------------ | -------------
[data-analysis-complete-case.R](https://github.com/jamieyap/SARA/blob/master/data-analysis-complete-case.R)| Specify marginal and moderated causal effects to estimate using complete case data
[data-analysis-multiple-imputation.R](https://github.com/jamieyap/SARA/blob/master/data-analysis-multiple-imputation.R) | Specify marginal and moderated causal effects to estimate using multiply imputed data
[primary_and_secondary_analysis.R](https://github.com/jamieyap/SARA/blob/master/primary_and_secondary_analysis.R) | Estimate moderated treatment effect and their standard errors
[display-results.Rmd](https://github.com/jamieyap/SARA/blob/master/display-results.Rmd) | Display results from analysis using `data-analysis-complete-case.R` and  `data-analysis-multiple-imputation.R` in [display-results.pdf](https://github.com/jamieyap/SARA/blob/master/display-results.pdf) 

### 3.3 Other
File | Description
------------ | -------------
[data-manip-utils.R](https://github.com/jamieyap/SARA/blob/master/data-manip-utils.R) | Contains functions to perform data manipulation tasks
[io-utils.R](https://github.com/jamieyap/SARA/blob/master/io-utils.R) | Contains functions to perform input/output tasks
[main-utils.R](https://github.com/jamieyap/SARA/blob/master/main-utils.R) | Contains functions to perform data analysis tasks
[file-check-utils.R](https://github.com/jamieyap/SARA/blob/master/file-check-utils.R) | Contains functions to compare different files and sets of files between different folders
[create-user-specific-environ.R](https://github.com/jamieyap/SARA/blob/master/create-user-specific-environ.R) | Create an `.Renviron` file where names of file paths are stored

## 4. References

1. Rabbi M, Meredith P, Predrag K, Erin B, Inbal N, Maureen W, Murphy S. SARA - Substance Abuse Research Assistant. Open Science Framework. 2017 Oct 22; doi: 10.17605/OSF.IO/VWZMD.

2. Rabbi, M., Philyaw Kotov, M., Cunningham, R., Bonar, E. E., Nahum-Shani, I., Klasnja, P., … Murphy, S. (2018). Toward Increasing Engagement in Substance Use Data Collection: Development of the Substance Abuse Research Assistant App and Protocol for a Microrandomized Trial Using Adolescents and Emerging Adults. JMIR research protocols, 7(7), e166. doi:10.2196/resprot.9850

3. Wickham H (2011). “testthat: Get Started with Testing.” The R Journal, 3, 5–10. https://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf.

