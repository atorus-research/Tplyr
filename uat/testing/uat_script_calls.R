#Validation Script

library(Tplyr)
library(tidyverse, lib.loc = .libPaths()[2])
library(testthat)
library(shinydashboard)
source("~/Tplyr/uat/references/input/helper_test_code.R")

make_test_case_rmd("~/Tplyr/uat/input/test_cases.csv")
make_specification_rmd("~/Tplyr/uat/input/specs.csv")

# Source all of the files in the rtf_test_files directory
lapply(list.files("~/Tplyr/uat/test_cases.R", full.names = TRUE,
                  pattern = ".R$"), source)

shiny::runApp("~/Tplyr/uat/references/input/app.R")
