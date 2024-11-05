library(jaspTools)
library(testthat)

# required so that date formatting in figures always identical
Sys.setlocale("LC_ALL", "C")
jaspTools::runTestsTravis(module = getwd())
