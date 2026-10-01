# Test runner script for testthat
# Can be run from RStudio: source("run_tests.R")
# Or from terminal: Rscript run_tests.R

if (!requireNamespace("testthat", quietly = TRUE)) {
  message("Package 'testthat' is required. Installing...")
  install.packages("testthat")
}

library(testthat)
source("R/utils.R")

message("\n=== Running Unit Tests ===")
test_results <- testthat::test_dir("tests/testthat")
print(test_results)
