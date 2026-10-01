# Test runner script for testthat
# Run from repository root: Rscript scripts/run_tests.R
# Or in R: source("scripts/run_tests.R")

if (!requireNamespace("testthat", quietly = TRUE)) {
  message("Package 'testthat' is required. Installing...")
  install.packages("testthat")
}

library(testthat)

# Resolve repository root
root_dir <- if (file.exists("R/utils.R")) {
  "."
} else if (file.exists("../R/utils.R")) {
  ".."
} else {
  getwd()
}

source(file.path(root_dir, "R/utils.R"))

message("\n=== Running Unit Tests ===")
test_results <- testthat::test_dir(file.path(root_dir, "tests/testthat"))
print(test_results)
