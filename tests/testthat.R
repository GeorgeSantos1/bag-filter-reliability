library(testthat)
library(WienerRS)

test_check("WienerRS")
if (file.exists("Rplots.pdf")) unlink("Rplots.pdf")
