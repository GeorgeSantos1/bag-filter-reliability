#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr %>%
#' @importFrom utils tail
## usethis namespace: end
NULL

# Declare non-standard evaluation variables used in ggplot2 / dplyr / data masks
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".data", "Time", "Wt", "Y"))
}
