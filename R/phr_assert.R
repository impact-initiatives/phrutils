#' @title IPHRA Shiny-Aware Assertion
#' @description
#' Tests a condition and displays a safe, Shiny-compatible error
#' if the assertion fails.
#' Works similarly to `stopifnot()` but with standardized IPHRA messaging and logging.
#'
#' @param condition Logical expression to evaluate.
#' @param message Error message to show if the condition is `FALSE`.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective suggestion for the user.
#'
#' @return No return value. Stops execution locally if the condition fails.
#' @export
phr_assert <- function(condition, message, origin = NULL, hint = NULL) {
  if (!isTRUE(condition)) {
    phr_error(message = message, type = "AssertionError", origin = origin, hint = hint)
  }
}
