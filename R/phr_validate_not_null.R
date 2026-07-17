#' @title Validate That an Object Is Not NULL or NA
#' @description
#' Checks that an object exists and is not missing. Handles all object types safely.
#'
#' @param x Object to check.
#' @param origin Optional name of the calling function for context.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#'
#' @return Invisibly TRUE if valid; otherwise throws [phr_error()] or [phr_warning()].
#' @export
phr_validate_not_null <- function(x, origin = NULL, soft) {
  if (is.null(x)) {
    msg <- "Received NULL input, expected a valid object."
    hint <- "Ensure the object exists before validation."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint)
  }

  # Only check for NA if x is atomic (vector, not list/data.frame)
  if (is.atomic(x) && length(x) == 1 && is.na(x)) {
    msg <- "Received NA input, expected a non-missing value."
    hint <- "Ensure missing values are handled before validation."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint)
  }

  invisible(TRUE)
}
