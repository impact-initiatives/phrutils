#' @title Validate Numeric Input (Strict)
#' @description
#' Checks that `x` is strictly numeric (not logical, character, or other type).
#' Accepts vectors of numeric values.
#' @param x Object to test.
#' @param origin Optional name of originating function.
#' @param hint Optional corrective hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_numeric <- function(x, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)
  if (!is.numeric(x) || is.logical(x)) {
    msg <- "Expected a strictly numeric value."
    hint_txt <- hint %||% "Ensure input is of type 'numeric'. Logical or character values are not accepted."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}
