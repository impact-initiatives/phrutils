#' @title Validate Entire Vector is Date or Coercible to Date
#' @description
#' Checks that all values in a vector (or column) are Date, POSIX, or can be safely
#' converted to a Date representation using common formats.
#' @param x Vector or data frame column to test.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_all_date <- function(x, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return(invisible(TRUE))
  if (is_safely_coercible(x, "Date")) return(invisible(TRUE))
  msg <- "Expected all values to be Date, POSIX, or coercible to Date format."
  hint_txt <- hint %||% "Ensure date strings follow standard formats like 'YYYY-MM-DD' or 'DD/MM/YYYY'."
  if (soft) {
    phr_warning(message = msg, origin = origin, hint = hint_txt)
    return(invisible(FALSE))
  }
  phr_error(msg, origin = origin, hint = hint_txt)
}
