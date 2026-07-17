#' @title Validate Entire Vector is Logical or Coercible to Logical
#' @description
#' Checks that all values in a vector are logical (TRUE/FALSE) or coercible from
#' standard encodings such as "TRUE"/"FALSE", "T"/"F", or 0/1.
#' @param x Vector or data frame column to test.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_all_logical <- function(x, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)
  if (is.logical(x)) return(invisible(TRUE))
  if (is_safely_coercible(x, "logical")) return(invisible(TRUE))
  msg <- "Expected all values to be logical (TRUE/FALSE) or safely coercible."
  hint_txt <- hint %||% "Ensure inputs are TRUE/FALSE, 1/0, or equivalent character codes."
  if (soft) {
    phr_warning(message = msg, origin = origin, hint = hint_txt)
    return(invisible(FALSE))
  }
  phr_error(msg, origin = origin, hint = hint_txt)
}
