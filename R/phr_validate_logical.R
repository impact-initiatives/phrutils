#' @title Validate Logical Input (Strict)
#' @description
#' Checks that `x` is strictly logical. Does not allow numeric 0/1 substitutes.
#' @param x Object to test.
#' @param origin Optional name of originating function.
#' @param hint Optional corrective hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_logical <- function(x, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)
  if (!is.logical(x)) {
    msg <- "Expected a logical (TRUE/FALSE) value."
    hint_txt <- hint %||% "Convert numeric indicators (0/1) to TRUE/FALSE explicitly."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}
