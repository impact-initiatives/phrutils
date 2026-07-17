#' @title Validate Factor Input
#' @description
#' Ensures that `x` is a factor variable.
#' @param x Object to test.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_factor <- function(x, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)
  if (!is.factor(x)) {
    msg <- "Expected a factor variable."
    hint_txt <- hint %||% "Use factor() to convert character or numeric inputs before validation."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}

# ---- Validate Columns (new) -----------------------------------------
