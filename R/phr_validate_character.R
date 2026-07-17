#' @title Validate Character Input (Strict)
#' @description
#' Ensures that `x` is a character vector (not factor, numeric, or logical).
#' @param x Object to test.
#' @param origin Optional name of originating function.
#' @param hint Optional corrective hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_character <- function(x, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)
  if (!is.character(x)) {
    msg <- "Expected a character input (string)."
    hint_txt <- hint %||% "Ensure the input variable is of type 'character'. Factors or numerics are not allowed."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}
