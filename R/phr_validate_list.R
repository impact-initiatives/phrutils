#' @title Validate That an Object Is a True List (Not a Data Frame)
#' @description
#' Ensures the input object is a plain list. Rejects data frames, tibbles,
#' and other list-like objects used to represent tabular data.
#'
#' @param x Object to validate.
#' @param origin Optional character string indicating where validation was called.
#' @param hint Optional hint to include in the error message for user guidance.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#'
#' @return Invisibly returns TRUE if the object is a list (and not a data frame).
#' Otherwise, triggers [phr_error()] or [phr_warning()].
#' @export
phr_validate_list <- function(x, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)

  # Reject anything that isn't a list, or that is a data.frame
  if (!is.list(x) || is.data.frame(x)) {
    msg <- "Expected a list (not a data frame or tibble)."
    hint_txt <- hint %||% "Ensure the input is a plain list structure (not a data.frame)."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }

  invisible(TRUE)
}
