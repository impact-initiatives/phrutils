#' @title Validate Non-Negative Values
#' @description
#' Ensures numeric columns have no negative values.
#' @param df Data frame to validate.
#' @param cols Character vector of numeric columns to check.
#' @param origin Optional origin.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_non_negative <- function(df, cols, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  for (col in cols) {
    if (any(df[[col]] < 0, na.rm = TRUE)) {
      msg <- paste0("Column '", col, "' contains negative values.")
      hint_txt <- hint %||% "Ensure counts or quantities are zero or positive."
      if (soft) {
        phr_warning(message = msg, origin = origin, hint = hint_txt)
        return(invisible(FALSE))
      }
      phr_error(msg, origin = origin, hint = hint_txt)
    }
  }
  invisible(TRUE)
}
