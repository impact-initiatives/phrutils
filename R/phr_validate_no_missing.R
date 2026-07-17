#' @title Validate No Missing Values
#' @description
#' Checks that specified columns contain no missing (NA) values.
#' @param df Data frame to test.
#' @param cols Character vector of column names to check.
#' @param origin Optional function name.
#' @param hint Optional correction hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid, otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_no_missing <- function(df, cols, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  for (col in cols) {
    if (any(is.na(df[[col]]))) {
      msg <- paste0("Column '", col, "' contains missing (NA) values.")
      hint_txt <- hint %||% "Fill or remove missing data before proceeding."
      if (soft) {
        phr_warning(message = msg, origin = origin, hint = hint_txt)
        return(invisible(FALSE))
      }
      phr_error(msg, origin = origin, hint = hint_txt)
    }
  }
  invisible(TRUE)
}
