#' @title Validate No Duplicate Rows
#' @description
#' Ensures there are no fully duplicated rows in the data frame.
#' @param df Data frame to check.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_no_duplicates <- function(df, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  if (any(duplicated(df))) {
    msg <- "Duplicate rows detected in the dataset."
    hint_txt <- hint %||% "Use distinct() or unique() to remove duplicates."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}
