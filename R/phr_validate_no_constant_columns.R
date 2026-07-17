#' @title Validate No Constant Columns
#' @description
#' Ensures no column in a data frame has the same value for all rows.
#' @param df Data frame to check.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_no_constant_columns <- function(df, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  const_cols <- names(df)[sapply(df, function(x) length(unique(stats::na.omit(x))) <= 1)]
  if (length(const_cols) > 0) {
    msg <- paste0("Constant (uninformative) columns detected: ", paste(const_cols, collapse = ", "))
    hint_txt <- hint %||% "Consider removing or reviewing these columns."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}
