#' @title Validate Column Dependency
#' @description
#' Ensures if one column has a non-missing value, another dependent column is also filled.
#' @param df Data frame to test.
#' @param col_a Independent column (trigger).
#' @param col_b Dependent column (must be filled if A is non-missing).
#' @param origin Optional origin.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_dependency <- function(df, col_a, col_b, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  missing_dep <- !is.na(df[[col_a]]) & is.na(df[[col_b]])
  if (any(missing_dep)) {
    msg <- paste0("Rows where '", col_a, "' is filled but '", col_b, "' is missing.")
    hint_txt <- hint %||% paste0("Ensure '", col_b, "' is provided when '", col_a, "' is non-missing.")
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}
