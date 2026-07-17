#' @title Validate Reference Values
#' @description
#' Ensures all entries in a column exist within a reference vector or table.
#' @param df Data frame to check.
#' @param col Column name.
#' @param ref_values Vector of reference or lookup values.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_reference_values <- function(df, col, ref_values, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  invalid <- setdiff(unique(df[[col]]), ref_values)
  if (length(invalid) > 0) {
    msg <- paste0("Values in '", col, "' not found in reference list: ", paste(invalid, collapse = ", "))
    hint_txt <- hint %||% "Check code lists or join keys for consistency."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}
