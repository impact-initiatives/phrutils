#' @title Validate Join Keys
#' @description
#' Ensures all key values from one data frame exist in a reference data frame before joining.
#' @param df Data frame containing key column to check.
#' @param ref_df Reference data frame containing valid key column.
#' @param key_col Name of the key column.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_join_keys <- function(df, ref_df, key_col, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  phr_validate_dataframe(ref_df, origin, soft)
  missing_keys <- setdiff(unique(df[[key_col]]), unique(ref_df[[key_col]]))
  if (length(missing_keys) > 0) {
    msg <- paste0("Join key values in '", key_col, "' not found in reference data: ", paste(missing_keys, collapse = ", "))
    hint_txt <- hint %||% "Check for mismatched or missing join identifiers."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}
