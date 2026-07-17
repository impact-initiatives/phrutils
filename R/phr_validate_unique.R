#' @title Validate Column Uniqueness
#' @description
#' Ensures one or more columns (or their combination) form a unique identifier.
#' @param df Data frame to validate.
#' @param cols Character vector of columns whose combination must be unique.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_unique <- function(df, cols, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  dupes <- duplicated(df[cols])
  if (any(dupes)) {
    msg <- paste("Duplicate entries detected for key columns:", paste(cols, collapse = ", "))
    hint_txt <- hint %||% "Ensure unique identifiers or combination keys."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}
