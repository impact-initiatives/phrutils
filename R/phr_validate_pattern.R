#' @title Validate Pattern Match for Character Columns
#' @description
#' Ensures all non-missing values in a character column match a regular expression.
#' @param df Data frame to validate.
#' @param col Character column name.
#' @param pattern Regular expression pattern (e.g., `"^[0-9]\{10\}$"`).
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_pattern <- function(df, col, pattern, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  vals <- df[[col]]
  bad <- !grepl(pattern, vals[!is.na(vals)])
  if (any(bad)) {
    msg <- paste0("Some values in '", col, "' do not match the required pattern: ", pattern)
    hint_txt <- hint %||% "Verify field formatting and encoding."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}
