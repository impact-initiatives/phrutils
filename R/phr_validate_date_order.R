#' @title Validate Date Order
#' @description
#' Ensures that all rows have start_date <= end_date.
#' @param df Data frame containing the two date columns.
#' @param start_col Name of the start date column.
#' @param end_col Name of the end date column.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_date_order <- function(df, start_col, end_col, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  if (any(df[[start_col]] > df[[end_col]], na.rm = TRUE)) {
    msg <- paste0("Start date in column '", start_col, "' exceeds end date in '", end_col, "'.")
    hint_txt <- hint %||% "Ensure start and end dates are in logical order."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}
