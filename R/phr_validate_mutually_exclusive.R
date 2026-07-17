#' @title Validate Mutually Exclusive Columns
#' @description
#' Ensures only one of several related indicator columns is TRUE (or 1) per row.
#' @param df Data frame to test.
#' @param cols Character vector of logical or numeric indicator columns.
#' @param origin Optional origin.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_mutually_exclusive <- function(df, cols, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  count_true <- rowSums(df[cols] == TRUE | df[cols] == 1, na.rm = TRUE)
  if (any(count_true > 1)) {
    msg <- paste0("Multiple TRUE/1 values found across mutually exclusive columns: ", paste(cols, collapse = ", "))
    hint_txt <- hint %||% "Ensure only one column is TRUE per record."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}
