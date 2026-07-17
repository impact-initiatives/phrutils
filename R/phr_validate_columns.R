#' @title Validate Presence of Required Columns
#' @description
#' Checks whether specified column names exist within a data frame.
#' @param df The data frame to check.
#' @param required_cols Character vector of required column names.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if all columns are present; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_columns <- function(df, required_cols, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(df, origin, soft)
  phr_validate_not_null(required_cols, origin, soft)
  if (!is.data.frame(df)) {
    msg <- "Input must be a data frame for column validation."
    if (soft) {
      phr_warning(message = msg, origin = origin)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin)
  }
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    msg <- paste0("Missing required columns: ", paste(missing, collapse = ", "))
    hint_txt <- hint %||% "Ensure the data frame includes all required fields."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}
