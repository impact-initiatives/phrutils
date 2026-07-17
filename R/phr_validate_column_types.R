#' @title Validate Column Types
#' @description
#' Ensures specified columns in a data frame match expected R classes.
#' @param df Data frame to validate.
#' @param expected_types Named list of expected types, e.g. `list(age = "numeric", name = "character")`.
#' @param origin Optional name of the calling function.
#' @param hint Optional hint for correction.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_column_types <- function(df, expected_types, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  missing <- setdiff(names(expected_types), names(df))
  if (length(missing) > 0) {
    msg <- paste("Missing expected columns:", paste(missing, collapse = ", "))
    hint_txt <- hint %||% "Ensure all required columns are present."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  for (col in names(expected_types)) {
    actual <- class(df[[col]])[1]
    expected <- expected_types[[col]]
    if (actual != expected) {
      msg <- paste0("Column '", col, "' is of type '", actual, "' but expected '", expected, "'.")
      hint_txt <- hint %||% "Check data import or preprocessing steps."
      if (soft) {
        phr_warning(message = msg, origin = origin, hint = hint_txt)
        return(invisible(FALSE))
      }
      phr_error(msg, origin = origin, hint = hint_txt)
    }
  }
  invisible(TRUE)
}
