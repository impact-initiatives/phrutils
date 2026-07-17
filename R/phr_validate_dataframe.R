#' @title Validate Data Frame Input
#' @description
#' Ensures the object is a data.frame/tibble **and every column is atomic**,
#' not a list, not nested, not a data.frame inside a column.
#'
#' @param x Object to test.
#' @param origin Optional name of originating function.
#' @param hint Optional corrective hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#'
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_dataframe <- function(x, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)

  # Must be a data.frame
  if (!is.data.frame(x)) {
    msg <- "Expected a data frame (or tibble)."
    hint_txt <- hint %||% "Ensure the object is created with data.frame(), tibble(), or similar."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }

  # --- NEW: Validate all columns are atomic (no list columns) ---
  bad_cols <- names(x)[vapply(x, function(col) !is.atomic(col) || is.list(col), logical(1))]
  if (length(bad_cols) > 0) {
    msg <- paste0(
      "The following columns are non-atomic or contain list-like data: ",
      paste(bad_cols, collapse = ", ")
    )
    hint_txt <- "Flatten, unnest, or otherwise convert these columns to atomic vectors before use."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }

  invisible(TRUE)
}
