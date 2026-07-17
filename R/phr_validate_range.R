#' @title Validate Range of Numeric Values
#' @description
#' Checks that values in a numeric column fall within a specified inclusive range.
#' @param df Data frame to validate.
#' @param col Column name to check.
#' @param min Minimum acceptable value.
#' @param max Maximum acceptable value.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()`.
#' @export
phr_validate_range <- function(df,
                                 col,
                                 min,
                                 max,
                                 origin = NULL,
                                 hint = NULL,
                                 soft) {

  phr_validate_dataframe(df, origin, soft)

  vals <- df[[col]]
  out_of_range <- vals < min | vals > max

  if (any(out_of_range, na.rm = TRUE)) {

    msg <- paste0(
      "Values in '", col, "' are out of range [", min, ", ", max, "]."
    )

    if (isTRUE(soft)) {
      # -----------------------------------------
      # NEW: Soft mode (warning only)
      # -----------------------------------------
      phr_warning(
        message = msg,
        origin = origin,
        hint = hint %||% "Check data entry or range filters."
      )
      return(invisible(FALSE))  # return FALSE for caller logic
    }

    # -----------------------------------------
    # Original behavior: hard error
    # -----------------------------------------
    phr_error(
      message = msg,
      origin = origin,
      hint = hint %||% "Check data entry or range filters."
    )
  }

  invisible(TRUE)
}
