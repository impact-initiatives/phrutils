#' @title Validate No Missing Values in Required Columns
#' @description
#' Checks that required columns in a data frame contain no NA values.
#'
#' @param df A data frame to validate.
#' @param required_cols Character vector of column names that must not contain NA.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective suggestion.
#' @param soft Logical; if TRUE, issues a warning instead of an error.
#'
#' @return Invisibly returns TRUE if valid, FALSE if soft validation fails.
#' @export
phr_validate_required_na <- function(df, required_cols, origin = NULL, hint = NULL, soft) {

  phr_validate_dataframe(df, origin = origin, soft = soft)

  na_rows <- which(
    apply(df[required_cols], 1, function(x) any(is.na(x)))
  )

  if (length(na_rows) > 0) {
    msg <- paste0(
      "Missing values (NA) detected in required fields at rows: ",
      paste(na_rows, collapse = ", ")
    )
    hint_txt <- hint %||% "Ensure all required fields are populated."

    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }

    phr_error(message = msg, origin = origin, hint = hint_txt)
  }

  invisible(TRUE)
}
