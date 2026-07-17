#' @title Validate Full Data Frame Schema
#' @description
#' Validates a data frame against a declarative schema specification.
#' Each element of the schema list can include:
#'   - `type`: Expected R class (e.g. `"numeric"`, `"character"`, `"Date"`)
#'   - `allowed_values`: Optional allowed set
#'   - `range`: Optional numeric vector of length 2 for min/max
#'
#' @param df Data frame to validate.
#' @param schema Named list specifying expected structure.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_schema <- function(df, schema, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  for (col in names(schema)) {
    if (!col %in% names(df)) {
      msg <- paste("Missing expected column:", col)
      if (soft) {
        phr_warning(message = msg, origin = origin)
        return(invisible(FALSE))
      }
      phr_error(msg, origin = origin)
    }
    spec <- schema[[col]]
    if (!is.null(spec$type)) {
      phr_validate_column_types(df[, col, drop = FALSE], stats::setNames(list(spec$type), col), origin = origin, soft = soft)
    }
    if (!is.null(spec$allowed_values)) {
      phr_validate_all_character(df[[col]], allowed_values = spec$allowed_values, origin = origin, soft = soft)
    }
    if (!is.null(spec$range) && length(spec$range) == 2 && is.numeric(df[[col]])) {
      phr_validate_range(df, col, spec$range[1], spec$range[2], origin = origin, soft = soft)
    }
  }
  invisible(TRUE)
}
