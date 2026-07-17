#' @title Validate Date or Date-Time Input (Strict Validation Only)
#' @description
#' Checks whether `x` is a valid date or datetime representation that can be safely
#' converted to a standard `Date` ("YYYY-MM-DD") format.
#'
#' The function performs format recognition without returning the converted value.
#' Supported inputs include:
#' - `Date` objects
#' - `POSIXct` or `POSIXlt` (considered valid)
#' - Character strings that can be parsed into valid dates using common formats
#'   such as `"2025-10-16"`, `"16/10/2025"`, `"2025-10-16T14:32:00Z"`, etc.
#'
#' @param x Object to test.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint to display on failure.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#'
#' @return Invisibly returns TRUE if valid. Triggers `phr_error()` or `phr_warning()` if invalid.
#' @export
phr_validate_date <- function(x, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)

  # Accept existing Date or POSIX objects outright
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    return(invisible(TRUE))
  }

  # Attempt parsing if character vector
  if (is.character(x)) {
    formats <- c(
      "%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d", "%m-%d-%Y", "%d-%m-%Y",
      "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%Y-%m-%dT%H:%M:%OSZ"
    )

    for (fmt in formats) {
      parsed <- suppressWarnings(as.Date(x, format = fmt))
      if (!any(is.na(parsed))) {
        return(invisible(TRUE))
      }
    }
  }

  # If nothing matched, raise structured error or warning
  msg <- "Expected a valid date or datetime convertible to 'YYYY-MM-DD'."
  hint_txt <- hint %||% "Ensure input is a Date, POSIX, or correctly formatted string (e.g. '2025-10-16')."
  if (soft) {
    phr_warning(message = msg, origin = origin, hint = hint_txt)
    return(invisible(FALSE))
  }
  phr_error(msg, origin = origin, hint = hint_txt)
}

# Shared datetime format strings used by phr_validate_datetime(),
# is_safely_coercible(), and phr_convert_datetime().
.phr_datetime_formats <- c(
  "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S",
  "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%OSZ", "%Y-%m-%dT%H:%M:%OS%z",
  "%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %I:%M:%S %p",
  "%m/%d/%Y %I:%M:%S %p", "%d/%m/%Y %H:%M:%S", "%d-%m-%Y %H:%M:%S",
  "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M", "%d/%m/%Y %H:%M"
)
