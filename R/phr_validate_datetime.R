#' @title Validate Date-Time Input
#' @description
#' Checks whether `x` is a valid datetime object (`POSIXct` or `POSIXlt`),
#' or a character string that can be parsed as a datetime with both date
#' and time components.
#'
#' Unlike `phr_validate_date`, this function rejects bare `Date` objects
#' and date-only strings without a time component.
#'
#' Supported inputs include:
#' - `POSIXct` or `POSIXlt` objects
#' - Character strings with time components such as `"2025-10-16 14:32:00"`,
#'   `"2025-10-16T14:32:00Z"`, `"16/10/2025 14:32"`, etc.
#'
#' @param x Object to test.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint to display on failure.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#'
#' @return Invisibly returns TRUE if valid. Triggers `phr_error()` or `phr_warning()` if invalid.
#' @export
phr_validate_datetime <- function(x, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)

  # POSIXct or POSIXlt are datetime objects \u2014 accept directly
  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    return(invisible(TRUE))
  }

  # Check character strings that include a time component
  if (is.character(x)) {
    for (fmt in .phr_datetime_formats) {
      parsed <- suppressWarnings(as.POSIXct(x, format = fmt, tz = "UTC"))
      if (!any(is.na(parsed))) {
        return(invisible(TRUE))
      }
    }
  }

  # Nothing matched \u2014 raise structured error or warning
  msg <- "Expected a datetime object (POSIXct or POSIXlt) or a string with date and time components."
  hint_txt <- hint %||% "Ensure input is POSIXct, POSIXlt, or a datetime string like '2025-10-16 14:32:00'."
  if (soft) {
    phr_warning(message = msg, origin = origin, hint = hint_txt)
    return(invisible(FALSE))
  }
  phr_error(msg, origin = origin, hint = hint_txt)
}
