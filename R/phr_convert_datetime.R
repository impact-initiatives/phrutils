#' Convert character, numeric, Date, or POSIX values to POSIXct (datetime)
#'
#' Converts various input types to a `POSIXct` vector, preserving time
#' information. Unlike `phr_convert_date()`, this function does not strip the
#' time component.
#'
#' @param x Character, Date, numeric (Unix timestamp), or POSIX vector.
#' @param tz Time zone to use for the output POSIXct vector. Defaults to `"UTC"`.
#' @return A `POSIXct` vector.
#' @export
phr_convert_datetime <- function(x, tz = "UTC") {

  # Already POSIXct \u2014 return as-is (re-stamp tz to be safe)
  if (inherits(x, "POSIXct")) return(as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = tz))

  # POSIXlt \u2192 POSIXct
  if (inherits(x, "POSIXlt")) return(as.POSIXct(x, tz = tz))

  # Date \u2192 POSIXct (midnight)
  if (inherits(x, "Date")) return(as.POSIXct(as.character(x), format = "%Y-%m-%d", tz = tz))

  # Numeric \u2014 treat as Unix timestamp
  if (is.numeric(x)) return(as.POSIXct(x, origin = "1970-01-01", tz = tz))

  # Character \u2014 try known datetime formats
  x_chr <- as.character(x)
  is_na <- is.na(x_chr)
  to_parse <- trimws(x_chr[!is_na])

  parsed <- NULL
  for (fmt in .phr_datetime_formats) {
    converted <- suppressWarnings(as.POSIXct(to_parse, format = fmt, tz = tz))
    if (all(!is.na(converted))) {
      parsed <- converted
      break
    }
  }

  if (is.null(parsed) || any(is.na(parsed))) {
    invalid_vals <- if (is.null(parsed)) unique(to_parse) else unique(to_parse[is.na(parsed)])
    stop(
      "Could not convert values to datetime (POSIXct): ",
      paste0("'", invalid_vals, "'", collapse = ", "),
      ". Expected formats like '2025-10-16 14:32:00' or ISO 8601."
    )
  }

  out <- as.POSIXct(rep(NA_real_, length(x_chr)), origin = "1970-01-01", tz = tz)
  out[!is_na] <- parsed
  return(out)
}
