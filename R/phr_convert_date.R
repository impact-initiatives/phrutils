#' Convert character, numeric, or POSIX dates to standard Date (YYYY-MM-DD)
#'
#' @param x Character, Date, numeric, or POSIX vector.
#' @param origin Character string specifying the origin date for numeric input conversion.
#'   Use `"excel"` for Excel serial dates (treated as `"1899-12-30"`), or any date string
#'   in `"YYYY-MM-DD"` format (default: `"1970-01-01"` for Unix epoch).
#' @return A Date vector in YYYY-MM-DD format.
#' @export
phr_convert_date <- function(x, origin = "1970-01-01") {

  # Excel keyword
  if (identical(origin, "excel")) {
    origin <- "1899-12-30"
  }

  today <- Sys.Date()
  future_limit <- today + 365 * 5   # numeric\u2192date more than 5 years in the future is suspicious

  # Already Date
  if (inherits(x, "Date")) return(x)

  # POSIX \u2192 Date
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    return(as.Date(x))
  }

  # -------- NUMERIC INPUT --------
  if (is.numeric(x)) {

    # ORIGINAL LOGIC (minimal revision)
    origin_num <- if (all(x > 30000 & x < 60000, na.rm = TRUE)) {
      "1899-12-30"     # Excel
    } else {
      "1970-01-01"     # Unix
    }

    return(as.Date(x, origin = origin_num))
  }

  # -------- CHARACTER NUMERIC-LIKE --------
  if (is.character(x) && all(grepl("^[0-9]+$", x[!is.na(x)]))) {

    x_num <- as.numeric(x)

    # ORIGINAL LOGIC (minimal revision)
    origin_num <- if (all(x_num > 20000 & x_num < 60000, na.rm = TRUE)) {
      "1899-12-30"     # Excel
    } else {
      "1970-01-01"     # Unix
    }

    return(as.Date(x_num, origin = origin_num))
  }

  # -------- GENERAL CHARACTER DATES --------
  x_chr <- as.character(x)
  is_na <- is.na(x_chr)
  to_parse <- trimws(x_chr[!is_na])

  # Remove timezones
  to_parse <- sub("\\s*(UTC|GMT|CEST|CET|EST|PST|[+-]\\d{2}:\\d{2})$",
                  "", to_parse, ignore.case = TRUE)

  # Strip ISO timestamps
  to_parse <- sub("T.*$", "", to_parse)

  # Warn if time-of-day present
  if (any(grepl("\\d{2}:\\d{2}:\\d{2}", x_chr[!is_na]))) {
    warning("Time components detected and removed by phr_convert_date().")
  }

  # Try to parse human-readable dates
  parsed <- suppressWarnings(lubridate::parse_date_time(
    to_parse,
    orders = c("ymd", "dmy", "mdy", "Ymd HMS", "dmY HMS"),
    exact = FALSE
  ))

  parsed <- as.Date(parsed)

  # Fail on parsing errors
  if (any(is.na(parsed))) {
    invalid_vals <- unique(to_parse[is.na(parsed)])
    stop(
      "Could not convert values to Date: ",
      paste0("'", invalid_vals, "'", collapse = ", "),
      ". Expected formats: ymd, dmy, mdy."
    )
  }

  # Reinsert NA
  out <- rep(NA, length(x_chr))
  out[!is_na] <- parsed
  class(out) <- "Date"

  return(out)
}
