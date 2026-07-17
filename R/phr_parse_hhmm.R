#' Parse an \code{"HH:MM"} time-of-day string to minutes since midnight
#'
#' Converts a character string in 24-hour \code{"HH:MM"} format (e.g.\
#' \code{"08:30"}, \code{"18:00"}) to a single numeric value representing
#' the number of minutes elapsed since midnight.  Numeric inputs are returned
#' unchanged (they are assumed to already be minutes since midnight).
#'
#' @param x A character string in \code{"HH:MM"} format, or a numeric value
#'   (minutes since midnight).
#' @param origin Optional name of the calling function, used in error messages.
#' @return A single numeric value: minutes since midnight.
#' @export
phr_parse_hhmm <- function(x, origin = NULL) {

  if (is.numeric(x)) return(x)

  x_chr <- trimws(as.character(x))

  if (!grepl("^[0-9]{1,2}:[0-9]{2}$", x_chr)) {
    phr_error(
      message = glue::glue("Cannot parse time-of-day value: '{x_chr}'. Expected 'HH:MM' format (e.g. '08:30', '18:00')."),
      origin = origin %||% "phr_parse_hhmm",
      hint   = "Provide a 24-hour time string such as '08:00' or '18:30'."
    )
  }

  parts <- as.integer(strsplit(x_chr, ":", fixed = TRUE)[[1L]])
  h <- parts[1L]
  m <- parts[2L]

  if (h < 0L || h > 23L || m < 0L || m > 59L) {
    phr_error(
      message = glue::glue("Invalid time-of-day value: '{x_chr}'. Hours must be 0-23 and minutes 0-59."),
      origin = origin %||% "phr_parse_hhmm"
    )
  }

  h * 60L + m
}
