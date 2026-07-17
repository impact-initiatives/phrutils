#' Check whether a vector can be safely coerced to a target type
#'
#' `is_safely_coercible()` returns a single logical value indicating whether
#' all elements of a vector can be safely coerced to a specified type. It is a
#' higher-level wrapper around `which_bad_coercible()` and is intended for
#' validating input before performing type conversion.
#'
#' @param x A vector of values to test.
#' @param to_type A character string specifying the target type. Supported
#'   values include `"numeric"`, `"character"`, `"logical"`, `"Date"`,
#'   `"datetime"`, `"POSIXct"`, `"POSIXlt"`, and `"factor"`.
#'
#' @details
#' The function checks coercibility using base R conversion functions and a
#' wide set of date/time formats. `NA` values are treated as safely coercible.
#' For logical coercion, only standard logical tokens (`TRUE`, `FALSE`, `T`,
#' `F`, `1`, `0`) are accepted. For datetime coercion, the function uses the
#' internal `.phr_datetime_formats` list of formats.
#'
#' @return A single logical value: `TRUE` if all elements of `x` can be safely
#'   coerced to the requested type, otherwise `FALSE`.
#'
#' @examples
#' is_safely_coercible(c("1", "2", "3"), "numeric")
#' is_safely_coercible(c("TRUE", "FALSE", "maybe"), "logical")
#' is_safely_coercible(c("2020-01-01", "not a date"), "Date")
#'
#' @export
is_safely_coercible <- function(x, to_type) {
  # returns TRUE if all elements of x can be safely coerced to the target type
  suppressWarnings({
    if (to_type == "numeric") {
      coerced <- suppressWarnings(as.numeric(x))
      return(!any(is.na(coerced) & !is.na(x)))
    } else if (to_type == "character") {
      coerced <- suppressWarnings(as.character(x))
      return(TRUE)  # as.character() never fails
    } else if (to_type == "logical") {
      valid_vals <- c("TRUE", "FALSE", "T", "F", "1", "0")
      return(all(toupper(as.character(x)) %in% valid_vals | is.na(x)))
    } else if (to_type == "Date" | to_type == "date") {
      formats <- c(
        "%Y-%m-%d", "%Y/%m/%d", "%d/%m/%Y", "%d-%m-%Y",
        "%m/%d/%Y", "%m-%d-%Y", "%Y%m%d", "%d%m%Y", "%m%d%Y",
        "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S",
        "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%OSZ", "%Y-%m-%dT%H:%M:%OS%z",
        "%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %I:%M:%S %p",
        "%m/%d/%Y %I:%M:%S %p", "%d/%m/%Y %H:%M:%S", "%d-%m-%Y %H:%M:%S",
        "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M", "%d/%m/%Y %H:%M",
        "%Y-%m", "%m/%Y"
      )

      # Check if all elements can be parsed by at least one format
      is_convertible <- vapply(x, function(val) {
        if (is.na(val)) {
          TRUE  # Treat NA as safely coercible
        } else {
          any(!is.na(sapply(formats, function(fmt) {
            suppressWarnings(as.POSIXct(val, format = fmt, tz = "UTC"))
          })))
        }
      }, logical(1))

      return(all(is_convertible))

    } else if (to_type == "factor") {
      return(is.character(x) || is.factor(x))
    } else if (to_type == "datetime" || to_type == "POSIXct" || to_type == "POSIXlt") {
      if (inherits(x, c("POSIXct", "POSIXlt"))) return(TRUE)
      is_convertible <- vapply(x, function(val) {
        if (is.na(val)) {
          TRUE
        } else {
          any(!is.na(sapply(.phr_datetime_formats, function(fmt) {
            suppressWarnings(as.POSIXct(as.character(val), format = fmt, tz = "UTC"))
          })))
        }
      }, logical(1))
      return(all(is_convertible))
    } else {
      return(FALSE)
    }
  })
}
