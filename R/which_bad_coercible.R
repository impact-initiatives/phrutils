#' Identify elements that cannot be safely coerced to a target type
#'
#' `which_bad_coercible()` checks each element of a vector and returns a logical
#' vector indicating which elements would fail or produce invalid results when
#' coerced to a specified type. It is useful for validating user input before
#' performing type conversion.
#'
#' @param x A vector of values to test.
#' @param to_type A character string specifying the target type. Supported
#'   values include `"numeric"`, `"character"`, `"logical"`, `"Date"`, and
#'   `"factor"`.
#'
#' @details
#' The function attempts coercion using base R conversion functions and a
#' variety of date/time formats. For `"logical"`, only standard logical tokens
#' (`TRUE`, `FALSE`, `T`, `F`, `1`, `0`) are considered valid. For `"Date"`,
#' multiple common date and datetime formats are tested.
#'
#' @return A logical vector of the same length as `x`, where `TRUE` indicates
#'   that the element cannot be safely coerced to the requested type.
#'
#' @examples
#' which_bad_coercible(c("1", "2", "x"), "numeric")
#' which_bad_coercible(c("TRUE", "nope"), "logical")
#' which_bad_coercible(c("2020-01-01", "not a date"), "Date")
#'
#' @export
which_bad_coercible <- function(x, to_type) {
  suppressWarnings({
    n <- length(x)

    if (to_type == "numeric") {
      coerced <- suppressWarnings(as.numeric(x))
      return(is.na(coerced) & !is.na(x))
    }

    if (to_type == "character") {
      return(rep(FALSE, n))  # always safe
    }

    if (to_type == "logical") {
      valid_vals <- c("TRUE", "FALSE", "T", "F", "1", "0")
      return(!(toupper(as.character(x)) %in% valid_vals | is.na(x)))
    }

    if (to_type == "Date") {
      formats <- c(
        "%Y-%m-%d", "%Y/%m/%d", "%d/%m/%Y", "%d-%m-%Y",
        "%m/%d/%Y", "%m-%d-%Y", "%Y%m%d", "%d%m%Y", "%m%d%Y",
        "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S",
        "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%OSZ",
        "%Y-%m-%dT%H:%M:%OS%z", "%Y-%m-%d %H:%M:%OS",
        "%Y-%m-%d %I:%M:%S %p", "%m/%d/%Y %I:%M:%S %p",
        "%d/%m/%Y %H:%M:%S", "%d-%m-%Y %H:%M:%S",
        "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M",
        "%d/%m/%Y %H:%M", "%Y-%m", "%m/%Y"
      )

      ok_row <- vapply(x, function(val) {
        any(!is.na(sapply(formats, function(fmt) {
          suppressWarnings(as.POSIXct(val, format = fmt, tz = "UTC"))
        })))
      }, logical(1))

      return(!ok_row & !is.na(x))
    }

    if (to_type == "factor") {
      return(!(is.character(x) | is.factor(x)))
    }

    # fallback
    rep(TRUE, n)
  })
}
