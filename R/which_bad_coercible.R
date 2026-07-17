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

# ---- All Numeric ----------------------------------------------------
