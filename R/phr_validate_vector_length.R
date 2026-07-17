#' @title Validate Vector Length
#' @description
#' Ensures a vector has a specified minimum or exact length.
#' @param x The vector to test.
#' @param min_length Minimum allowed length (default = 1).
#' @param exact_length Optional integer specifying exact expected length.
#' @param origin Optional name of the originating function.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_vector_length <- function(x, min_length = 1, exact_length = NULL, origin = NULL, soft) {
  phr_validate_not_null(x, origin, soft)
  len <- length(x)
  if (!is.null(exact_length) && len != exact_length) {
    msg <- paste0("Expected vector of length ", exact_length, ", got length ", len, ".")
    if (soft) {
      phr_warning(message = msg, origin = origin)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin)
  } else if (len < min_length) {
    msg <- paste0("Expected vector of length \u2265 ", min_length, ", got ", len, ".")
    if (soft) {
      phr_warning(message = msg, origin = origin)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin)
  }
  invisible(TRUE)
}
