#' @title Validate Entire Vector is Numeric or Coercible to Numeric
#' @description
#' Checks that all values in a vector (or column) are numeric or can be safely
#' converted to numeric without introducing NAs.
#' @param x Vector or data frame column to test.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()`.
#' @export
phr_validate_all_numeric <- function(x,
                                       origin = NULL,
                                       hint = NULL,
                                       soft) {

  phr_validate_not_null(x, origin, soft)

  # Already numeric \u2192 valid
  if (is.numeric(x)) return(invisible(TRUE))

  # Safely coercible \u2192 valid
  if (is_safely_coercible(x, "numeric")) return(invisible(TRUE))

  # ---- Build translated message + hint ----
  msg_txt  <- "Expected all values to be numeric or safely coercible to numeric."
  hint_txt <- hint %||% "Ensure values contain only digits and valid numeric strings."

  # ---- SOFT MODE: warning only ----
  if (soft) {
    phr_warning(message = msg_txt, origin = origin, hint = hint_txt)
    return(invisible(FALSE))
  }

  # ---- HARD MODE: throw error ----
  phr_error(
    msg_txt,
    origin = origin,
    hint = hint_txt
  )
}
