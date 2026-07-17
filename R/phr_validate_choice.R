#' @title Validate Choice Membership
#' @description
#' Ensures that the input value(s) belong to a defined set of allowed options.
#' @param x Value or vector to test.
#' @param choices Character vector of allowed values.
#' @param origin Optional name of the originating function.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_choice <- function(x, choices, origin = NULL, soft) {
  phr_validate_not_null(x, origin, soft)
  phr_validate_not_null(choices, origin, soft)
  bad <- setdiff(x, choices)
  if (length(bad) > 0) {
    msg <- paste0("Invalid choice(s): ", paste(bad, collapse = ", "))
    hint_txt <- paste0("Allowed values are: ", paste(choices, collapse = ", "))
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_warning(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}
