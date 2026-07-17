#' @title Validate Entire Vector is Character or Coercible to Character
#' @description
#' Checks that all values in a vector (or column) are character strings or can be
#' represented as character safely. Optionally enforces that all values belong to a
#' specified set of allowable options.
#'
#' @param x Vector or data frame column to test.
#' @param allowed_values Optional character vector of allowed values. If provided,
#'   validation will fail if any element of `x` is not found within this set.
#' @param origin Optional name of the originating function (for tracing errors).
#' @param hint Optional corrective hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#'
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_all_character <- function(x, allowed_values = NULL, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)

  # Type or coercion check
  if (!is.character(x) && !is_safely_coercible(x, "character")) {
    msg <- "Expected all values to be character or coercible to character."
    hint_txt <- hint %||% "Use as.character() or convert factors/integers before passing."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }

  # Allowed values check
  if (!is.null(allowed_values)) {
    # Coerce to character for comparison safety
    x_chr <- as.character(x)
    invalid <- setdiff(unique(x_chr), allowed_values)
    if (length(invalid) > 0) {
      msg <- paste0(
        "Invalid values detected: ", paste(invalid, collapse = ", "),
        ". Expected values: ", paste(allowed_values, collapse = ", ")
      )
      hint_txt <- hint %||% "Ensure all character entries match allowed options."
      if (soft) {
        phr_warning(message = msg, origin = origin, hint = hint_txt)
        return(invisible(FALSE))
      }
      phr_error(msg, origin = origin, hint = hint_txt)
    }
  }

  invisible(TRUE)
}


# ---- All Logical ----------------------------------------------------
