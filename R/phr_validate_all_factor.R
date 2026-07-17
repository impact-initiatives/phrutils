#' @title Validate Entire Vector is Factor or Coercible to Factor
#' @description
#' Checks that all values are factors or coercible (e.g., from character strings).
#' Optionally enforces that all factor levels belong to a specified set of
#' allowed levels, and optionally verifies that the factor order matches an
#' expected ordering.
#'
#' @param x Vector or data frame column to test.
#' @param allowed_levels Optional character vector specifying the allowed factor levels.
#'   Validation fails if any observed level is not in this set.
#' @param expected_order Optional character vector specifying the correct ordering
#'   of factor levels. If provided, validation fails if the factor's level order
#'   does not exactly match.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint.
#'
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()`.
#' @export
#' @title Validate That All Values Are Factors
#' @description
#' Validates that all values in a vector are factor type.
#' Optionally checks for allowed levels and correct ordering.
#' Strict: will not coerce from character or numeric values.
#'
#' @param x Vector to validate.
#' @param allowed_levels Optional character vector of valid factor levels.
#' @param expected_order Optional character vector specifying expected order of levels.
#' @param origin Optional name of the function or module calling this validator.
#' @param hint Optional suggestion or guidance for fixing detected issues.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#'
#' @return Invisibly returns TRUE if valid; otherwise throws an [phr_error()] or [phr_warning()].
#' @export
phr_validate_all_factor <- function(
    x,
    allowed_levels = NULL,
    expected_order = NULL,
    origin = NULL,
    hint = NULL,
    soft
) {
  phr_validate_not_null(x, origin, soft)

  # ---- Type check ----
  if (!is.factor(x)) {
    msg <- "Expected a factor vector, but received a non-factor type."
    hint_txt <- hint %||% "Convert variable to factor before validation."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }

  # ---- Check allowed levels ----
  if (!is.null(allowed_levels)) {
    levels_present <- levels(x)
    invalid <- setdiff(levels_present, allowed_levels)
    if (length(invalid) > 0) {
      msg <- paste0(
        "Invalid factor levels detected: ", paste(invalid, collapse = ", "),
        ". Allowed levels are: ", paste(allowed_levels, collapse = ", ")
      )
      hint_txt <- hint %||% "Check factor level definitions or category spelling."
      if (soft) {
        phr_warning(message = msg, origin = origin, hint = hint_txt)
        return(invisible(FALSE))
      }
      phr_error(msg, origin = origin, hint = hint_txt)
    }
  }

  # ---- Check expected order ----
  if (!is.null(expected_order)) {
    actual_order <- levels(x)
    if (!identical(actual_order, expected_order)) {
      msg <- paste0(
        "Incorrect factor level order. Actual: ",
        paste(actual_order, collapse = " > "),
        ". Expected: ",
        paste(expected_order, collapse = " > ")
      )
      hint_txt <- hint %||% "Ensure factor levels are defined in the correct order."
      if (soft) {
        phr_warning(message = msg, origin = origin, hint = hint_txt)
        return(invisible(FALSE))
      }
      phr_error(msg, origin = origin, hint = hint_txt)
    }
  }

  invisible(TRUE)
}
