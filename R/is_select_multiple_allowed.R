#' @title Check if a Select Multiple Input Contains Only Allowable Values
#'
#' @description This function checks if a `select_multiple` input (character or factor vector) contains only values included in a set of allowable values. If all values are valid, it returns `TRUE`; otherwise, it returns `FALSE`.
#'
#' @param variable A character or factor vector (representing the `select_multiple` input) to be checked.
#' @param allowable_values A character vector specifying the allowable values.
#'
#' @return A logical value: `TRUE` if all values are valid, `FALSE` if there are any invalid values.
#'
#' @details The function splits elements in the `variable` vector by whitespace and checks if each value belongs to the `allowable_values`. `NA` is considered valid and excluded from checks.
#'
#' @examples
#' variable <- c("option_a option_b", "option_c", "option_d option_e", NA)
#' allowable_values <- c("option_a", "option_b", "option_c")
#' is_select_multiple_allowed(variable, allowable_values)
#' # Output: FALSE (because "option_d" and "option_e" are invalid)
#'
#' @export
is_select_multiple_allowed <- function(variable, allowable_values) {
  origin <- "is_select_multiple_allowed"

  phr_try({
    # Validate `variable`
    phr_assert(
      is.vector(variable) && (is.character(variable) || is.factor(variable)),
      origin = origin,
      hint = glue::glue("`variable` must be a character or factor vector.")
    )

    # Validate `allowable_values`
    phr_assert(
      is.vector(allowable_values) && is.character(allowable_values),
      origin = origin,
      hint = glue::glue("`allowable_values` must be a character vector.")
    )

    # Convert `variable` to character if it's a factor
    variable <- as.character(variable)

    # Remove NA values from `allowable_values` for safe comparison
    allowable_values <- allowable_values[!is.na(allowable_values)]

    # Validate that `allowable_values` is not empty after removing NAs
    phr_assert(
      length(allowable_values) > 0,
      origin = origin,
      hint = glue::glue("`allowable_values` must contain at least one valid value.")
    )

    # Helper function to check if all values in a cell are valid
    check_element <- function(x) {
      if (is.na(x)) return(TRUE)  # NA is valid
      values <- unlist(strsplit(x, "\\s+"))  # Split by spaces
      all(values %in% allowable_values)  # Check if all values are valid
    }

    # Apply the helper function to the `variable`
    result <- all(sapply(variable, check_element))

    return(result)

  }, on_error = "abort", origin = origin, hint = glue::glue("Ensure `variable` contains valid select multiple responses, and `allowable_values` defines the allowed options."))
}
