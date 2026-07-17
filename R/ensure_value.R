#' Ensure a Value with a Default Fallback
#'
#' The `ensure_value` function ensures that a variable has a valid value.
#' If the input variable is `NULL` or an empty vector (e.g., `character(0)`),
#' it returns the specified default value. Otherwise, it returns the input value unchanged.
#'
#' @param value Any variable or expression to be checked.
#' @param default A default value to return if `value` is `NULL` or empty (`length(value) == 0`).
#'
#' @return Either the original `value` (if it's not `NULL` and not empty)
#' or the `default` value if `value` is `NULL` or empty.
#'
#' @examples
#' # Default value is returned when input is NULL
#' ensure_value(NULL, "default_value")  # Returns: "default_value"
#'
#' # Default value is returned when input is an empty vector
#' ensure_value(character(0), "default_value")  # Returns: "default_value"
#'
#' # Input value is returned when it is valid
#' ensure_value("valid_input", "default_value")  # Returns: "valid_input"
#'
#' # Usage inside a pipeline or similar defensive checks
#' some_var <- NULL
#' some_var <- ensure_value(some_var, "fallback_value")
#'
#' @export
ensure_value <- function(value, default) {
  if (is.null(value) || length(value) == 0) {
    default
  } else {
    value
  }
}
