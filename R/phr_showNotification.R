#' Display a notification, using Shiny if available
#'
#' `phr_showNotification()` is a lightweight wrapper around
#' `shiny::showNotification()`. It allows packages to trigger user-facing
#' notifications without requiring Shiny as a hard dependency. If Shiny is
#' installed, the function calls `shiny::showNotification()`. Otherwise, it
#' prints a fallback message to the console.
#'
#' @param message A character string containing the notification text.
#' @param type A character string indicating the notification type.
#'   Common values include `"default"`, `"message"`, `"warning"`, and `"error"`.
#'
#' @details
#' This function is designed for internal package use and for testing
#' environments where Shiny may not be available. The fallback console message
#' is prefixed with `"[PHR::Notify Fallback]"` to make it easy to detect during
#' automated tests.
#'
#' @return Invisibly returns `NULL`. The side effect is either a Shiny
#'   notification or a console message.
#'
#' @export
phr_showNotification <- function(message, type = "default") {
  if (requireNamespace("shiny", quietly = TRUE)) {
    fn <- get("showNotification", asNamespace("shiny"))
    fn(message, type = type)
  } else {
    message(sprintf("[PHR::Notify Fallback] %s (%s)", message, type))
  }
}
