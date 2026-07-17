#' @title Internal Notification Dispatcher
#' @description
#' Handles displaying messages, warnings, and errors either via Shiny notifications
#' (if the app is running) or via console output (when outside Shiny).
#'
#' @param message The text to display.
#' @param type One of `"message"`, `"warning"`, or `"error"`.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @export
phr_notify <- function(message, type = c("message", "warning", "error")) {
  type <- match.arg(type)
  if (requireNamespace("shiny", quietly = TRUE) && shiny::isRunning()) {
    shiny::showNotification(message, type = type)
  } else {
    switch(
      type,
      message = base::message(message),
      warning = base::warning(message, call. = FALSE),
      error   = base::message(paste0("[PHR::Error] ", message))
    )
  }
}
