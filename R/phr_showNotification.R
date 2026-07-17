# Internal notification wrapper for testing and Shiny-safe mocking
#' @export
phr_showNotification <- function(message, type = "default") {
  if (requireNamespace("shiny", quietly = TRUE)) {
    fn <- get("showNotification", asNamespace("shiny"))
    fn(message, type = type)
  } else {
    message(sprintf("[PHR::Notify Fallback] %s (%s)", message, type))
  }
}
