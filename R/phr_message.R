#' @title Emit a Shiny-Aware IPHRA Message
#' @description
#' Displays an informational message in the console or via a Shiny notification
#' and records it in the session log.
#'
#' @param message Informational message text.
#' @param origin Optional name of the originating function.
#'
#' @return Invisibly returns `NULL`.
#' @export
phr_message <- function(message, origin = NULL) {
  full_msg <- paste0(
    "[PHR::Message] ",
    if (!is.null(origin)) paste0("In `", origin, "`: ") else "",
    message
  )
  # phr_log("message", full_msg, origin)
  phr_notify(full_msg, type = "message")
  invisible(NULL)
}
