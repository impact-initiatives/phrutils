#' @title Issue a Shiny-Aware IPHRA Warning
#' @description
#' Displays a structured warning in either the console or the Shiny app,
#' and logs the message for the current session.
#'
#' @param message The warning text.
#' @param type Character string specifying the type (default = `"Warning"`).
#' @param origin Optional name of the originating function.
#' @param hint Optional string suggesting corrective action.
#'
#' @return Invisibly returns `NULL`.
#' @export
phr_warning <- function(message, type = "Warning", origin = NULL, hint = NULL) {
  full_msg <- paste0(
    "[PHR::", type, "] ",
    if (!is.null(origin)) paste0("In `", origin, "`: ") else "",
    message,
    if (!is.null(hint)) paste0("\n \u2022 Hint: ", hint) else ""
  )
  # phr_log("warning", full_msg, origin)
  phr_notify(full_msg, type = "warning")
  invisible(NULL)
}
