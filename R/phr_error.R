#' @title Throw a Shiny-Aware Standardized IPHRA Error
#' @description
#' Displays or raises a structured error safely.
#' In Shiny, it shows a non-crashing notification and stops only
#' the local operation using `shiny::req(FALSE)`.
#' Outside Shiny, it aborts execution with a standardized error message.
#'
#' @param message The error text.
#' @param type Character string specifying the type (default = `"Error"`).
#' @param origin Optional name of the originating function or process.
#' @param hint Optional string providing a corrective suggestion for the user.
#'
#' @return No return value; execution stops locally.
#' @export
phr_error <- function(message, type = "Error", origin = NULL, hint = NULL) {
  full_msg <- paste0(
    "[PHR::", type, "] ",
    if (!is.null(origin)) paste0("In `", origin, "`: ") else "",
    message,
    if (!is.null(hint)) paste0("\n \u2022 Hint: ", hint) else ""
  )

  # phr_log("error", full_msg, origin)

  if (requireNamespace("shiny", quietly = TRUE) && shiny::isRunning()) {
    phr_showNotification(full_msg, type = "error")

    # \u2705 Skip shiny::req(FALSE) only during testing
    if (!isTRUE(getOption("IPHRA_TEST_MODE", FALSE))) {
      shiny::req(FALSE)
    } else {
      message("[IPHRA_TEST_MODE active] \u2014 skipping shiny::req(FALSE)")
    }
  } else {
    rlang::abort(message = full_msg, class = paste0("phr_", tolower(type)))
  }
}
