#' @title Lightweight Step-Level Try Wrapper for Nested Error Handling
#' @description
#' A convenience wrapper around `phr_try` designed for inner try blocks
#' within a nested error handling pattern. Always uses `on_error = "return"`
#' so that errors bubble up to the outer handler.
#'
#' This function is intended for use inside an outer `phr_try` block to
#' provide granular step-level error context (e.g., "Validation", "Core Logic",
#' "Result Handling").
#'
#' @param expr Expression to evaluate.
#' @param step String identifying the step within the larger operation.
#' @param hint Optional corrective hint to show in case of error.
#'
#' @return A list with `success = FALSE` and error details if an error occurs,
#'   otherwise returns the result of `expr`.
#' @export
#'
#' @examples
#' \dontrun{
#' observe({
#'   phr_try({
#'     # Use phr_try_step for each logical step
#'     result <- phr_try_step({
#'       validate_input(input$data)
#'     }, step = "Validation")
#'     if (isFALSE(result$success)) return(result)
#'
#'     result <- phr_try_step({
#'       process_data(input$data)
#'     }, step = "Core Logic")
#'     if (isFALSE(result$success)) return(result)
#'
#'   },
#'   on_error = "warn",
#'   origin = "Module: Operation"
#'   )
#' })
#' }
phr_try_step <- function(expr, step, hint = NULL) {
  phr_try(
    expr,
    on_error = "return",
    origin = NULL,  # Let outer handler provide origin
    step = step,
    hint = hint
  )
}
