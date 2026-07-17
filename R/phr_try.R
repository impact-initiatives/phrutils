#' @title Safe IPHRA Try Wrapper (Enhanced with Nested Context Support)
#' @description
#' Evaluates an expression safely and reports structured feedback
#' to the console or Shiny UI without crashing the app.
#' Preserves and passes along origin and hint metadata for better context.
#'
#' Supports nested error handling via the `step` parameter, which allows
#' inner try blocks to add sub-context to error messages. When `step` is
#' provided alongside `origin`, messages are formatted as "origin -> step".
#'
#' @param expr Expression to evaluate.
#' @param on_error One of "warn", "return", or "abort".
#' @param origin Optional string identifying where the try block is called from.
#' @param hint Optional corrective hint to show in case of error.
#' @param step Optional string identifying the step within a larger operation.
#'   When provided, adds sub-context to error messages for nested try blocks.
#'   If both `origin` and `step` are provided, the full origin becomes "origin -> step".
#'
#' @return If `on_error = "return"`, returns list(success = FALSE, error = message, origin = ..., step = ..., hint = ...).
#'   On success, returns the result of `expr` (or NULL if no explicit return).
#' @export
#'
#' @examples
#' \dontrun{
#' # Outer catch-all with nested step tracking
#' observe({
#'   phr_try({
#'     # Validation step
#'     result <- phr_try({
#'       if (is.null(input$value)) stop("Value required")
#'     }, on_error = "return", step = "Validation")
#'     if (isFALSE(result$success)) return(result)
#'
#'     # Core logic step
#'     result <- phr_try({
#'       process_data(input$value)
#'     }, on_error = "return", step = "Core Logic")
#'     if (isFALSE(result$success)) return(result)
#'
#'   },
#'   on_error = "warn",
#'   origin = "Module: Operation",
#'   hint = "Check input values"
#'   )
#' })
#' }
phr_try <- function(expr,
                      on_error = c("warn", "return", "abort"),
                      origin = NULL,
                      hint = NULL,
                      step = NULL) {
  on_error <- match.arg(on_error)

  # Build full origin with step context if provided
  full_origin <- if (!is.null(step) && !is.null(origin)) {
    paste0(origin, " \u2192 ", step)  # Unicode arrow \u2192
  } else if (!is.null(step)) {
    step
  } else {
    origin
  }

  tryCatch(
    expr,
    error = function(e) {
      base_msg <- conditionMessage(e)

      # Check if error came from a nested phr_try with step info
      # If so, preserve the nested context chain
      if (grepl("\\[PHR::", base_msg)) {
        # Already has IPHRA structure - check if we need to add outer context
        if (!is.null(origin) && !grepl(origin, base_msg, fixed = TRUE)) {
          # Add outer origin context to nested error
          msg <- sub("\\[PHR::TryError\\] ",
                     paste0("[PHR::TryError] ", origin, " \u2192 "),
                     base_msg)
        } else {
          msg <- base_msg
        }
      } else {
        # New error - format with full origin (including step if present)
        if (!is.null(full_origin)) {
          msg <- paste0("[PHR::TryError] ", full_origin, ": ", base_msg)
        } else {
          msg <- paste0("[PHR::TryError] ", base_msg)
        }
      }

      # Log the failure
      # phr_log("error", msg, full_origin)

      # Act according to user preference
      switch(
        on_error,
        warn = phr_warning(msg, origin = full_origin %||% "phr_try", hint = hint),
        abort = phr_error(msg, origin = full_origin %||% "phr_try", hint = hint),
        return = list(success = FALSE, error = msg, origin = origin, step = step, hint = hint)
      )
    }
  )
}
