#' @title Check if an phr_try Result Indicates Failure
#' @description
#' Utility function to check if the result from `phr_try` or `phr_try_step`
#' indicates a failure. Returns TRUE if the result is a list with `success = FALSE`.
#'
#' @param result The result from an `phr_try` or `phr_try_step` call.
#'
#' @return TRUE if the result indicates failure, FALSE otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' result <- phr_try_step({ stop("error") }, step = "Test")
#' if (phr_failed(result)) {
#'   return(result)  # Bubble up to outer handler
#' }
#' }
phr_failed <- function(result) {
  is.list(result) && !is.null(result$success) && isTRUE(result$success == FALSE)
}
