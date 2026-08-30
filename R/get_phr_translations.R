#' Get current translation json loaded in the phr_environment
#'
#' @export
get_phr_translations <- function() {

  if (is.null(.phr_env$translations)) {
    .phr_env$translations <- phr_load_translations()
  }

  .phr_env$translations
}
