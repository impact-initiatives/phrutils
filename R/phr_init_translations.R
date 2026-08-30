#' Initialize saved json translation files
#'
#' Useful for development when translation files are updated.
#' @export
phr_init_translations <- function() {
  .phr_env$translations <- phr_load_translations()
  invisible(.phr_env$translations)
}
