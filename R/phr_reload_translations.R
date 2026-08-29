#' Reload translations from JSON files
#'
#' Useful for development when translation files are updated.
#' @export
phr_reload_translations <- function() {
  phr_translations <<- phrutils::phr_load_translations()
  invisible(phr_translations)
}
