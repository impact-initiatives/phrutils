#' Get current language
#' @export
get_phr_language <- function(
    session = shiny::getDefaultReactiveDomain()
) {

  if (!is.null(session) &&
      !is.null(session$userData$lang)) {

    return(session$userData$lang())
  }

  "en"
}
