#' Set current language
#' @export
set_phr_language <- function(
    lang,
    session = shiny::getDefaultReactiveDomain()
) {

  if (is.null(session)) {
    stop("No active shiny session.")
  }

  if (is.null(session$userData$lang)) {
    session$userData$lang <- shiny::reactiveVal("en")
  }

  session$userData$lang(lang)

  invisible(lang)
}
