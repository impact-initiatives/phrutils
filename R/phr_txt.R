# ---- Safe Translation Lookup ----
#' Get translated text for a given key
#'
#' @param key The translation key (string). The key can be a full text string
#'   which will be converted to a key format, or a pre-defined key from the
#'   translation files.
#' @param lang Optional language code (e.g., "en", "fr"). If NULL, attempts to
#'   get from session or falls back to phr_current_lang.
#' @param default Optional default value if key is not found.
#' @param session Shiny session object for reactive language selection.
#' @return The translated text string.
#' @export
phr_txt <- function(
    key,
    lang = NULL,
    default = NULL,
    session = shiny::getDefaultReactiveDomain()
) {

  translations <- get_phr_translations()

  if (is.null(lang)) {
    lang <- get_phr_language(session)
  }

  if (is.null(lang) || !lang %in% names(translations)) {
    lang <- .phr_env$current_lang
  }

  lookup_key <- phr_text_to_key(key)

  value <- translations[[lang]][[lookup_key]]

  if (is.null(value) && lang != "en") {
    value <- translations[["en"]][[lookup_key]]
  }

  if (is.null(value) || value == "") {
    return(default %||% key)
  }

  value
}
