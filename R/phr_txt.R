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
#' @noRd
phr_txt <- function(key, lang = NULL, default = NULL, session = shiny::getDefaultReactiveDomain()) {
  # [FUTURE] When language reactivity is connected, use session$userData$lang()
  # If no reactive session available, fallback to phr_current_lang or "en"

  if (is.null(lang)) {
    if (!is.null(session) && !is.null(session$userData$lang)) {
      # Safe reactive access - only works once session$userData$lang is defined
      lang <- tryCatch(session$userData$lang(), error = function(e) NULL)
    }
  }

  # Fallback chain
  if (is.null(lang) || !lang %in% names(phr_translations)) {
    if (exists("phr_current_lang", envir = .GlobalEnv)) {
      lang <- get("phr_current_lang", envir = .GlobalEnv)
    } else {
      lang <- "en"
    }
  }

  # Convert text to key format if it's a full text string
  lookup_key <- phr_text_to_key(key)

  # ---- Lookup ----
  value <- phr_translations[[lang]][[lookup_key]]

  # If not found in target language and not English, try English as fallback
  if (is.null(value) && lang != "en") {
    value <- phr_translations[["en"]][[lookup_key]]
  }

  # ---- Fallback logic ----

  if (is.null(value) || value == "") {
    if (!is.null(default)) return(default)
    # Return the original key as-is (useful during development)
    return(key)
  }

  return(value)
}
