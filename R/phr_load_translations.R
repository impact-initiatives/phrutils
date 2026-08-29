#' Load translation dictionaries from JSON files in inst/app/www/i18n/
#' @export
phr_load_translations <- function() {

  translations <- list()

  i18n_paths <- c(
    file.path(getwd(), "inst", "app", "www", "i18n"),
    file.path(getwd(), "app", "www", "i18n")
  )

  i18n_dir <- NULL

  for (path in i18n_paths) {
    if (dir.exists(path)) {
      i18n_dir <- path
      break
    }
  }

  if (is.null(i18n_dir)) {
    warning("i18n directory not found. Using fallback translations.")
    return(list(
      en = list(
        export_tor = "Export ToR",
        validation_passed = "Validation checks passed (dummy mode).",
        tor_export_success = "ToR export simulated successfully"
      ),
      fr = list(
        export_tor = "Exporter les TdR",
        validation_passed = "Verifications terminees avec succes (mode fictif).",
        tor_export_success = "Exportation simulee des TdR reussie"
      )
    ))
  }

  json_files <- list.files(
    i18n_dir,
    pattern = "\\.json$",
    full.names = TRUE
  )

  for (json_file in json_files) {
    lang_code <- tools::file_path_sans_ext(basename(json_file))

    tryCatch({
      translations[[lang_code]] <-
        jsonlite::fromJSON(json_file, simplifyVector = FALSE)
    }, error = function(e) {
      warning(
        paste(
          "Failed to load translation file:",
          json_file,
          "-",
          e$message
        )
      )
    })
  }

  translations
}
