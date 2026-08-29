get_phr_translations <- function() {
  if (is.null(.phr_env$translations)) {
    phr_init_translations()
  }

  .phr_env$translations
}
