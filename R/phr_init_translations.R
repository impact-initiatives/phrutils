phr_init_translations <- function() {
  .phr_env$translations <- phr_load_translations()
  invisible(.phr_env$translations)
}
