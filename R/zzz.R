.phr_env <- new.env(parent = emptyenv())
.phr_env$translations <- NULL
.phr_env$current_lang <- "en"

.onLoad <- function(libname, pkgname) {
  try(
    phr_init_translations(),
    silent = TRUE
  )
}
