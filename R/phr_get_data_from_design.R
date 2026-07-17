#' Extract Data Frame from Survey Design Object
#'
#' Validates that the input is a srvyr or survey design object and extracts
#' the underlying data frame.
#'
#' @param survey_design A srvyr or survey design object (e.g., created with
#'   \code{srvyr::as_survey_design()}).
#' @param origin Optional character string indicating where this function was called from.
#'
#' @return The underlying data frame extracted from the survey design object.
#' @export
phr_get_data_from_design <- function(survey_design, origin = NULL) {
  valid_classes <- c("tbl_svy", "survey.design", "survey.design2", "svyrep.design")

  if (!inherits(survey_design, valid_classes)) {
    phr_error(
      message = "survey_design must be a survey design object (e.g. created with srvyr::as_survey_design()).",
      origin = origin,
      hint = "Use srvyr::as_survey_design() to create a survey design from your data frame."
    )
  }

  df <- survey_design$variables

  if (!is.data.frame(df)) {
    phr_error(
      message = "Could not extract a valid data frame from the survey design object.",
      origin = origin
    )
  }

  df
}
