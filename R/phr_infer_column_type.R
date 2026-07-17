#' Infer the likely data type of a column
#'
#' Inspects a vector and attempts to infer whether it represents
#' numeric, date, logical, or character data based on its content.
#' Non-destructive: does not coerce, only classifies.
#'
#' @param x A vector (typically one column from a data frame).
#' @param name Optional column name (for diagnostic messages).
#' @param return_details Logical; if TRUE, returns additional metadata
#'   (percentages of matching patterns per type).
#'
#' @return A character string ("numeric", "date", "logical", "character", or "unknown"),
#'   or a list with detailed info if `return_details = TRUE`.
#'
#' @export
#'
#' @examples
#' phr_infer_column_type(c("12", "5", "9"))
#' phr_infer_column_type(c("TRUE", "FALSE", "TRUE"))
#' phr_infer_column_type(c("2024-05-03", "2024-05-04"))
#' phr_infer_column_type(c("North", "South"))

phr_infer_column_type <- function(x, name = NULL, return_details = FALSE) {
  # --- Prepare input ---
  x_clean <- trimws(as.character(x))
  x_clean <- x_clean[!is.na(x_clean) & x_clean != ""]

  if (length(x_clean) == 0) {
    if (return_details) {
      return(list(type = "unknown", valid = TRUE, reason = "All values empty or NA"))
    } else {
      return("unknown")
    }
  }

  n <- length(x_clean)

  # --- Numeric detection ---
  numeric_pattern <- grepl("^[-+]?[0-9]*\\.?[0-9]+$", x_clean)
  pct_numeric <- mean(numeric_pattern)

  # --- Logical detection ---
  logical_pattern <- grepl("^(TRUE|FALSE|T|F|Yes|No|Y|N|1|0)$", x_clean, ignore.case = TRUE)
  pct_logical <- mean(logical_pattern)

  # --- Date detection ---
  date_formats <- c(
    "%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d", "%m-%d-%Y", "%d-%m-%Y",
    "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%Y-%m-%dT%H:%M:%OSZ"
  )
  pct_date <- 0
  for (fmt in date_formats) {
    suppressWarnings({
      converted <- as.POSIXct(x_clean, format = fmt, tz = "UTC")
    })
    if (all(!is.na(converted))) {
      pct_date <- 1
      break
    } else {
      pct_date <- max(pct_date, mean(!is.na(converted)))
    }
  }

  # --- Choose dominant type ---
  scores <- c(numeric = pct_numeric, date = pct_date, logical = pct_logical)
  best_type <- names(scores)[which.max(scores)]
  confidence <- max(scores)

  # --- Threshold logic ---
  inferred_type <- if (confidence >= 0.9) best_type else "character"

  # --- Construct result ---
  if (return_details) {
    return(list(
      type = inferred_type,
      confidence = confidence,
      pct_numeric = pct_numeric,
      pct_logical = pct_logical,
      pct_date = pct_date,
      n_values = n
    ))
  } else {
    return(inferred_type)
  }
}
