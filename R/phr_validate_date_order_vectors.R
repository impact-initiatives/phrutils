#' Validate chronological order of date vectors
#'
#' Ensures that for each record, the start date is before or equal to the end date.
#'
#' @param start A vector of start dates (character, Date, or POSIXct).
#' @param end A vector of end dates (character, Date, or POSIXct).
#' @param origin Optional string indicating the calling module or object.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#'
#' @return TRUE (invisibly) if validation passes; otherwise issues an IPHRA warning or error.
#'
#' @export
#'
#' @examples
#' phr_validate_date_order_vectors(
#'   as.Date(c("2024-01-01", "2024-02-01")),
#'   as.Date(c("2024-03-01", "2024-01-15")),
#'   origin = "MortalityHouseholdData",
#'   soft = TRUE
#' )
#' # Issues a warning for the second pair.

phr_validate_date_order_vectors <- function(start, end, origin = NULL, soft) {
  # --- Defensive checks ---
  if (length(start) != length(end)) {
    msg <- glue::glue("Start and end date vectors differ in length. Comparing first {min(length(start), length(end))} pairs.")
    if (soft) {
      phr_warning(message = msg, origin = origin)
    } else {
      phr_error(msg, origin = origin)
    }
    n <- min(length(start), length(end))
    start <- start[seq_len(n)]
    end <- end[seq_len(n)]
  }

  # Coerce to Date if necessary
  suppressWarnings({
    start_date <- as.Date(start)
    end_date <- as.Date(end)
  })

  # Identify invalid coercions
  invalid_pairs <- which(is.na(start_date) | is.na(end_date))
  if (length(invalid_pairs) > 0) {
    msg <- glue::glue("Found {length(invalid_pairs)} records with invalid or missing start/end dates.")
    if (soft) {
      phr_warning(message = msg, origin = origin)
    } else {
      phr_error(msg, origin = origin)
    }
  }

  # Identify reversed (chronologically invalid) pairs
  reversed <- which(start_date > end_date)
  if (length(reversed) > 0) {
    msg <- glue::glue("Found {length(reversed)} records where recall_start occurs after recall_end.")
    if (soft) {
      phr_warning(message = msg, origin = origin)
      return(invisible(FALSE))
    } else {
      phr_error(msg, origin = origin)
    }
  }

  phr_message(glue::glue("Date order validation passed for {length(start_date)} records."))
  return(invisible(TRUE))
}
