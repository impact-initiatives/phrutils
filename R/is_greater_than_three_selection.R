#' @title Check if a Select Multiple Input has More Than Three Selections
#'
#' @description This helper function checks if rows in a `select_multiple` column or a vector have more than three selections (responses) by splitting responses based on whitespace delimiters.
#'
#' @param select_multiple_input A data frame, tibble, or a character vector containing the relevant column or values.
#' @param select_multiple_col A character string specifying the name of the `select_multiple` column to be checked (if a data frame is provided).
#'
#' @return A logical vector where `TRUE` indicates that a row or vector element has more than three selections and `FALSE` otherwise.
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   select_multiple_column = c(
#'     "option_a option_b",
#'     "option_c",
#'     "option_d option_e option_f option_g",
#'     ""
#'   )
#' )
#'
#' # Check with a data frame
#' is_greater_than_three_selection(df, "select_multiple_column")
#' # Output: FALSE, FALSE, TRUE, FALSE
#'
#' # Check with a vector
#' vec <- c("option_a option_b", "option_c", "option_d option_e option_f option_g", "")
#' is_greater_than_three_selection(vec)
#' # Output: FALSE, FALSE, TRUE, FALSE
#'
#' @export
is_greater_than_three_selection <- function(select_multiple_input, select_multiple_col = NULL) {
  origin <- "is_greater_than_three_selection"

  phr_try({
    # Handle input validation
    if (is.data.frame(select_multiple_input)) {
      # Validate column
      phr_validate_columns(
        select_multiple_input,
        select_multiple_col,
        origin = origin,
        hint = "Ensure the column for testing multiple selections exists in the dataset.",
        soft = FALSE
      )

      select_multiple_input <- select_multiple_input[[select_multiple_col]]
    }

    # Ensure the input is a character or factor vector
    phr_assert(
      is.character(select_multiple_input) || is.factor(select_multiple_input),
      origin = origin,
      "The input must be a character or factor vector."
    )

    # Check condition: more than three selections
    result <- sapply(
      as.character(select_multiple_input),
      function(row) {
        length(strsplit(row, split = "\\s+")[[1]]) > 3
      }
    )

    return(result)
  }, on_error = "abort", origin = origin, hint = "Ensure the input contains valid select multiple responses delimited by spaces.")
}
