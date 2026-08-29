#' Convert a text string to a translation key format
#'
#' @param text The text string to convert
#' @return A lowercase key with underscores instead of spaces
#' @export
phr_text_to_key <- function(text) {
  if (is.null(text) || text == "") return("")

  # Convert to lowercase
  key <- tolower(text)
  # Replace special characters with spaces first (preserve word boundaries)
  key <- gsub("[^a-z0-9]", " ", key)
  # Collapse multiple spaces and replace with underscores
  key <- gsub("\\s+", "_", trimws(key))
  # Remove leading/trailing underscores
  key <- gsub("^_+|_+$", "", key)
  # Handle keys that start with numbers
  if (nchar(key) > 0 && grepl("^[0-9]", key)) {
    key <- paste0("txt_", key)
  }
  # Limit key length to 100 characters for longer descriptive text
  if (nchar(key) > 100) {
    # Use a simple hash based on string length and character sum for uniqueness
    char_sum <- sum(utf8ToInt(text))
    hash_suffix <- sprintf("%x", char_sum %% 65536)
    key <- paste0(substr(key, 1, 90), "_", hash_suffix)
  }

  return(key)
}
