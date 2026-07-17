#' Traffic Light Color Palette
#'
#' Provides a traffic light color palette with red, orange/yellow, green, and light grey
#' in that order (worst to best). Useful for quality indicators, status displays, and
#' categorical data where the first level is the worst outcome.
#'
#' @param n Number of colors to return. If NULL, returns all 4 colors (red, orange, green, grey).
#'   If n is less than 4, returns the first n colors in order.
#'   If n is greater than 4, cycles through the colors or interpolates.
#' @param include_grey Logical indicating whether to include the light grey color. Default is TRUE.
#' @param reverse Logical indicating whether to reverse the color order. Default is FALSE.
#'
#' @return A character vector of hex color codes.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get all traffic light colors
#'   traffic_light_colors()
#'
#'   # Get only red, orange, green (no grey)
#'   traffic_light_colors(include_grey = FALSE)
#'
#'   # Get 6 colors by cycling
#'   traffic_light_colors(n = 6)
#' }
traffic_light_colors <- function(n = NULL, include_grey = TRUE, reverse = FALSE) {
  origin <- "traffic_light_colors"

  # Define traffic light palette (worst to best: red, orange/yellow, green, grey for NA)
  colors <- c(
    "#E74C3C",  # Red (critical/fail - worst)
    "#F39C12",  # Yellow/Orange (warning)
    "#2ECC71",  # Green (good/pass - best)
    "#BDC3C7"   # Light grey (neutral/NA)
  )

  # Remove grey if not included
  if (!include_grey) {
    colors <- colors[1:3]
  }

  # Reverse if requested
  if (reverse) {
    colors <- rev(colors)
  }

  # If n is specified, adjust the color vector
  if (!is.null(n)) {
    phr_assert(is.numeric(n) && n > 0,
                 "n must be a positive integer",
                 origin = origin)

    if (n <= length(colors)) {
      # Return first n colors
      colors <- colors[1:n]
    } else {
      # Cycle through colors to reach n
      colors <- rep(colors, length.out = n)
    }
  }

  return(colors)
}
