#' Sanitation Theme Color Palette
#'
#' Provides a color palette based on the Sanitation theme color (#532F87), using tints
#' blended with white at 80\%, 55\%, and 30\%. Can be scaled to any number of colors
#' by interpolating the full gradient from the primary color to the lightest tint.
#'
#' @param n Number of colors to return. If NULL, returns the 3 standard tints (80\%, 55\%, 30\%).
#'   If n is less than or equal to 3, returns the first n standard tints.
#'   If n is greater than 3, interpolates across the full gradient to return exactly n colors.
#' @param reverse Logical indicating whether to reverse the color order. Default is FALSE.
#'   When FALSE, returns colors from darkest (80\%) to lightest (30\%).
#'
#' @return A named character vector of hex color codes (unnamed when interpolated).
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get the 3 standard Sanitation tints
#'   sanitation_colors()
#'
#'   # Get 6 interpolated colors across the Sanitation gradient
#'   sanitation_colors(n = 6)
#'
#'   # Get colors in reverse order (lightest to darkest)
#'   sanitation_colors(reverse = TRUE)
#' }
sanitation_colors <- function(n = NULL, reverse = FALSE) {
  origin <- "sanitation_colors"

  # Standard tints: Sanitation primary (#532F87, RGB 83/47/135) blended with white
  colors <- c(
    "#75599F",  # RGB(117, 89, 159) — 80% primary + 20% white
    "#A08DBD",  # RGB(160, 141, 189) — 55% primary + 45% white
    "#CBC1DB"   # RGB(203, 193, 219) — 30% primary + 70% white
  )

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
      colors <- colors[1:n]
    } else {
      # Include primary color for a richer gradient range
      gradient <- c("#532F87", "#75599F", "#A08DBD", "#CBC1DB")
      if (reverse) gradient <- rev(gradient)
      colors <- grDevices::colorRampPalette(gradient)(n)
    }
  }

  return(colors)
}
