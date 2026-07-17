#' Water Theme Color Palette
#'
#' Provides a color palette based on the Water theme color (#28A1D2), using tints
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
#'   # Get the 3 standard Water tints
#'   water_colors()
#'
#'   # Get 6 interpolated colors across the Water gradient
#'   water_colors(n = 6)
#'
#'   # Get colors in reverse order (lightest to darkest)
#'   water_colors(reverse = TRUE)
#' }
water_colors <- function(n = NULL, reverse = FALSE) {
  origin <- "water_colors"

  # Standard tints: Water primary (#28A1D2, RGB 40/161/210) blended with white
  colors <- c(
    "#53B4DB",  # RGB(83, 180, 219)  — 80% primary + 20% white
    "#89CBE6",  # RGB(137, 203, 230) — 55% primary + 45% white
    "#BEE3F2"   # RGB(190, 227, 242) — 30% primary + 70% white
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
      gradient <- c("#28A1D2", "#53B4DB", "#89CBE6", "#BEE3F2")
      if (reverse) gradient <- rev(gradient)
      colors <- grDevices::colorRampPalette(gradient)(n)
    }
  }

  return(colors)
}
