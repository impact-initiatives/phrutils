#' Hygiene Theme Color Palette
#'
#' Provides a color palette based on the Hygiene theme color (#008D48), using tints
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
#'   # Get the 3 standard Hygiene tints
#'   hygiene_colors()
#'
#'   # Get 6 interpolated colors across the Hygiene gradient
#'   hygiene_colors(n = 6)
#'
#'   # Get colors in reverse order (lightest to darkest)
#'   hygiene_colors(reverse = TRUE)
#' }
hygiene_colors <- function(n = NULL, reverse = FALSE) {
  origin <- "hygiene_colors"

  # Standard tints: Hygiene primary (#008D48, RGB 0/141/72) blended with white
  colors <- c(
    "#33A46D",  # RGB(51, 164, 109)  — 80% primary + 20% white
    "#73C09A",  # RGB(115, 192, 154) — 55% primary + 45% white
    "#B2DDC8"   # RGB(178, 221, 200) — 30% primary + 70% white
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
      gradient <- c("#008D48", "#33A46D", "#73C09A", "#B2DDC8")
      if (reverse) gradient <- rev(gradient)
      colors <- grDevices::colorRampPalette(gradient)(n)
    }
  }

  return(colors)
}
