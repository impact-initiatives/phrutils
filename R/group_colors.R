#' Group Comparison Color Palette
#'
#' Provides a set of highly distinguishable colors for comparing multiple groups
#' (e.g., administrative regions, strata, program areas). Colors are selected to maximize
#' visual contrast and avoid light or similar tones that make distinctions difficult.
#'
#' @param n Number of colors to return. If NULL, returns all 10 colors from the palette.
#'   If n is less than or equal to 10, returns the first n colors.
#'   If n is greater than 10, interpolates to generate exactly n colors.
#' @param reverse Logical indicating whether to reverse the color order. Default is FALSE.
#'
#' @return A character vector of hex color codes.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get all group comparison colors
#'   group_colors()
#'
#'   # Get 5 colors for 5 groups
#'   group_colors(n = 5)
#'
#'   # Get colors in reverse order
#'   group_colors(reverse = TRUE)
#' }
group_colors <- function(n = NULL, reverse = FALSE) {
  origin <- "group_colors"

  # Define group comparison palette with highly distinguishable colors
  # Selected to maximize contrast and avoid light/similar tones
  colors <- c(
    "#E63946",  # Red - Strong, warm
    "#1D3557",  # Dark blue - Cool, deep
    "#F77F00",  # Orange - Bright, energetic
    "#06A77D",  # Teal/green - Fresh, distinct
    "#9B59B6",  # Purple - Rich, unique
    "#D4A373",  # Tan/brown - Earthy, warm
    "#2E86AB",  # Medium blue - Professional
    "#D62828",  # Crimson - Bold, striking
    "#588157",  # Olive green - Natural
    "#BC4749"   # Burgundy - Deep, sophisticated
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

    if (n == 1) {
      # Return first color for single group
      colors <- colors[1]
    } else if (n <= length(colors)) {
      # Return first n colors
      colors <- colors[1:n]
    } else {
      # Use colorRampPalette to interpolate for more colors
      colors <- grDevices::colorRampPalette(colors)(n)
    }
  }

  return(colors)
}
