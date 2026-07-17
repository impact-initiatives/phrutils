#' Sex Comparison Color Palette
#'
#' Provides a standard 2-color palette for sex/gender comparisons (e.g., Male vs Female).
#' Uses colors from REACH brand palette that provide good contrast.
#'
#' @param reverse Logical indicating whether to reverse the color order. Default is FALSE.
#'   When FALSE, returns [Male color, Female color].
#'   When TRUE, returns [Female color, Male color].
#'
#' @return A character vector of 2 hex color codes: Male and Female.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get sex comparison colors (Male, Female)
#'   sex_colors()
#'
#'   # Get reversed order (Female, Male)
#'   sex_colors(reverse = TRUE)
#' }
sex_colors <- function(reverse = FALSE) {
  # Define sex comparison palette using REACH colors
  # Using darker shades for good contrast and readability
  colors <- c(
    "#58585A",  # Male - Dark gray (from REACH ramp 2)
    "#EE5859"   # Female - Red/Pink (from REACH ramp 1)
  )

  # Reverse if requested
  if (reverse) {
    colors <- rev(colors)
  }

  return(colors)
}
