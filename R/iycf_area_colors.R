#' IYCF Area Graph Color Palette
#'
#' Provides a color palette specifically designed for IYCF (Infant and Young Child Feeding) area graphs.
#' This palette uses a gradient from light gray through yellows and oranges to dark brown, representing
#' the progression from unknown/not breastfed states to exclusive breastfeeding.
#'
#' The palette is optimized for visualizing the following IYCF feeding categories in order from
#' lightest to darkest:
#' - Unknown
#' - Not Breastfed
#' - Breastfed & Solid or Semi-Solid Foods
#' - Breastfed & Animal Milk or Formula
#' - Breastfed & Non-Milk Liquids
#' - Breastfed & Plain Water
#' - Exclusive Breastfed
#'
#' @param reverse Logical indicating whether to reverse the color order. Default is FALSE.
#'   When FALSE, returns colors from lightest (Unknown) to darkest (Exclusive Breastfed).
#'   When TRUE, returns colors from darkest to lightest.
#'
#' @return A named character vector of 7 hex color codes for the IYCF feeding categories.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get IYCF area graph colors (light to dark)
#'   iycf_area_colors()
#'
#'   # Get reversed order (dark to light)
#'   iycf_area_colors(reverse = TRUE)
#' }
iycf_area_colors <- function(reverse = FALSE) {
  # Define IYCF area graph palette
  # Colors progress from light gray (Unknown) through yellow/orange gradient to dark brown (Exclusive Breastfed)
  colors <- c(
    "Unknown" = "#CCCCCC",                                    # Light gray
    "Not Breastfed" = "#FFF7BC",                             # Very light yellow
    "Breastfed & Solid or Semi-Solid Foods" = "#FEE391",     # Light yellow
    "Breastfed & Animal Milk or Formula" = "#FEC44F",        # Medium yellow
    "Breastfed & Non-Milk Liquids" = "#FE9929",              # Orange-yellow
    "Breastfed & Plain Water" = "#EC7014",                   # Orange
    "Exclusive Breastfed" = "#8C2D04"                        # Dark brown (darkest)
  )

  # Reverse if requested
  if (reverse) {
    colors <- rev(colors)
  }

  return(colors)
}
