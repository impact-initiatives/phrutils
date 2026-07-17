#' REACH Color Palette
#'
#' Provides access to the REACH brand color palettes including primary and secondary colors
#' organized into 4 distinct color ramps.
#'
#' @param ramp Integer from 1 to 4 specifying which color ramp to use.
#'   - Ramp 1: Red/Pink tones (primary)
#'   - Ramp 2: Gray tones (primary)
#'   - Ramp 3: Beige tones (secondary)
#'   - Ramp 4: Light gray tones (secondary)
#' @param n Number of colors to return. If NULL, returns all 5 colors from the ramp.
#'   If n is less than 5, interpolates to return exactly n colors.
#'   If n is greater than 5, interpolates to return exactly n colors.
#' @param reverse Logical indicating whether to reverse the color order. Default is FALSE.
#'
#' @return A character vector of hex color codes.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get all colors from ramp 1 (red/pink)
#'   reach_colors(ramp = 1)
#'
#'   # Get 3 colors from ramp 2 (gray)
#'   reach_colors(ramp = 2, n = 3)
#'
#'   # Get colors in reverse order
#'   reach_colors(ramp = 1, reverse = TRUE)
#' }
reach_colors <- function(ramp = 1, n = NULL, reverse = FALSE) {
  origin <- "reach_colors"

  # Define REACH color ramps
  reach_palettes <- list(
    # PRIMARY COLOURS
    # Color Ramp 1: Red/Pink tones
    ramp1 = c(
      "#EE5859",  # RGB(238,90,89)
      "#F1797A",  # RGB(243,121,122)
      "#F49A9B",  # RGB(243,155,156)
      "#F8BCBC",  # RGB(249,189,188)
      "#FBDDDD"   # RGB(252,222,220)
    ),
    # Color Ramp 2: Gray tones
    ramp2 = c(
      "#58585A",  # RGB(88,88,90)
      "#79797B",  # RGB(122,121,123)
      "#9A9A9C",  # RGB(154,153,154)
      "#BCBCBD",  # RGB(187,188,189)
      "#DDDDDE"   # RGB(221,221,222)
    ),
    # SECONDARY COLOURS
    # Color Ramp 3: Beige tones
    ramp3 = c(
      "#D2CBB8",  # RGB(209,202,184)
      "#DBD5C6",  # RGB(221,214,198)
      "#E4DFD4",  # RGB(227,222,211)
      "#EDEAE2",  # RGB(237,234,227)
      "#F6F4F0"   # RGB(247,245,240)
    ),
    # Color Ramp 4: Light gray tones
    ramp4 = c(
      "#C7C8CA",  # RGB(198,200,202)
      "#D2D3D4",  # RGB(209,211,213)
      "#DDDEDF",  # RGB(221,221,222)
      "#E8E9E9",  # RGB(231,233,232)
      "#F3F4F4"   # RGB(244,244,244)
    )
  )

  # Validate ramp parameter
  phr_assert(ramp %in% 1:4,
               "ramp must be an integer between 1 and 4",
               origin = origin)

  # Get selected ramp
  colors <- reach_palettes[[paste0("ramp", ramp)]]

  # Reverse if requested
  if (reverse) {
    colors <- rev(colors)
  }

  # If n is specified, interpolate to get exactly n colors
  if (!is.null(n)) {
    phr_assert(is.numeric(n) && n > 0,
                 "n must be a positive integer",
                 origin = origin)

    if (n == 1) {
      # Return middle color for single color request
      colors <- colors[3]
    } else if (n != length(colors)) {
      # Use colorRampPalette to interpolate
      colors <- grDevices::colorRampPalette(colors)(n)
    }
  }

  return(colors)
}
