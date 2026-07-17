#' Get Color Palette for Plotting
#'
#' A convenience function that returns an appropriate color palette based on the specified type.
#' Supports REACH brand colors, traffic light colors, sex comparison colors, group comparison colors,
#' IYCF area graph colors, IPC phase colors, WASH theme colors (water, sanitation, hygiene),
#' and ggplot2 default colors.
#'
#' @param type Character string specifying the palette type.
#'   Options: "reach1", "reach2", "reach3", "reach4", "traffic_light", "sex", "group", "iycf_area",
#'   "ipc", "water", "sanitation", "hygiene", "default".
#'   Default is "reach1".
#' @param n Number of colors to return. If NULL, returns all available colors for the palette.
#'   Note: For "sex" palette, n is ignored as it always returns 2 colors.
#'   Note: For "iycf_area" palette, n is ignored as it always returns 7 named colors.
#' @param reverse Logical indicating whether to reverse the color order. Default is FALSE.
#'
#' @return A character vector of hex color codes.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get REACH ramp 1 colors
#'   get_color_palette("reach1", n = 5)
#'
#'   # Get traffic light colors
#'   get_color_palette("traffic_light", n = 3)
#'
#'   # Get sex comparison colors
#'   get_color_palette("sex")
#'
#'   # Get group comparison colors
#'   get_color_palette("group", n = 8)
#'
#'   # Get IYCF area graph colors
#'   get_color_palette("iycf_area")
#'
#'   # Get IPC phase colors
#'   get_color_palette("ipc")
#'
#'   # Get Water theme colors (6 interpolated shades)
#'   get_color_palette("water", n = 6)
#'
#'   # Get Sanitation theme colors
#'   get_color_palette("sanitation")
#'
#'   # Get Hygiene theme colors
#'   get_color_palette("hygiene")
#'
#'   # Get default ggplot2 colors
#'   get_color_palette("default", n = 6)
#' }
get_color_palette <- function(type = "reach1", n = NULL, reverse = FALSE) {
  origin <- "get_color_palette"

  # Validate type
  valid_types <- c("reach1", "reach2", "reach3", "reach4", "traffic_light", "sex", "group", "iycf_area", "ipc", "water", "sanitation", "hygiene", "default")
  phr_assert(type %in% valid_types,
               "type must be one of: {paste(valid_types, collapse = ', ')}",
               origin = origin)

  # Return appropriate palette
  if (type == "traffic_light") {
    return(traffic_light_colors(n = n, reverse = reverse))
  } else if (type == "sex") {
    return(sex_colors(reverse = reverse))
  } else if (type == "group") {
    return(group_colors(n = n, reverse = reverse))
  } else if (type == "iycf_area") {
    return(iycf_area_colors(reverse = reverse))
  } else if (type == "ipc") {
    return(ipc_colors(n = n, reverse = reverse))
  } else if (type == "water") {
    return(water_colors(n = n, reverse = reverse))
  } else if (type == "sanitation") {
    return(sanitation_colors(n = n, reverse = reverse))
  } else if (type == "hygiene") {
    return(hygiene_colors(n = n, reverse = reverse))
  } else if (grepl("^reach[1-4]$", type)) {
    ramp_num <- as.integer(substr(type, 6, 6))
    return(reach_colors(ramp = ramp_num, n = n, reverse = reverse))
  } else if (type == "default") {
    # Return ggplot2 default color palette
    if (is.null(n)) n <- 6
    colors <- scales::hue_pal()(n)
    if (reverse) colors <- rev(colors)
    return(colors)
  }
}
