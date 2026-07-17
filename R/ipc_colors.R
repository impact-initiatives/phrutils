#' IPC Color Palette
#'
#' Provides the standard IPC (Integrated Food Security Phase Classification) color palette
#' representing the 5 food security phases ordered from worst (Phase 5 / Famine) to best
#' (Phase 1 / Minimal). This ordering ensures the first color maps to the worst outcome,
#' consistent with factor variables ordered worst-to-best.
#'
#' @param n Number of colors to return. If NULL, returns all 5 phase colors.
#'   If n is less than or equal to 5, returns the first n phase colors (worst n phases).
#'   If n is greater than 5, interpolates to generate exactly n colors.
#' @param reverse Logical indicating whether to reverse the color order. Default is FALSE.
#'   When FALSE, returns colors from Phase 5 (Famine/worst) to Phase 1 (Minimal/best).
#'
#' @return A character vector of hex color codes.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get all IPC phase colors (P5 to P1, worst to best)
#'   ipc_colors()
#'
#'   # Get colors for worst 4 phases (P5 to P2)
#'   ipc_colors(n = 4)
#'
#'   # Get colors in reverse order (P1 to P5, best to worst)
#'   ipc_colors(reverse = TRUE)
#' }
ipc_colors <- function(n = NULL, reverse = FALSE) {
  origin <- "ipc_colors"

  # Define IPC phase colors ordered worst to best (P5 Famine → P1 Minimal)
  colors <- c(
    "#8C0000",  # P5 Famine       RGB(140, 0, 0)
    "#FF0000",  # P4 Emergency    RGB(255, 0, 0)
    "#FF9900",  # P3 Crisis       RGB(255, 153, 0)
    "#FFEE00",  # P2 Stressed     RGB(255, 238, 0)
    "#A1FE8D"   # P1 Minimal      RGB(161, 254, 141)
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
      colors <- grDevices::colorRampPalette(colors)(n)
    }
  }

  return(colors)
}
