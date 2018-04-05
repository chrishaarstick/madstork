
# Palette -----------------------------------------------------------------


# madstork colors
madstork_colors <- c(
  `orange`     = "#f2671d",
  `dark grey`  = "#8c8c8c",
  `dark blue`  = "#00009a",
  `light grey` = "#cccccc",
  `light blue` = "#1da8f2"
 )


#' Function to extract madstork colors as hex codes
#'
#' @param ... Character names of madstork_colors
madstork_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (madstork_colors)

  madstork_colors[cols]
}


madstork_palettes <- list(
  `main`  = madstork_cols("orange", "dark grey", "dark blue", "light grey"),
  `grey`  = madstork_cols("dark grey", "light grey")
)


#' Return function to interpolate a madstork color palette
#'
#' @param palette Character name of palette in madstork_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#' @export
madstork_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- madstork_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}


#' Color scale constructor for madstork colors
#'
#' @param palette Character name of palette in madstork_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
scale_color_madstork <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- madstork_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("madstork_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


#' Fill scale constructor for madstork colors
#'
#' @param palette Character name of palette in madstork_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
scale_fill_madstork <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- madstork_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("madstork_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
