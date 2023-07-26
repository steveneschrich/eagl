#' Pull column corresponding to variable name
#'
#' @description Similar to [dplyr::pull()], but for formula-based
#' naming of variables (for plotly).
#'
#' @param .x
#' @param v
#'
#' @return
#' @export
#'
#' @examples
pull <- function(.x, v) {
  dplyr::pull(.x, dplyr::all_of(rlang::f_text(v)))
}


#' Color Maps for Plotting
#' @name color_maps
#' @description A list of color maps that can be used for plotting
#'
"color_maps"

#' Color Palettes for Plotting
#' @name color_palettes
#'
"color_palettes"

#' Map for 1KG Population Labels
#' @name map_1kg_population
"map_1kg_population"

#' Map for 1KG SuperPopulation Labels
#' @name map_1kg_superpopulation
"map_1kg_superpopulation"
