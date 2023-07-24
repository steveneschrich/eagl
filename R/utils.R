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
