#' Title
#'
#' @return
#' @export
#'
#' @examples
theme_scatter <- function() {


  ggplot2::`%+replace%`(
    ggplot2::theme_minimal(),
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.position = "none"
    )
  )
}

theme_column <- function() {

}
