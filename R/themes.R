#' Title
#'
#' @return
#' @export
#'
#' @examples
theme_scatterplot <- function() {


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

theme_columnplot <- function() {

  ggplot2::`%+replace%`(
    ggplot2::theme_minimal(),
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size=18)
    )
  )
}
