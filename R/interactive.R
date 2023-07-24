#' Add interactivity to ggplot
#'
#' This function will wrap the ggplot provided into an interactivity plot
#' via the [ggiraph::girafe()] function. This function will setup the
#' options specific for this package.
#'
#' @param x A [ggplot2::ggplot()] object.
#'
#' @return A [ggiraph::girafe()] object for visualizing.
#' @export
#'
#' @examples
plot_interactive <- function(x) {
  ggiraph::girafe(
    ggobj=x,
    options = list(ggiraph::opts_sizing(rescale = TRUE,width=0.85))
  ) |>
    ggiraph::girafe_options(
      ggiraph::opts_tooltip(
        use_fill = TRUE
      )
    )

}
