
#' Title
#'
#' @param p
#' @param data
#' @param x
#' @param y
#' @param label
#' @param subset
#' @param color_by
#' @param tooltip_header
#' @param tooltip_include
#' @param tooltip_digits
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
scatterplot_overlay <- function(p, data, x = NULL, y = NULL,
                         label = NULL,
                         subset=NULL,
                         color_by=NULL,
                         # Tooltips:
                         # header is a glue string from the data to use as the tooltip label
                         tooltip_header = NULL,
                         # include is a list of variables to include within the tooltip
                         tooltip_include = NULL, # Tooltips
                         tooltip_digits = getOption("digits"),

                         alpha = 0.2
                         #scale_fill_discrete = mapping,
                         # names = "1KG - {p} - {sp} - {sampleid}" # Glue format
) {
  # Enquo these so we can use them later (converting to names for printing)
  # The include option is using an approach from gtsummary that I found - it
  # does that hard work of getting to a character vector of variables which
  # we can then manipulate.
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  color_by <- rlang::enquo(color_by)
  tooltip_include <- broom.helpers::.select_to_varnames(
    select={{ tooltip_include }},
    data=data
  )

    stopifnot(is(p, "gg"))

  # If no include options were given (for tooltips), construct
  # from the x, y parameters provided for the function.
  if ( is.null(tooltip_include) ) {
    tooltip_include <- broom.helpers::.select_to_varnames(
      c({{ x }},{{ y }}),
      data = data
    )
  }

  # Create tooltips if not already present
  tooltips <- create_tooltips(data, header = tooltip_header, include = tooltip_include,
                              digits = tooltip_digits)

  # We build up our own structure for the plotting, pulling from the input.
  # This allows us to control what's in the structure as well as to cause
  # early errors if not all variables are present.
  gdf <- dplyr::select(data, {{ x }}, {{ y }}, dplyr::all_of(tooltip_include)) |>
    dplyr::bind_cols(
      tooltips = tooltips
    )

  # Finally, we have the contents needed to create the plot layer. Here, we
  # use ggiraph although you can use it as a static plot.
  #
  # NB: Need to set the color scale. And if no color by, then use purple for
  # everything.

  # Either we have a color_by (which is a gradient, so other things to set)
  # or we have a default_color.

  default_color <- "#64166A"

  # Default arguments to geom_point_interactive
  plot_args <- list(
    data = data,
    mapping = ggplot2::aes(
      x={{ x }},y= {{ y }},
      tooltip = tooltips,
      fill = {{ color_by }},
      color = {{ color_by }}
    ),
    size = 0.5,
    show.legend = FALSE
  )
  # If there is no color_by, use the default color for points.
  if (rlang::quo_is_null(color_by) )
    plot_args <- c(plot_args, list(fill = default_color, color = default_color))

  p <- p +
    do.call(
      ggiraph::geom_point_interactive,
      plot_args
    )

  p
}

