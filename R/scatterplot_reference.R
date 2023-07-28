#' Generate Scatter Plot of Reference Population
#'
#' @description This function will plot data from a data frame, under the assumption that the
#' data frame represents various pieces of information relevant for estimated
#' ancestry (or human diversity map) within a reference population.
#'
#' @details A variety of terms are important here:
#'
#' - Reference Population: There are several efforts to measure human variation
#'   by evaluating specific populations across the globe. Examples include the
#'   Thousand Genomes Project (1KG) or HGDP (Human Genome Diversity Project).
#'
#' - Projection: Genetic diversity is a genome (typically exome)-wide evaluation
#'   of human variation. However, to visualize this variability we need to project
#'   this variation into a relatively small number of coordinates (typically 2) that
#'   can be plotted.
#'
#' @note Most parameters default to NULL so that intelligent (ggplot) defaults can
#'  be used unless overridden.
#'
#' @param p A ggplot object (possibly NULL) to add reference samples. If NULL, a new
#'  plot will be created.
#' @param data A data frame to extract reference information from for plotting. Many
#'  of the subsequent parameters are columns from this data to use for various purposes.
#' @param x The x coordinate column of the projection
#' @param y The y coordinate column of the projection
#' @param label A column to use as labels for the reference population. Note that a
#'   reference population is typically clustered. This column should have repeated
#'   labels for every member of the cluster. A representative sample will be identified
#'   among rows having the same label value (currently random) and labelled on the graph
#'   using [ggrepel::geom_text_repel()].
#'
#'   If this value is NULL (default) when using the `color_by` parameter, then the
#'   `color_by` values will be printed as labels. This is not desirable in the case
#'   of a continuous value `color_by` option.
#' @param subset Subset of samples to plot using color scheme (all others will be gray)
#' @param tooltip_header A [glue::glue()] string to use as the header within a tooltip. Most
#'  typically used to refer to specific columns in `data` when creating the tooltip header.
#' @param tooltip_include A list of columns in `data` to include within the tooltip text.
#' @param tooltip_digits The number of digits when formatting numbers in the tooltips. Default
#'  is `geOption("digits")`.
#' @param alpha (default 0.2) The alpha level for reference samples. Too high a value and it
#'  becomes difficult to see the overlay samples.
#' @param color_by A column to use for varying colors among the populations. Typically this
#'  would be a Population or SuperPopulation option, however continuous variables can also
#'  be used.
#' @param color_map A named vector of colors to use when assigning colors to data points.
#'  This is tied to the `color_by` parameter and will assign colors to specific values pointed
#'  to by that parameter. When continuous, this parameter is used for the max value (the min
#'  value is set to gray currently).
#'
#' @return A [ggplot2::ggplot()] object representing the reference cohort projected onto
#'  the `x` and `y` values.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' scatterplot_reference(
#'   data = iris,
#'   x = Sepal.Length,
#'   y = Sepal.Width,
#'   color_by = Species,
#'   color_map = c("red","green","blue")
#' )
#' }
scatterplot_reference <- function(
    p=NULL,
    data,
    x = NULL,
    y = NULL,
    label = NULL,
    subset=NULL,

    # Tooltips:
    # header is a glue string from the data to use as the tooltip label
    tooltip_header = NULL,
    # include is a list of variables to include within the tooltip
    tooltip_include = NULL,
    tooltip_digits = getOption("digits"),

    alpha = 0.2,
    color_by=NULL,
    color_map = NULL
)
{

  # A note about many of the parameters. This function supports using symbols that
  # can be converted (non-standard evaluation) to data variables. In order to do this
  # but still examine the variables, we have to [rlang::enquo()] them. Once done, we
  # can examine the contents for plotting later.
  #
  # The `tooltip_include` option uses an approach from gtsummary that I found - it
  # does that hard work of getting to a character vector of variables which
  # we can then manipulate. It works when the contents are varnames anyway (not expressions).
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  color_by <- rlang::enquo(color_by)
  label <- rlang::enquo(label)
  tooltip_include <- broom.helpers::.select_to_varnames(
    select={{ tooltip_include }},
    data=data
  )

  # If there is no label column provided, use the value of color_by. Not
  # sure this is the best, there may be unseen consequences.
  if ( rlang::quo_is_null(label) ) {
    label <- {{ color_by }}
  }

  # If no include options were given (for tooltips), construct
  # from the x, y parameters provided for the function.
  if ( is.null(tooltip_include) ) {
    tooltip_include <- broom.helpers::.select_to_varnames(
      c({{ x }},{{ y }}),
      data = data
    )
  }

  # Create tooltip text from components
  tooltips <- create_tooltips(
    data,
    header = tooltip_header,
    include = tooltip_include,
    digits = tooltip_digits
  )

  # We build up our own structure for the plotting, pulling from the input.
  # This allows us to control what's in the structure as well as to cause
  # early errors if not all variables are present.
  gdf <- dplyr::select(
    data,
    {{ x }}, {{ y }},
    {{ color_by }}, {{ label }},
    dplyr::all_of(tooltip_include)
  ) |>
    dplyr::bind_cols(
      tooltips = tooltips
    )


  # Create an empty ggplot
  p <- ggplot2::ggplot(data.frame())


  # Finally, we have the contents needed to create the plot layer. Here, we
  # use ggiraph although you can use it as a static plot.
  p <- p +
    ggiraph::geom_point_interactive(
    #ggplot2::geom_point(
      data = gdf,
      mapping = ggplot2::aes(
        x={{ x }},y= {{ y }},
        tooltip = tooltips,
        fill = {{ color_by }},
        color = {{ color_by }}
      ),
      alpha = alpha,
      show.legend = FALSE
    )

  # If the `color_map` was set, we want custom colors (not the ggplot default).
  # Decide if color_by is continuous or categorical, then
  # do the appropriate scale based on the `color_map` parameter.
  if ( !is.null(color_map) ) {
    scale_type <- class(dplyr::pull(data, {{ color_by }}))
    if ( scale_type == "numeric" )
      p <- p + ggplot2::scale_color_gradient(low="grey", high = color_map)
    else if (scale_type == "character" || scale_type == "factor")
      p <- p + ggplot2::scale_color_manual(values = color_map)
  }

  # TODO: I am randomly picking a point to assign the label as coming from. I
  # should instead find the mediod of the dense region to use as exemplar.
  label_data <- data |>
    dplyr::group_by({{ label }}) |>
    dplyr::slice_sample(n=1) |>
    dplyr::ungroup()

  # Add a layer next that is the reference labels, using ggrepel.
  p <- p +
    ggiraph::geom_text_repel_interactive(
    #ggrepel::geom_text_repel(
      data = label_data,
      ggplot2::aes(
        x= {{ x }},y= {{ y }},
        label={{ label }},
        color = {{ color_by }}
      ),
      box.padding = 0.75
    )

  # Now apply the default theme for these plots.
  p <- p +
    theme_scatterplot()

  p
}




