

#' Title
#'
#' @param data
#' @param projections
#' @param SuperPopulation
#' @param Population
#' @param population_label
#'
#' @return
#' @export
#'
#' @examples
columnplot_reference <- function(
    data,
    projections = paste0("Q",1:7), # Names of projections in the data
    SuperPopulation = SuperPopulation, # SuperPopulation column
    Population = Population, # Population column
    population_label = "1KG Population",
    colors = NULL,
    # Tooltips:
    # header is a glue string from the data to use as the tooltip label
    tooltip_header = NULL,
    # include is a list of variables to include within the tooltip
    tooltip_include = NULL,
    tooltip_digits = getOption("digits")
) {

  projections <- broom.helpers::.select_to_varnames(projections, data = data)

  Population <- rlang::enquo(Population)
  tooltip_include <- broom.helpers::.select_to_varnames(
    select={{ tooltip_include }},
    data=data
  )
  # If no include options were given (for tooltips), construct
  # from the x, y parameters provided for the function.
  if ( is.null(tooltip_include) ) {
    tooltip_include <- broom.helpers::.select_to_varnames(
      c({{ projections }}),
      data = data
    )
  }

  # Calculate the max group (of the projections) and store the
  # max value for that group for sorting later.
  data <- data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      max_group_val = max(dplyr::pick(dplyr::all_of(projections))),
      max_group = projections[which.max(dplyr::pick(dplyr::all_of(projections)))]
    ) |>
    dplyr::ungroup()

  if ( rlang::quo_is_null(Population)) {
    Population <- rlang::sym("max_group")
  }

  # Summarize projections by Population and then create tooltips to add
  # for everyone.
  ttd <- data |>
    dplyr::group_by({{ Population }}) |>
    dplyr::summarize(
      dplyr::across(dplyr::all_of(projections), mean),
      dplyr::across(dplyr::everything(), \(.x) {head(.x,n=1)})
    )
  tooltips <- dplyr::bind_cols(
    {{ Population }} := dplyr::pull(ttd,{{ Population }}),
    tooltips = create_tooltips(
      data = ttd,
      header = tooltip_header,
      include = tooltip_include,
      digits = tooltip_digits
    )
  )


  data <- data |>
    dplyr::left_join(tooltips, by = dplyr::join_by({{ Population }}))

  # Now we arrange the data by Population, then by increasing value of the
  # max.
  data <- data |>
    dplyr::arrange({{ SuperPopulation }}, {{ Population }}) |>
    dplyr::group_by({{ SuperPopulation }}, {{ Population }}) |>
    dplyr::arrange(max_group_val, .by_group=TRUE) |>
    dplyr::ungroup()

  # Then we store the order and pivot to longer form for graphing
  data <- data |>
    dplyr::mutate(rn = dplyr::row_number()) |>
    tidyr::pivot_longer(cols=dplyr::all_of(projections),
                        names_to="Measure",values_to="Projection") |>
    # NB: Should we still do this? Need to check
    dplyr::mutate(Measure = factor(Measure, levels = rev(projections)))

  #plyr::mutate(max_rn = max(rn)) |>


  p <- ggplot2::ggplot(data, ggplot2::aes(x=rn, y = Projection, col=Measure, tooltip = tooltips)) +
    ggiraph::geom_col_interactive()
    #ggplot2::geom_col()

  if ( !is.null(colors) ) {
    p <- p +
      ggplot2::scale_color_manual(values = colors)

  }

  # There are two more things to plot: the lines and the labels.
  labels <- data |>
    dplyr::group_by({{ SuperPopulation }}, {{ Population }}) |>
    dplyr::mutate(
      group_min = min(rn),
      group_median = median(rn),
      group_max = max(rn)
    ) |>
    dplyr::distinct(dplyr::pick({{ SuperPopulation }}, {{ Population }}), .keep_all = TRUE) |>
    dplyr::ungroup()

  super_labels <- labels |>
    dplyr::group_by( {{ SuperPopulation }} )  |>
    dplyr::mutate(
      group_center = min(group_min) + ((max(group_max)-min(group_min))/2.0)
    ) |>
    dplyr::distinct(dplyr::pick({{ SuperPopulation }}), .keep_all = TRUE) |>
    dplyr::ungroup()

  # We create lines between each Population
  p <- p +
    ggplot2::geom_segment(
      data = labels,
      ggplot2::aes(x = group_max, y = 0, xend=group_max,yend=1),
      col="black"
    )

  # And then add coordinates.

    # Add coordinates (median rn), y=-10, text = max_group, color = from before.
    p <- p +
      ggplot2::geom_text(
        data=labels,
        ggplot2::aes(x=group_median,y=-0.015,label= {{ Population }}),
        col="black"
      ) +
    ggplot2::scale_x_continuous(
      breaks = super_labels$group_center,
      labels=dplyr::pull(super_labels,{{ SuperPopulation }}),
      expand = c(0,0)
    ) +
      ggplot2::ylab(population_label)
# NB: Could also use the tick marks and labels on axes instead (probably better, although colors won't work then)


    # Style now
    p <- p +
      theme_columnplot()

  p
}
