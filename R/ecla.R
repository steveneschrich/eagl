#' Estimated Cell Line Ancestry Data
#' @name ecla
#' @description Data used for the Cancer Research paper on estimated cell line
#' ancestry.
"ecla"

# ecla
# Recreate the figures in the ECLA (http://ecla.moffitt.org) application

#' Title
#'
#' @return
#' @export
#'
#' @examples
ecla_tsne_plot <- function() {


  scatterplot_reference(
    data = eagl::ecla |> dplyr::filter(isReference),
    x = X1, y = X2,
    tooltip_header = "1KG - {SuperPopulation} - {Population} - {FID}",
    tooltip_include = c(`Population Description`, cluster.infomap, Q1, Q2, Q3, Q4, Q5, Q6, Q7),
    color_by = Population,
    color_map = eagl::color_maps$ecla_population
  ) |>

    scatterplot_overlay(data = eagl::ecla |> dplyr::filter(!isReference),
                 x = X1, y = X2,
                 tooltip_header = "Cell Line: {cLine_ID}",
                 tooltip_include = c(cLine_ID, Tissue,
                                     CCLE_membership, COSMIC_membership,
                                     cluster.infomap,Q1,Q2,Q3,Q4,Q5,Q6,Q7)
    )|>
    plot_interactive()


}

ecla_tsne_plot_q1 <- function() {
  scatterplot_reference(
    data = eagl::ecla |> dplyr::filter(isReference),
    x = X1, y = X2,
    tooltip_header = "1KG - {SuperPopulation} - {Population} - {FID}",
    tooltip_include = c(`Population Description`, cluster.infomap, Q1, Q2, Q3, Q4, Q5, Q6, Q7),
    color_by = Q1,
    label = Population,
    color_map = eagl::color_maps$ecla_q[1]
  ) |>

    scatterplot_overlay(data = eagl::ecla |> dplyr::filter(!isReference),
                 x = X1, y = X2,
                 color_by = Q1,
                 tooltip_header = "Cell Line: {cLine_ID}",
                 tooltip_include = c(cLine_ID, Tissue,
                                     CCLE_membership, COSMIC_membership,
                                     cluster.infomap,Q1,Q2,Q3,Q4,Q5,Q6,Q7)
    )|>
    plot_interactive()

}



plot_pong <- function() {

  # TODO: Figure out how to numerically arrange the Q's since they may go
  # to Q10/Q11/etc.
  mixture <- eagl::ecla |>
    dplyr::filter(isReference) |>
    dplyr::group_by(Population) |>
    dplyr::arrange(dplyr::desc(Q7)) |>
    dplyr::ungroup() |>
    dplyr::arrange(Population) |>
    dplyr::mutate(rn = dplyr::row_number()) |>
    tidyr::pivot_longer(cols=c(Q1,Q2,Q3,Q4,Q5,Q6,Q7), names_to="Measure",values_to="Projection")

# Based on ECLA (need to verify), the order of the bars is just Q1-Q7 (Q7 on top). We
# make the Q's a factor to get that order.

  mixture <- mixture |>
    dplyr::mutate(Measure=factor(Measure,levels=paste0("Q",7:1)))

# There are two different arranges that have to happen.
  ggplot2::ggplot(mixture, ggplot2::aes(x=rn, y = Projection, col=Measure)) +
    ggplot2::geom_col()
  #+
  #  ggplot2::scale_y_reverse() # Put the Q7 as last item, Q1 as first.
}


plot_pong_celllines <- function() {

  projections <- paste0("Q",1:7)
  # TODO: Figure out how to numerically arrange the Q's since they may go
  # to Q10/Q11/etc.
  mixture <- eagl::ecla |>
    dplyr::filter(!isReference) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      max_group_val = max(dplyr::pick(dplyr::all_of(projections))),
      max_group = projections[which.max(dplyr::pick(dplyr::all_of(projections)))]
    ) |>
    dplyr::ungroup()

  # Now we arrange the data by Population, then by increasing value of the
  # max.
  mixture <- mixture |>
    dplyr::arrange(dplyr::desc(max_group)) |>
    dplyr::group_by(max_group) |>
    dplyr::arrange(max_group_val, .by_group=TRUE) |>
    dplyr::ungroup()

  # Next, we reverse arrange by max group (Q7 down to Q1)
  # mixture <- mixture |>
  #   dplyr::arrange(dplyr::desc(max_group)) |>
  #   dplyr::group_by(max_group) |>
  #   dplyr::arrange(Q1, .by_group = TRUE)

  # Then, within each group, we arrange by increasing amounts of it, somehow
  mixture <- mixture |>
    dplyr::mutate(rn = dplyr::row_number()) |>
    tidyr::pivot_longer(cols=c(Q1,Q2,Q3,Q4,Q5,Q6,Q7), names_to="Measure",values_to="Projection")

  # Based on ECLA (need to verify), the order of the bars is just Q1-Q7 (Q7 on top). We
  # make the Q's a factor to get that order.

  mixture <- mixture |>
    dplyr::mutate(Measure=factor(Measure,levels=paste0("Q",7:1)))

  # There are two different arranges that have to happen.
  ggplot2::ggplot(mixture, ggplot2::aes(x=rn, y = Projection, col=Measure)) +
    ggplot2::geom_col() +
    # Add vlines at the max rn for the group
    ggplot2::geom_vline(xintercept=100) +
    # Add coordinates (median rn), y=-10, text = max_group, color = from before.
    ggplot2::geom_text(data=data.frame(x=c(100,200,300), text=c("A","B","C")), ggplot2::aes(x=x,y=0,label=text,col="green"))


  #+
  #  ggplot2::scale_y_reverse() # Put the Q7 as last item, Q1 as first.


  # So first, by grouping there is increasing Q7. Maybe this is a majority
  # call (max Q to arrange stuff), then sort that group by increasing value
}

plot_blank <- function() {
# Another question for the morning. I have the right ordering (at least for the paper).
# But what about having it by Pop or Superpop or both?
# This would look like group_by(Super,Pop) and then do the rest.
# ----------

mixture <- eagl::ecla |>
  dplyr::filter(isReference) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    max_group_val = max(dplyr::pick(dplyr::all_of(projections))),
    max_group = projections[which.max(dplyr::pick(dplyr::all_of(projections)))]
  ) |>
  dplyr::ungroup()

# Now we arrange the data by Population, then by increasing value of the
# max.
mixture <- mixture |>
  dplyr::arrange(SuperPopulation,Population) |>
  dplyr::group_by(SuperPopulation, Population) |>
  dplyr::arrange(max_group_val, .by_group=TRUE) |>
  dplyr::ungroup()

# Next, we reverse arrange by max group (Q7 down to Q1)
# mixture <- mixture |>
#   dplyr::arrange(dplyr::desc(max_group)) |>
#   dplyr::group_by(max_group) |>
#   dplyr::arrange(Q1, .by_group = TRUE)

# Then, within each group, we arrange by increasing amounts of it, somehow
mixture <- mixture |>
  dplyr::mutate(rn = dplyr::row_number()) |>
  tidyr::pivot_longer(cols=c(Q1,Q2,Q3,Q4,Q5,Q6,Q7), names_to="Measure",values_to="Projection")

# Based on ECLA (need to verify), the order of the bars is just Q1-Q7 (Q7 on top). We
# make the Q's a factor to get that order.

mixture <- mixture |>
  dplyr::mutate(Measure=factor(Measure,levels=paste0("Q",7:1)))

# There are two different arranges that have to happen.
ggplot2::ggplot(mixture, ggplot2::aes(x=rn, y = Projection, col=Measure)) +
  ggplot2::geom_col() +
  # Add vlines at the max rn for the group
  ggplot2::geom_vline(xintercept=100) +
  # Add coordinates (median rn), y=-10, text = max_group, color = from before.
  ggplot2::geom_text(
    data = data.frame(
      x = c(100, 200, 300),
      text = c("A", "B", "C")
    ),
    ggplot2::aes(x = x, y = 0, label = text, col = "green")
  )
}


# Reference barplot. I think that the other plot (non-labelled)
ecla_barplot_reference <- function() {
  columnplot_reference(
    eagl::ecla |> dplyr::filter(isReference),
    projections = paste0("Q", 1:7),
    SuperPopulation = SuperPopulation, # SuperPopulation column
    Population = Population, # Population column
    population_label = "1KG Population",
    colors = eagl::color_maps$ecla_q,
    tooltip_header = "{SuperPopulation}: {`SuperPopulation Description`}",
    tooltip_include = c(Population, `Population Description`, Q1, Q2, Q3, Q4, Q5, Q6, Q7)
  ) |>
    plot_interactive()

}




