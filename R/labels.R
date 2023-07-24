#' Title
#'
#' TODO: Incorporate labelled data for the names, instead of variable name
#'
#' @param data A data frame to evaluate tooltips in the context of.
#' @param header A glue string that will form the tooltip header.
#' @param include A list of column names to include in the tooltip. tidyselect syntax is
#'   supported.
#'
#' @return
#' @export
#'
#' @examples
create_tooltips <- function(data, header = NULL, include = NULL,
                            digits = getOption("digits")) {

  save_digits <- getOption("digits")
  options(digits = digits)

  # The header should be a glue string to evaluate in the context of
  # the data. Note that if this is NULL, the header_string is
  # set to NULL which allows the code below to not need special cases.
  if ( !is.null(header) ) {
    header_string <- list(glue::glue_data(data, header))
  } else {
    header_string <- NULL
  }

  # We have to enquo the include variable (in case it's symbols) and then
  # see if it's empty. If not empty, we select the data frame down to these
  # variables.
  include <- rlang::enquo(include)
  if ( ! rlang::quo_is_null(include) ) {
    vars <- broom.helpers::.select_to_varnames({{ include }}, data)
    data <- dplyr::select(data, dplyr::all_of(vars))
  }


  # For each variable in the reduced data frame, create an entry of the type
  # variable: value for each data point.
  labelled_variables <- purrr::imap(data, \(x, name) {
    if ( !is.null(labelled::label_attribute(x)) )
      name <- labelled::label_attribute(x)
    paste0(name,": ",format(x,scientific = FALSE))
  })

  # Add header information (if it exists) to the list of things
  # to combine.
  labelled_variables <- c(header_string, labelled_variables)

  # Collapse the labelled variables into a single string for the tooltip
  # to show.
  res <- labelled_variables |>
    purrr::pmap_chr(.f = \(...) {
      x <- list(...)
      paste0(x, collapse="\n")
    })

  options(digits=save_digits)

  res
}



#' Title
#'
#' @param data
#' @param name
#'
#' @return
#' @export
#'
#' @examples
create_sample_names <- function(data, name) {

  # Just use glue for the names
  glue::glue_data(data, name)
}




