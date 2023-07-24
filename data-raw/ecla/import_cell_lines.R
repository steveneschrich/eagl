cell_lines <- readRDS(here::here("data-raw/ecla/cell_lines.rds"))

cell_lines <- cell_lines |>
  dplyr::rename(
    SuperPopulation = p,
    Population = sp
  ) |>
  dplyr::left_join(
    eagl::map_1kg_population |>
      dplyr::select(`Population Description`, `Population Code`),
    by = c("Population"="Population Code")
  ) |>
  dplyr::left_join(
    eagl::map_1kg_superpopulation |>
      dplyr::rename(`SuperPopulation Description` = Description),
    by = c("SuperPopulation"="Code")

  )

ecla <- cell_lines |>
  dplyr::mutate(isReference =  is.na(cLine_ID)) |>
  labelled::set_variable_labels(
    Q1 = "Q1 (European, South)",
    Q2 = "Q1 (Native American)",
    Q3 = "Q3 (East Asian, South)",
    Q4 = "Q4 (East Asian, North)",
    Q5 = "Q5 (South Asian)",
    Q6 = "Q6 (European, North)",
    Q7 = "Q7 (African)",
    `Population Description` = "Population"
  )

dataset <- tibble::tribble(
  ~id, ~name, ~description,
  1, "ecla", "Estimated Cell Line Ancestry"
)

usethis::use_data(ecla, overwrite = TRUE, version = 3)
usethis::use_data(dataset, overwrite=TRUE, version=3)
