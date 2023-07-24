
cli::cli_inform("Downloading population maps from the Thousand Genomes website.")
  download.file(
    url = "http://ftp.1000genomes.ebi.ac.uk/vol1/ftp/phase3/20131219.populations.tsv",
    destfile = here::here("data-raw/1kg-labels/populations.tsv")
  )
  download.file(
    url = "http://ftp.1000genomes.ebi.ac.uk/vol1/ftp/phase3/20131219.superpopulations.tsv",
    destfile = here::here("data-raw/1kg-labels/superpopulations.tsv")
  )

  map_1kg_population <- readr::read_tsv(
    here::here("data-raw/1kg-labels/populations.tsv"),
    col_types = "cccccdddd"
  ) |>
    dplyr::filter(!is.na(`Population Code`))

  map_1kg_superpopulation <- readr::read_tsv(
    here::here("data-raw/1kg-labels/superpopulations.tsv"),
    col_types = "cc"
  ) |>
    dplyr::rename(`Code` = `Population Code`)

  usethis::use_data(map_1kg_population, overwrite = TRUE, version = 3)
  usethis::use_data(map_1kg_superpopulation, overwrite = TRUE, version = 3)
