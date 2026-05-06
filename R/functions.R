#' Read in one nurses' stress data file
#'
#' @param file_path path to a data file
#' @param max_rows number of rows to read
#'
#' @returns outputs a dataframe / tibble
read <- function(file_path, max_rows = 10) {
  data <- file_path |>
    readr::read_csv( # tilføj readr:: for at tydeliggøre hvilken pakke funktionen kommer fra
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = max_rows
    )
  return(data)
}


#' Read all .csv.gz files of nurses stress folder
#'
#' @param filename Name of the file in the sub-folders that we want to read in
#'
#' @returns completed dataset (single tibble)
read_all <- function(filename) {
  files <- here::here("data-raw/nurses-stress/stress/") |> # root mappe
    fs::dir_ls(regexp = filename, recurse = TRUE) #alle filer med 'filename' i alle undermapper
  data <- files |>
    purrr::map(read) |> #brug read-funktionen på alle filer
    purrr::list_rbind(names_to = "file_path_id") #saml alle filer til en og tilføj en kolonne med filepathid
  return(data)
}

#' Extract ID from file path name
#'
#' @param dataset
#'
#' @returns dataset with id column
get_participant_id <- function(data) {
  data_with_id <- data |>
    dplyr::mutate(
      id = stringr::str_extract(
        file_path_id,
        pattern = "(?<=/stress/)[:alnum:]{2}(?=/)" # extracts the complete match
      ),
      .before = file_path_id
    ) |>
    dplyr::select(-file_path_id)
  return(data_with_id)
}

#' Round to minute and summarise by datetime
#'
#' @param data that you will convert and summarise
#'
#' @returns summarised dataset
summarise_by_datetime <- function(data) {
  summarised_data <- data |>
    dplyr::mutate(
      collection_datetime = lubridate::round_date(
        collection_datetime,
        unit = "minute")
    ) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::where(is.numeric),
        list(mean = mean, sd = sd, median = median)
      ),
      .by = c(id, collection_datetime)
    )
  return(summarised_data)
}
