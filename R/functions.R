#' Read in one nurses' stress data file
#'
#' @param file_path path to a data file
#' @param max_rows number of rows to read
#'
#' @returns outputs a dataframe / tibble
read <- function(file_path, max_rows = 100) {
  data <- file_path |>
    readr::read_csv( # tilføj readr:: for at tydeliggøre hvilken pakke funktionen kommer fra
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = max_rows
    )
  return(data)
}
