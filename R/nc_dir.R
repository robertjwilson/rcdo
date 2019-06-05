

#' @title search netcdf files in folder and list variables in each file
#'
#' @description This function will list the netcdf files in a directory that contain a specified variable
#' @param directory This is the directory to check.
#' @param recursive Do you want to recursive look through a directory?
#' @param print Do you want to print the name of files as you check them?
#' @return data frame. First column is the file name. Remaining column are the variables, with TRUE signifying the variable is available in the file on that row.
#' @export

nc_dir <- function(directory = getwd(), recursive = TRUE, print = FALSE) {
  setwd(directory)
  on.exit(setwd(getwd()))


  all_files <- dir(directory, recursive = recursive) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::filter(endsWith(value, ".nc") | endsWith(value, ".nc4"))

  get_vars <- function(ff) {
    if (print) {
      print(stringr::str_glue("getting variables from {ff}"))
    }
    nc_variables(ff)
  }

  all_vars <- all_files %>%
    dplyr::mutate(variables = purrr::map(value, get_vars)) %>%
    tidyr::unnest() %>%
    dplyr::mutate(Exists = 1) %>%
    tidyr::spread(variables, Exists) %>%
    dplyr::rename(File = value)

  tidy_result <- function(x) {
    complete.cases(x)
  }

  all_vars %>%
    dplyr::mutate_at(2:ncol(.), tidy_result)
}
