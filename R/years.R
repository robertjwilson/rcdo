


#' @title Find years available in a netcdf file
#'
#' @description Find years available in a netcdf file
#' @param ff This is the file to read years from.
#' @export

# need an option for cacheing results...

#' @examples
#'
#' # Get the years in the sample file
#' ff <- system.file("extdata", "woa18_decav_t01_01.nc", package = "rcdo")
#' nc_years(ff)
nc_years <- function(ff) {
  if (!file_valid(ff)) {
    stop(stringr::str_glue("error: {ff} does not exist or is not netcdf"))
  }

  ff_dates <- system(stringr::str_c("cdo showtimestamp ", ff), intern = TRUE, ignore.stderr = TRUE) %>%
    stringr::str_split(" ") %>%
    .[[1]]
  ff_years <- as.integer(str_sub(ff_dates[ff_dates != ""],1,4))

  dplyr::data_frame(Year = ff_years) %>%
    dplyr::distinct()
}

