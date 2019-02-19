

#' @title Find the format of a netcdf file.
#'
#' @description This function will tell you the format of a netcdf file
#' @param ff This is the file to check.
#' @export

#'@examples

#' # Get the format of the sample file
#' ff <- system.file("extdata", "woa18_decav_t01_01.nc", package = "rcdo")
#' nc_format(ff)

nc_format <- function(ff) {
  result <- system(stringr::str_c("cdo showformat ", ff), intern = TRUE, ignore.stderr = TRUE)
  result
}
