

#' @title nc_format
#'
#' @description This function will tell you the format of a netcdf file
#' @param ff This is the file to check.
#' @export


nc_format <- function(ff) {

	result <- system(stringr::str_c("cdo showformat ", ff), intern = TRUE, ignore.stderr = TRUE)
	result


}



