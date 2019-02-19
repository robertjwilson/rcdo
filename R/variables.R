
# to do:
# this really should be a data frame giving the units, full descriptions etc.


#' @title List the variables in a netcdf file.
#' @description This function will tell you the variables in a netcdf file
#' @param ff This is the file to read variables from.
#' @export
#' @examples

#' # Get the variable names in the NOAA WOA sample file
#' ff <- system.file("extdata", "woa18_decav_t01_01.nc", package = "rcdo")
#' nc_variables(ff)
#'

nc_variables <- function(ff) {

	if(!file_valid(ff))
		stop(stringr::str_glue("error: {ff} does not exist or is not netcdf"))

	ff_names <<- system(stringr::str_c("cdo showname ", ff), intern = TRUE, ignore.stderr = TRUE) %>%
		stringr::str_split(" ") %>%
		.[[1]]
	ff_names <- ff_names[ff_names != ""]
	ff_names <- as.character(ff_names)
	ff_names

}




