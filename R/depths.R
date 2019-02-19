

# to do: this potentially should be a list showing depth range and depths separately

#' @title Find depths in a netcdf file
#'
#' @description Find depths available in a netcdf file
#' @param ff This is the file to check.
#' @export


#'@examples

#' # Get the depths in the sample file
#' ff <- system.file("extdata", "woa18_decav_t01_01.nc", package = "rcdo")
#' nc_depths(ff)
#'

nc_depths <- function(ff) {


	if(!file_valid(ff))
		stop(stringr::str_glue("error: {ff} does not exist or is not netcdf"))

	ff_depths <<- system(stringr::str_c("cdo showlevel ", ff), intern = TRUE, ignore.stderr = TRUE) %>%
		stringr::str_split(" ") %>%
		.[[1]]
	ff_depths <- ff_depths[ff_depths != ""]
	ff_depths <- as.numeric(ff_depths)
	if(length(ff_depths) < 2)
		return("No depth information")


	dplyr::data_frame(Depth = ff_depths) %>%
		dplyr::distinct()

}



